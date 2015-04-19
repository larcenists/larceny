;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Prototype implementation of a character span library as specified at
;;; http://trac.sacrideo.us/wg/wiki/CharacterSpansCowan
;;;
;;; That document says
;;;
;;;     The sample implementation (which is not yet written) represents
;;;     spans as records containing a string and two string cursors, and
;;;     provides two implementations of string cursors, one using string
;;;     indexes directly and one that layers UTF-8 character spans on top
;;;     of single-byte native strings.
;;;
;;; This implementation does not fulfill that promise.  Instead, it has
;;; two implementations that represent spans and cursors as follows:
;;;
;;;     as strings and small exact integers (-1 and the valid indexes)
;;;     as bytevectors and small exact integers (-1 and valid indexes)
;;;
;;; In both implementations, the string or bytevector is hidden inside
;;; a record.
;;;
;;; Rationale:  There is no guarantee that an implementation's single-byte
;;; native strings are capable of representing every bit pattern needed by
;;; UTF-8, so the second of the promised sample implementations involves a
;;; reinvention of something like UTF-8 with one less bit.  Even that would
;;; assume the single-byte native strings can represent full Ascii, but
;;; some systems discriminate against the nul character.  Implementations
;;; that don't support bytevectors can use vectors instead and pay the
;;; price in space efficiency.

(define-library (in-progress spans)

  (export

   string-cursor-start
   span-cursor-start
   string-cursor-end
   span-cursor-end
   string-cursor-ref
   span-cursor-ref
   string-cursor-next
   span-cursor-next
   string-cursor-prev
   span-cursor-prev
   string-cursor-forward
   span-cursor-forward
   string-cursor-backward
   span-cursor-backward
;  string-cursor-forward-until      ; FIXME: dropped from proposal
;  span-cursor-forward-until        ; FIXME: dropped from proposal
;  string-cursor-backward-until     ; FIXME: dropped from proposal
;  span-cursor-backward-until       ; FIXME: dropped from proposal

   string-cursor=?
   span-cursor=?
   string-cursor<?
   span-cursor<?
   string-cursor>?
   span-cursor>?
   string-cursor<=?
   span-cursor<=?
   string-cursor>=?
   span-cursor>=?

   string-cursor->index
   span-cursor->index
   string-index->cursor
   span-index->cursor
   string-cursor-difference
   span-cursor-difference

   make-whole-span
   make-span
   span
   span-transform
   span-unfold
   span-unfold-right
   span-tabulate                ; deprecated because of argument order

   span?
   span-null?
   span-every?
   span-any?

   span-ref
   span-take
   span-take-right
   span-drop
   span-drop-right
   span-split-at
   span-replicate
   subspan
   subspan/cursors

   span-pad
   span-pad-right
   span-trim
   span-trim-right
   span-trim-both
   span-compress

   span-prefix
   span-suffix
   span-prefix-length
   span-suffix-length
   span-mismatch
   span-mismatch-right
   span-prefix?
   span-suffix?

   span-count
   span-find
   span-find-right
   span-skip
   span-skip-right
   span-take-while
   span-drop-while
   span-span
   span-break
   span-contains
   %span-contains:naive          ; FIXME: for internal benchmarking
   %span-contains:rabin-karp     ; FIXME: for internal benchmarking
   %span-contains:boyer-moore    ; FIXME: for internal benchmarking

   span-length
   span-reverse
   span-append
   span-concatenate
   span-concatenate-reverse

   span-map
   span-for-each
   span-fold
   span-fold-right

   span-split
   span-join

   span-filter
   span-remove
   span-partition

   string->span
   span->string
   span->list
   span->vector
   list->span
   vector->span
   reverse-list->span

   span-upcase
   span-downcase
   span-foldcase

   span=?
   span<?
   span>?
   span<=?
   span>=?
   span-ci=?
   span-ci<?
   span-ci>?
   span-ci<=?
   span-ci>=?

   span-comparator
   )

  (import (scheme base)
          (scheme char)
          (scheme write)   ; FIXME: shouldn't be here
          (srfi 69)        ; hashtables, for Boyer-Moore string search
          (srfi 114))      ; comparators, for span-comparator

  ;; A printable representation for spans aids debugging,
  ;; but is otherwise unnecessary.

  (cond-expand
   (larceny
    (import (larceny records printer)))
   (else))

  ;; A sane mod procedure is needed by span-replicate,
  ;; a matching div procedure is needed by the UTF-8 representation,
  ;; and the R6RS error procedure will be used if available.

  (cond-expand
   ((library (rnrs base))
    (import (only (rnrs base) div mod)
            (rename (only (rnrs base) error) (error %error))))
   (else
    (begin

     (define (div x y)
       (cond ((and (exact-integer? x)
                   (exact-integer? y)
                   (>= x 0))
              (quotient x y))
             ((< y 0)
              ; x < 0, y < 0
              (let* ((q (quotient x y))
                     (r (- x (* q y))))
                (if (= r 0)
                    q
                    (+ q 1))))
             (else
              ; x < 0, y > 0
              (let* ((q (quotient x y))
                     (r (- x (* q y))))
                (if (= r 0)
                    q
                    (- q 1))))))

     (define (mod x y)
       (cond ((and (exact-integer? x)
                   (exact-integer? y)
                   (>= x 0))
              (remainder x y))
             ((< y 0)
              ; x < 0, y < 0
              (let* ((q (quotient x y))
                     (r (- x (* q y))))
                (if (= r 0)
                    0
                    (- r y))))
             (else
              ; x < 0, y > 0
              (let* ((q (quotient x y))
                     (r (- x (* q y))))
                (if (= r 0)
                    0
                    (+ r y))))))

     (define (%error who msg . irritants)
       (cond ((symbol? who)
              (apply %error (symbol->string who) msg irritants))
             ((string? who)
              (apply error (string-append who ": " msg) irritants))
             (else
              (apply error msg irritants)))))))

  ;; The representation-dependent part of the implementation.

  (include "spans.body1.scm")

  ;; The representation-independent part of the implementation.

  (include "spans.body2.scm")

  )

; eof
