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

;;; This is a very shallow sanity test for character spans.

(import (scheme base)
        (scheme char)
        (srfi 114)
        (scheme write)
        (in-progress spans))

(define (writeln . xs)
  (for-each write xs)
  (newline))

(define (displayln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (displayln "Error: test failed: ")
  (writeln token)
  (if (not (null? more))
      (for-each writeln more))
  (newline)
  #f)

;;; FIXME

(define-syntax test
  (syntax-rules ()
   ((_ expr expected)
    (let ((actual expr))
      (or (equal? actual expected)
          (fail 'expr actual expected))))))

(define-syntax test-assert
  (syntax-rules ()
   ((_ expr)
    (or expr (fail 'expr)))))

(define-syntax test-deny
  (syntax-rules ()
   ((_ expr)
    (or (not expr) (fail 'expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These procedures have been removed from the character spans
;;; library.  The portable versions defined here saved me from
;;; rewriting some of the tests.

(define (string-cursor-forward-until str curs pred)
  (let ((n (string-length str)))
    (let loop ((i (string-cursor->index str curs)))
      (cond ((>= i n)
             (string-cursor-end str))
            ((and (<= 0 i)
                  (pred (string-ref str i)))
             (string-index->cursor str i))
            (else
             (loop (+ i 1)))))))

(define (span-cursor-forward-until sp curs pred)
  (let* ((start (span-cursor-start sp))
         (end   (span-cursor-end sp)))
    (let loop ((curs curs))
      (cond ((span-cursor>=? sp curs end)
             end)
            ((and (span-cursor<=? sp start curs)
                  (pred (span-cursor-ref sp curs)))
             curs)
            (else
             (loop (span-cursor-next sp curs)))))))

(define (string-cursor-backward-until str curs pred)
  (let ((n (string-length str)))
    (let loop ((i (string-cursor->index str curs)))
      (cond ((< i 0)
             (string-cursor-prev str (string-cursor-start str)))
            ((and (< i n)
                  (pred (string-ref str i)))
             (string-index->cursor str i))
            (else
             (loop (- i 1)))))))

(define (span-cursor-backward-until sp curs pred)
  (let* ((start (span-cursor-start sp))
         (end   (span-cursor-end sp)))
    (let loop ((curs curs))
      (cond ((span-cursor<? sp curs start)
             curs)
            ((and (span-cursor<? sp curs end)
                  (pred (span-cursor-ref sp curs)))
             curs)
            (else
             (loop (span-cursor-prev sp curs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; String and span cursors

(test (let* ((str "abc")
             (start0 (string-cursor-start str))
             (next0  (string-cursor-next str start0))
             (start1 (string-cursor-prev str next0))
             (next1  (string-cursor-next str start1)))
        (list (string-cursor-ref str start0)
              (string-cursor-ref str start1)
              (string-cursor-ref str next0)
              (string-cursor-ref str next1)
              (eqv? start0 next0)
              (eqv? start1 next1)))
      '(#\a #\a #\b #\b #f #f))

(test (let* ((span (make-whole-span "abc"))
             (start0 (span-cursor-start span))
             (next0  (span-cursor-next span start0))
             (start1 (span-cursor-prev span next0))
             (next1  (span-cursor-next span start1)))
        (list (span-cursor-ref span start0)
              (span-cursor-ref span start1)
              (span-cursor-ref span next0)
              (span-cursor-ref span next1)
              (eqv? start0 next0)
              (eqv? start1 next1)))
      '(#\a #\a #\b #\b #f #f))

(test (let* ((str "abc")
             (end0  (string-cursor-end str))
             (prev0 (string-cursor-prev str end0))
             (end1  (string-cursor-next str prev0))
             (prev1 (string-cursor-prev str end1)))
        (list (string-cursor-ref str prev0)
              (string-cursor-ref str prev1)
              (eqv? end0 prev0)
              (eqv? end1 prev1)))
      '(#\c #\c #f #f))

(test (let* ((span (make-whole-span "abc"))
             (end0  (span-cursor-end span))
             (prev0 (span-cursor-prev span end0))
             (end1  (span-cursor-next span prev0))
             (prev1 (span-cursor-prev span end1)))
        (list (span-cursor-ref span prev0)
              (span-cursor-ref span prev1)
              (eqv? end0 prev0)
              (eqv? end1 prev1)))
      '(#\c #\c #f #f))

(test (let* ((str "abcdefghijklm")
             (start0 (string-cursor-start str))
             (c5     (string-cursor-forward str start0 5))
             (c2     (string-cursor-backward str c5 3))
             (c13    (string-cursor-forward str c2 11))
             (c0     (string-cursor-backward str c13 13))
             (c7     (string-cursor-backward str c13 6)))
        (list (string-cursor-ref str start0)
              (string-cursor-ref str c0)
              (string-cursor-ref str c2)
              (string-cursor-ref str c5)
              (string-cursor-ref str c7)))
      '(#\a #\a #\c #\f #\h))

(test (let* ((span (make-whole-span "abcdefghijklm"))
             (start0 (span-cursor-start span))
             (c5     (span-cursor-forward span start0 5))
             (c2     (span-cursor-backward span c5 3))
             (c13    (span-cursor-forward span c2 11))
             (c0     (span-cursor-backward span c13 13))
             (c7     (span-cursor-backward span c13 6)))
        (list (span-cursor-ref span start0)
              (span-cursor-ref span c0)
              (span-cursor-ref span c2)
              (span-cursor-ref span c5)
              (span-cursor-ref span c7)))
      '(#\a #\a #\c #\f #\h))

;;; FIXME: string-cursor-forward-until and string-cursor-backward-until
;;; appear to have been removed from the proposal.

(test (let* ((str "abcdefghijklm")
             (vowel? (lambda (c) (memv c (string->list "aeiou"))))
             (start0 (string-cursor-start str))
             (c0     (string-cursor-forward-until str start0 vowel?))
             (c0a    (string-cursor-forward-until str c0 vowel?))
             (c1     (string-cursor-next str c0))
             (c4     (string-cursor-forward-until str c1 vowel?))
             (c4a    (string-cursor-forward-until str c4 vowel?))
             (c5     (string-cursor-next str c4))
             (c8     (string-cursor-forward-until str c5 vowel?))
             (c13    (string-cursor-end str))
             (c8b    (string-cursor-backward-until str c13 vowel?))
             (c7     (string-cursor-prev str c8b))
             (c4b    (string-cursor-backward-until str c7 vowel?))
             (c3     (string-cursor-prev str c4b))
             (c0b    (string-cursor-backward-until str c3 vowel?)))
        (list (string-cursor-ref str start0)
              (string-cursor-ref str c0)
              (string-cursor-ref str c0a)
              (string-cursor-ref str c0b)
              (string-cursor-ref str c1)
              (string-cursor-ref str c3)
              (string-cursor-ref str c4)
              (string-cursor-ref str c4a)
              (string-cursor-ref str c4b)
              (string-cursor-ref str c5)
              (string-cursor-ref str c7)
              (string-cursor-ref str c8)
              (string-cursor-ref str c8b)))
      '(#\a #\a #\a #\a #\b #\d #\e #\e #\e #\f #\h #\i #\i))

(test (let* ((span (make-whole-span "abcdefghijklm"))
             (vowel? (lambda (c)
                       (memv c (span->list (make-whole-span "aeiou")))))
             (start0 (span-cursor-start span))
             (c0     (span-cursor-forward-until span start0 vowel?))
             (c0a    (span-cursor-forward-until span c0 vowel?))
             (c1     (span-cursor-next span c0))
             (c4     (span-cursor-forward-until span c1 vowel?))
             (c4a    (span-cursor-forward-until span c4 vowel?))
             (c5     (span-cursor-next span c4))
             (c8     (span-cursor-forward-until span c5 vowel?))
             (c13    (span-cursor-end span))
             (c8b    (span-cursor-backward-until span c13 vowel?))
             (c7     (span-cursor-prev span c8b))
             (c4b    (span-cursor-backward-until span c7 vowel?))
             (c3     (span-cursor-prev span c4b))
             (c0b    (span-cursor-backward-until span c3 vowel?)))
        (list (span-cursor-ref span start0)
              (span-cursor-ref span c0)
              (span-cursor-ref span c0a)
              (span-cursor-ref span c0b)
              (span-cursor-ref span c1)
              (span-cursor-ref span c3)
              (span-cursor-ref span c4)
              (span-cursor-ref span c4a)
              (span-cursor-ref span c4b)
              (span-cursor-ref span c5)
              (span-cursor-ref span c7)
              (span-cursor-ref span c8)
              (span-cursor-ref span c8b)))
      '(#\a #\a #\a #\a #\b #\d #\e #\e #\e #\f #\h #\i #\i))

;;; FIXME:  "It is an error if the cursors do not refer to positions
;;; in string or span."  So the c13 case is an error, but it works
;;; okay in this sample implementation.  Think of it as a glass-box
;;; test.

(test (let* ((str "abcdefghijklm")
             (vowel? (lambda (c) (memv c (string->list "aeiou"))))
             (start0 (string-cursor-start str))
             (c0     (string-cursor-forward-until str start0 vowel?))
             (c0a    (string-cursor-forward-until str c0 vowel?))
             (c1     (string-cursor-next str c0))
             (c4     (string-cursor-forward-until str c1 vowel?))
             (c4a    (string-cursor-forward-until str c4 vowel?))
             (c5     (string-cursor-next str c4))
             (c8     (string-cursor-forward-until str c5 vowel?))
             (c13    (string-cursor-end str))
             (c8b    (string-cursor-backward-until str c13 vowel?))
             (c7     (string-cursor-prev str c8b))
             (c4b    (string-cursor-backward-until str c7 vowel?))
             (c3     (string-cursor-prev str c4b))
             (c0b    (string-cursor-backward-until str c3 vowel?))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c13)))
        (define (f pred)
          (map (lambda (c1)
                 (map (lambda (c2)
                        (pred str c1 c2))
                      cursors))
               cursors))
        (map f
             (list string-cursor=?
                   string-cursor<?
                   string-cursor>?
                   string-cursor<=?
                   string-cursor>=?)))

      '(;; string-cursor=?
        ((#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#f #f #f #t #f #f #f #f #f #f #f #f #f)   ; c1
         (#f #f #f #f #t #f #f #f #f #f #f #f #f)   ; c3
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4a
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4b
         (#f #f #f #f #f #f #f #f #t #f #f #f #f)   ; c5
         (#f #f #f #f #f #f #f #f #f #t #f #f #f)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #t #t #f)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #t #t #f)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #t))  ; c13

        ;; string-cursor<?
        ((#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0a
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0b
         (#f #f #f #f #t #t #t #t #t #t #t #t #t)   ; c1
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c3
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4a
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4b
         (#f #f #f #f #f #f #f #f #f #t #t #t #t)   ; c5
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #f #f #t)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #f #f #t)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #f))  ; c13

        ;; string-cursor>?
        ((#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c1
         (#t #t #t #t #f #f #f #f #f #f #f #f #f)   ; c3
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4a
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4b
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c5
         (#t #t #t #t #t #t #t #t #t #f #f #f #f)   ; c7
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c8
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c8b
         (#t #t #t #t #t #t #t #t #t #t #t #t #f))  ; c13

        ;; string-cursor<=?
        ((#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0a
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0b
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c1
         (#f #f #f #f #t #t #t #t #t #t #t #t #t)   ; c3
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4a
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4b
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c5
         (#f #f #f #f #f #f #f #f #f #t #t #t #t)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #t))  ; c13

        ;; string-cursor>=?
        ((#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#t #t #t #t #f #f #f #f #f #f #f #f #f)   ; c1
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c3
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4a
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4b
         (#t #t #t #t #t #t #t #t #t #f #f #f #f)   ; c5
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c7
         (#t #t #t #t #t #t #t #t #t #t #t #t #f)   ; c8
         (#t #t #t #t #t #t #t #t #t #t #t #t #f)   ; c8b
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)))); c13

(test (let* ((span (make-whole-span "abcdefghijklm"))
             (vowel? (lambda (c)
                       (memv c (span->list (make-whole-span "aeiou")))))
             (start0 (span-cursor-start span))
             (c0     (span-cursor-forward-until span start0 vowel?))
             (c0a    (span-cursor-forward-until span c0 vowel?))
             (c1     (span-cursor-next span c0))
             (c4     (span-cursor-forward-until span c1 vowel?))
             (c4a    (span-cursor-forward-until span c4 vowel?))
             (c5     (span-cursor-next span c4))
             (c8     (span-cursor-forward-until span c5 vowel?))
             (c13    (span-cursor-end span))
             (c8b    (span-cursor-backward-until span c13 vowel?))
             (c7     (span-cursor-prev span c8b))
             (c4b    (span-cursor-backward-until span c7 vowel?))
             (c3     (span-cursor-prev span c4b))
             (c0b    (span-cursor-backward-until span c3 vowel?))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c13)))
        (define (f pred)
          (map (lambda (c1)
                 (map (lambda (c2)
                        (pred span c1 c2))
                      cursors))
               cursors))
        (map f
             (list span-cursor=?
                   span-cursor<?
                   span-cursor>?
                   span-cursor<=?
                   span-cursor>=?)))

      '(;; span-cursor=?
        ((#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#f #f #f #t #f #f #f #f #f #f #f #f #f)   ; c1
         (#f #f #f #f #t #f #f #f #f #f #f #f #f)   ; c3
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4a
         (#f #f #f #f #f #t #t #t #f #f #f #f #f)   ; c4b
         (#f #f #f #f #f #f #f #f #t #f #f #f #f)   ; c5
         (#f #f #f #f #f #f #f #f #f #t #f #f #f)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #t #t #f)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #t #t #f)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #t))  ; c13

        ;; span-cursor<?
        ((#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0a
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c0b
         (#f #f #f #f #t #t #t #t #t #t #t #t #t)   ; c1
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c3
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4a
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c4b
         (#f #f #f #f #f #f #f #f #f #t #t #t #t)   ; c5
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #f #f #t)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #f #f #t)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #f))  ; c13

        ;; span-cursor>?
        ((#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#f #f #f #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c1
         (#t #t #t #t #f #f #f #f #f #f #f #f #f)   ; c3
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4a
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c4b
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c5
         (#t #t #t #t #t #t #t #t #t #f #f #f #f)   ; c7
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c8
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c8b
         (#t #t #t #t #t #t #t #t #t #t #t #t #f))  ; c13

        ;; span-cursor<=?
        ((#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0a
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)   ; c0b
         (#f #f #f #t #t #t #t #t #t #t #t #t #t)   ; c1
         (#f #f #f #f #t #t #t #t #t #t #t #t #t)   ; c3
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4a
         (#f #f #f #f #f #t #t #t #t #t #t #t #t)   ; c4b
         (#f #f #f #f #f #f #f #f #t #t #t #t #t)   ; c5
         (#f #f #f #f #f #f #f #f #f #t #t #t #t)   ; c7
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c8
         (#f #f #f #f #f #f #f #f #f #f #t #t #t)   ; c8b
         (#f #f #f #f #f #f #f #f #f #f #f #f #t))  ; c13

        ;; span-cursor>=?
        ((#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0a
         (#t #t #t #f #f #f #f #f #f #f #f #f #f)   ; c0b
         (#t #t #t #t #f #f #f #f #f #f #f #f #f)   ; c1
         (#t #t #t #t #t #f #f #f #f #f #f #f #f)   ; c3
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4a
         (#t #t #t #t #t #t #t #t #f #f #f #f #f)   ; c4b
         (#t #t #t #t #t #t #t #t #t #f #f #f #f)   ; c5
         (#t #t #t #t #t #t #t #t #t #t #f #f #f)   ; c7
         (#t #t #t #t #t #t #t #t #t #t #t #t #f)   ; c8
         (#t #t #t #t #t #t #t #t #t #t #t #t #f)   ; c8b
         (#t #t #t #t #t #t #t #t #t #t #t #t #t)))); c13

(test (let* ((str "abcdefghijklm")
             (vowel? (lambda (c) (memv c (string->list "aeiou"))))
             (start0 (string-cursor-start str))
             (c0     (string-cursor-forward-until str start0 vowel?))
             (c0a    (string-cursor-forward-until str c0 vowel?))
             (c1     (string-cursor-next str c0))
             (c4     (string-cursor-forward-until str c1 vowel?))
             (c4a    (string-cursor-forward-until str c4 vowel?))
             (c5     (string-cursor-next str c4))
             (c8     (string-cursor-forward-until str c5 vowel?))
             (c13    (string-cursor-end str))
             (c8b    (string-cursor-backward-until str c13 vowel?))
             (c7     (string-cursor-prev str c8b))
             (c4b    (string-cursor-backward-until str c7 vowel?))
             (c3     (string-cursor-prev str c4b))
             (c0b    (string-cursor-backward-until str c3 vowel?))
             (c12    (string-cursor-prev str c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12)))
        (map (lambda (cur) (string-cursor->index str cur))
             cursors))
      '(0 0 0 1 3 4 4 4 5 7 8 8 12))

(test (let* ((span (make-whole-span "abcdefghijklm"))
             (vowel? (lambda (c)
                       (memv c (span->list (make-whole-span "aeiou")))))
             (start0 (span-cursor-start span))
             (c0     (span-cursor-forward-until span start0 vowel?))
             (c0a    (span-cursor-forward-until span c0 vowel?))
             (c1     (span-cursor-next span c0))
             (c4     (span-cursor-forward-until span c1 vowel?))
             (c4a    (span-cursor-forward-until span c4 vowel?))
             (c5     (span-cursor-next span c4))
             (c8     (span-cursor-forward-until span c5 vowel?))
             (c13    (span-cursor-end span))
             (c8b    (span-cursor-backward-until span c13 vowel?))
             (c7     (span-cursor-prev span c8b))
             (c4b    (span-cursor-backward-until span c7 vowel?))
             (c3     (span-cursor-prev span c4b))
             (c0b    (span-cursor-backward-until span c3 vowel?))
             (c12    (span-cursor-prev span c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12)))
        (map (lambda (cur) (span-cursor->index span cur))
             cursors))
      '(0 0 0 1 3 4 4 4 5 7 8 8 12))

(test (let* ((str "abcdefghijklm")
             (vowel? (lambda (c) (memv c (string->list "aeiou"))))
             (start0 (string-cursor-start str))
             (c0     (string-cursor-forward-until str start0 vowel?))
             (c0a    (string-cursor-forward-until str c0 vowel?))
             (c1     (string-cursor-next str c0))
             (c4     (string-cursor-forward-until str c1 vowel?))
             (c4a    (string-cursor-forward-until str c4 vowel?))
             (c5     (string-cursor-next str c4))
             (c8     (string-cursor-forward-until str c5 vowel?))
             (c13    (string-cursor-end str))
             (c8b    (string-cursor-backward-until str c13 vowel?))
             (c7     (string-cursor-prev str c8b))
             (c4b    (string-cursor-backward-until str c7 vowel?))
             (c3     (string-cursor-prev str c4b))
             (c0b    (string-cursor-backward-until str c3 vowel?))
             (c12    (string-cursor-prev str c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12)))
        (map (lambda (cur) (string-cursor->index str cur))
             (map (lambda (i) (string-index->cursor str i))
                  (map (lambda (cur) (string-cursor->index str cur))
                       cursors))))
      '(0 0 0 1 3 4 4 4 5 7 8 8 12))

(test (let* ((span (make-whole-span "abcdefghijklm"))
             (vowel? (lambda (c)
                       (memv c (span->list (make-whole-span "aeiou")))))
             (start0 (span-cursor-start span))
             (c0     (span-cursor-forward-until span start0 vowel?))
             (c0a    (span-cursor-forward-until span c0 vowel?))
             (c1     (span-cursor-next span c0))
             (c4     (span-cursor-forward-until span c1 vowel?))
             (c4a    (span-cursor-forward-until span c4 vowel?))
             (c5     (span-cursor-next span c4))
             (c8     (span-cursor-forward-until span c5 vowel?))
             (c13    (span-cursor-end span))
             (c8b    (span-cursor-backward-until span c13 vowel?))
             (c7     (span-cursor-prev span c8b))
             (c4b    (span-cursor-backward-until span c7 vowel?))
             (c3     (span-cursor-prev span c4b))
             (c0b    (span-cursor-backward-until span c3 vowel?))
             (c12    (span-cursor-prev span c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12)))
        (map (lambda (cur) (span-cursor->index span cur))
             (map (lambda (i) (span-index->cursor span i))
                  (map (lambda (cur) (span-cursor->index span cur))
                       cursors))))
      '(0 0 0 1 3 4 4 4 5 7 8 8 12))

;;; FIXME: have to test this separately (see previous FIXME comment).

(test (let* ((str "abcdefghijklm"))
        (string-cursor=? str
                         (string-index->cursor str 13)
                         (string-cursor-end str)))
      #t)

(test (let* ((span (make-whole-span "abcdefghijklm")))
        (span-cursor=? span
                       (span-index->cursor span 13)
                       (span-cursor-end span)))
      #t)

(test (let* ((str "abcdefghijklm")
             (vowel? (lambda (c) (memv c (string->list "aeiou"))))
             (start0 (string-cursor-start str))
             (c0     (string-cursor-forward-until str start0 vowel?))
             (c0a    (string-cursor-forward-until str c0 vowel?))
             (c1     (string-cursor-next str c0))
             (c4     (string-cursor-forward-until str c1 vowel?))
             (c4a    (string-cursor-forward-until str c4 vowel?))
             (c5     (string-cursor-next str c4))
             (c8     (string-cursor-forward-until str c5 vowel?))
             (c13    (string-cursor-end str))
             (c8b    (string-cursor-backward-until str c13 vowel?))
             (c7     (string-cursor-prev str c8b))
             (c4b    (string-cursor-backward-until str c7 vowel?))
             (c3     (string-cursor-prev str c4b))
             (c0b    (string-cursor-backward-until str c3 vowel?))
             (c12    (string-cursor-prev str c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12 c13)))
        (map (lambda (cur1)
               (map (lambda (cur2)
                      (string-cursor-difference str cur1 cur2))
                    cursors))
             cursors))
      '((  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0
        (  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0a
        (  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0b
        ( -1  -1  -1   0   2  3  3  3  4  6  7  7 11 12)       ; c1
        ( -3  -3  -3  -2   0  1  1  1  2  4  5  5  9 10)       ; c3
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4a
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4b
        ( -5  -5  -5  -4  -2 -1 -1 -1  0  2  3  3  7  8)       ; c5
        ( -7  -7  -7  -6  -4 -3 -3 -3 -2  0  1  1  5  6)       ; c7
        ( -8  -8  -8  -7  -5 -4 -4 -4 -3 -1  0  0  4  5)       ; c8
        ( -8  -8  -8  -7  -5 -4 -4 -4 -3 -1  0  0  4  5)       ; c8b
        (-12 -12 -12 -11  -9 -8 -8 -8 -7 -5 -4 -4  0  1)       ; c12
        (-13 -13 -13 -12 -10 -9 -9 -9 -8 -6 -5 -5 -1  0)))     ; c13
        
(test (let* ((span (make-whole-span "abcdefghijklm"))
             (vowel? (lambda (c)
                       (memv c (span->list (make-whole-span "aeiou")))))
             (start0 (span-cursor-start span))
             (c0     (span-cursor-forward-until span start0 vowel?))
             (c0a    (span-cursor-forward-until span c0 vowel?))
             (c1     (span-cursor-next span c0))
             (c4     (span-cursor-forward-until span c1 vowel?))
             (c4a    (span-cursor-forward-until span c4 vowel?))
             (c5     (span-cursor-next span c4))
             (c8     (span-cursor-forward-until span c5 vowel?))
             (c13    (span-cursor-end span))
             (c8b    (span-cursor-backward-until span c13 vowel?))
             (c7     (span-cursor-prev span c8b))
             (c4b    (span-cursor-backward-until span c7 vowel?))
             (c3     (span-cursor-prev span c4b))
             (c0b    (span-cursor-backward-until span c3 vowel?))
             (c12    (span-cursor-prev span c13))
             (cursors (list c0 c0a c0b c1 c3 c4 c4a c4b c5 c7 c8 c8b c12 c13)))
        (map (lambda (cur1)
               (map (lambda (cur2)
                      (span-cursor-difference span cur1 cur2))
                    cursors))
             cursors))
      '((  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0
        (  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0a
        (  0   0   0   1   3  4  4  4  5  7  8  8 12 13)       ; c0b
        ( -1  -1  -1   0   2  3  3  3  4  6  7  7 11 12)       ; c1
        ( -3  -3  -3  -2   0  1  1  1  2  4  5  5  9 10)       ; c3
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4a
        ( -4  -4  -4  -3  -1  0  0  0  1  3  4  4  8  9)       ; c4b
        ( -5  -5  -5  -4  -2 -1 -1 -1  0  2  3  3  7  8)       ; c5
        ( -7  -7  -7  -6  -4 -3 -3 -3 -2  0  1  1  5  6)       ; c7
        ( -8  -8  -8  -7  -5 -4 -4 -4 -3 -1  0  0  4  5)       ; c8
        ( -8  -8  -8  -7  -5 -4 -4 -4 -3 -1  0  0  4  5)       ; c8b
        (-12 -12 -12 -11  -9 -8 -8 -8 -7 -5 -4 -4  0  1)       ; c12
        (-13 -13 -13 -12 -10 -9 -9 -9 -8 -6 -5 -5 -1  0)))     ; c13
        
;;; Span constructors.

(test (span-null? (make-whole-span "")) #t)
(test (span-null? (make-whole-span "a")) #f)

(test (span->string (make-whole-span ""))
      "")

(test (span->string (make-whole-span "abc"))
      "abc")

(test (span->string (make-span "abcdefghijklm" 0 13))
      "abcdefghijklm")

(test (span->string (make-span "abcdefghijklm" 0 0))
      "")

(test (span->string (make-span "abcdefghijklm" 0 1))
      "a")

(test (span->string (make-span "abcdefghijklm" 1 13))
      "bcdefghijklm")

(test (span->string (make-span "abcdefghijklm" 13 13))
      "")

(test (span->string (make-span "abcdefghijklm" 8 13))
      "ijklm")

(test (span->string (make-span "abcdefghijklm" 1 12))
      "bcdefghijkl")

(test (span->string (span)) "")
(test (span->string (span #\a)) "a")
(test (span->string (span #\a #\b #\c)) "abc")

(test (span->string (span-transform values (span))) "")
(test (span->string
       (span-transform (lambda (s m n)
                         (string-append s
                                        (number->string m)
                                        (number->string n)))
                       (span #\a #\b)
                       10
                       100))
      "ab10100")

(test (span->string (span-unfold (lambda (n) (>= n 110))
                                 integer->char
                                 (lambda (n) (+ n 2))
                                 100))
      "dfhjl")

(test (span->string (span-unfold-right (lambda (n) (>= n 110))
                                       integer->char
                                       (lambda (n) (+ n 2))
                                       100))
      "ljhfd")

;;; FIXME: deprecated because the order of arguments was a mistake

(test (span->string
       (span-tabulate 10
                      (lambda (n)
                        (integer->char (+ n (char->integer #\A))))))
      "ABCDEFGHIJ")

;;; Predicates.

(test (span? (span)) #t)
(test (span? (span #\a #\b)) #t)
(test (span? "") #f)
(test (span? "ab") #f)
(test (span? 'ab) #f)

(test (span-null? (span)) #t)
(test (span-null? (span #\a)) #f)
(test (span-null? (span #\a #\b #\c)) #f)

(test (span-every? char-upper-case? (string->span "")) #t)
(test (span-every? char-upper-case? (string->span "ABC")) #t)
(test (span-every? char-upper-case? (string->span "aBC")) #f)
(test (span-every? char-upper-case? (string->span "AbC")) #f)
(test (span-every? char-upper-case? (string->span "ABc")) #f)

(test (span-any? char-lower-case? (string->span "")) #f)
(test (span-any? char-lower-case? (string->span "ABC")) #f)
(test (span-any? char-lower-case? (string->span "aBC")) #t)
(test (span-any? char-lower-case? (string->span "AbC")) #t)
(test (span-any? char-lower-case? (string->span "ABc")) #t)

;;; Selection.

(test (let* ((span (make-whole-span "abc")))
        (map (lambda (i) (span-ref span i))
             '(0 1 2)))
      '(#\a #\b #\c))

(test (let* ((span (make-whole-span "abc")))
        (map span->string
             (map (lambda (n) (span-take span n))
                  '(0 1 2 3))))
      '("" "a" "ab" "abc"))

(test (let* ((span (make-whole-span "abc")))
        (map span->string
             (map (lambda (n) (span-take-right span n))
                  '(0 1 2 3))))
      '("" "c" "bc" "abc"))

(test (let* ((span (make-whole-span "abc")))
        (map span->string
             (map (lambda (n) (span-drop span n))
                  '(0 1 2 3))))
      '("abc" "bc" "c" ""))

(test (let* ((span (make-whole-span "abc")))
        (map span->string
             (map (lambda (n) (span-drop-right span n))
                  '(0 1 2 3))))
      '("abc" "ab" "a" ""))

(test (let* ((span (make-whole-span "abc")))
        (map (lambda (n)
               (call-with-values
                (lambda () (span-split-at span n))
                (lambda (sp1 sp2)
                  (list (span->string sp1)
                        (span->string sp2)))))
             '(0 1 2 3)))
      '(("" "abc")
        ("a" "bc")
        ("ab" "c")
        ("abc" "")))

(test (span->string (span-replicate (make-whole-span "abcdef") 2 7))
      "cdefa")

(test (span->string (span-replicate (make-whole-span "abcdef") -2 4))
      "efabcd")

(test (span->string (span-replicate (make-whole-span "abc") 0 7))
      "abcabca")

(test (span->string (span-replicate (make-whole-span "") 0 0))
      "")

(test (span->string (span-replicate (make-whole-span "abc") -10 5))
      "cabcabcabcabcab")

(test (span->string (subspan (string->span "") 0 0))
      "")

(test (span->string (subspan (string->span "abcdef") 0 0))
      "")

(test (span->string (subspan (string->span "abcdef") 6 6))
      "")

(test (span->string (subspan (string->span "abcdef") 0 1))
      "a")

(test (span->string (subspan (string->span "abcdef") 1 6))
      "bcdef")

(test (span->string (subspan (string->span "abcdef") 1 4))
      "bcd")


(test (let ((sp (string->span "abcdef")))
        (span->string (subspan/cursors sp
                                       (span-index->cursor sp 0)
                                       (span-index->cursor sp 0))))
      "")

(test (let ((sp (string->span "abcdef")))
        (span->string (subspan/cursors sp
                                       (span-index->cursor sp 6)
                                       (span-index->cursor sp 6))))
      "")

(test (let ((sp (string->span "abcdef")))
        (span->string (subspan/cursors sp
                                       (span-index->cursor sp 0)
                                       (span-index->cursor sp 1))))
      "a")

(test (let ((sp (string->span "abcdef")))
        (span->string (subspan/cursors sp
                                       (span-index->cursor sp 1)
                                       (span-index->cursor sp 6))))
      "bcdef")

(test (let ((sp (string->span "abcdef")))
        (span->string (subspan/cursors sp
                                       (span-index->cursor sp 1)
                                       (span-index->cursor sp 4))))
      "bcd")

;;; Padding, trimming, and compressing.

(test (span->string (span-pad (string->span "") 0))
      "")

(test (span->string (span-pad (string->span "") 0 #\a))
      "")

(test (span->string (span-pad (string->span "abcd") 0))
      "")

(test (span->string (span-pad (string->span "abcd") 0 #\a))
      "")

(test (span->string (span-pad (string->span "abcd") 3))
      "bcd")

(test (span->string (span-pad (string->span "abcd") 3 #\a))
      "bcd")

(test (span->string (span-pad (string->span "abcd") 4))
      "abcd")

(test (span->string (span-pad (string->span "abcd") 4 #\a))
      "abcd")

(test (span->string (span-pad (string->span "abcd") 6))
      "  abcd")

(test (span->string (span-pad (string->span "abcd") 6 #\a))
      "aaabcd")

(test (span->string (span-pad-right (string->span "") 0))
      "")

(test (span->string (span-pad-right (string->span "") 0 #\a))
      "")

(test (span->string (span-pad-right (string->span "abcd") 0))
      "")

(test (span->string (span-pad-right (string->span "abcd") 0 #\a))
      "")

(test (span->string (span-pad-right (string->span "abcd") 3))
      "abc")

(test (span->string (span-pad-right (string->span "abcd") 3 #\a))
      "abc")

(test (span->string (span-pad-right (string->span "abcd") 4))
      "abcd")

(test (span->string (span-pad-right (string->span "abcd") 4 #\a))
      "abcd")

(test (span->string (span-pad-right (string->span "abcd") 6))
      "abcd  ")

(test (span->string (span-pad-right (string->span "abcd") 6 #\a))
      "abcdaa")

;;; FIXME:  I'm guessing the trimming stops when the predicate returns false.

(test (span->string
       (span-trim (string->span "   abcdef   ")))
      "abcdef   ")

(test (span->string
       (span-trim (string->span "abcdef")))
      "abcdef")

(test (span->string
       (span-trim (string->span "abcdef")
                  char?))
      "")

(test (span->string
       (span-trim (string->span "abcdef")
                  (lambda (c) (char<? c #\d))))
      "def")

(test (span->string
       (span-trim (string->span "abcdef")
                  (lambda (c) (char>? c #\d))))
      "abcdef")

(test (span->string
       (span-trim-right (string->span "   abcdef   ")))
      "   abcdef")

(test (span->string
       (span-trim-right (string->span "abcdef")))
      "abcdef")

(test (span->string
       (span-trim-right (string->span "abcdef")
                        char?))
      "")

(test (span->string
       (span-trim-right (string->span "abcdef")
                        (lambda (c) (char<? c #\d))))
      "abcdef")

(test (span->string
       (span-trim-right (string->span "abcdef")
                        (lambda (c) (char>? c #\d))))
      "abcd")

(test (span->string
       (span-trim-both (string->span "   abcdef   ")))
      "abcdef")

(test (span->string
       (span-trim-both (string->span "abcdef")))
      "abcdef")

(test (span->string
       (span-trim-both (string->span "abcdef")
                       char?))
      "")

(test (span->string
       (span-trim-both (string->span "abcdef")
                       (lambda (c) (char<? c #\d))))
      "def")

(test (span->string
       (span-trim-both (string->span "abcdef")
                       (lambda (c) (char>? c #\d))))
      "abcd")

(test (span->string (span-compress (string->span "")))
      "")

(test (span->string (span-compress (string->span "") #\d))
      "")

(test (span->string (span-compress (string->span "abcdef")))
      "abcdef")

(test (span->string (span-compress (string->span "abcdef") #\d))
      "abcdef")

(test (span->string
       (span-compress (string->span "  aa  bb  cc  dd  ee fff")))
      " aa bb cc dd ee fff")

(test (span->string
       (span-compress (string->span "  aa  bb  cc  dd  ee fff") #\d))
      "  aa  bb  cc  d  ee fff")

;;; Prefixes and suffixes.

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span->string (span-prefix span1 span2)))
                    spans))
             spans))
      '(("" "" "" "" "" "" "")                         ; ""
        ("" "a" "a" "a" "a" "" "a")                    ; "a"
        ("" "a" "abc" "abc" "ab" "" "a")               ; "abc"
        ("" "a" "abc" "abcdef" "ab" "" "a")            ; "abcdef"
        ("" "a" "ab" "ab" "abracadabra" "" "a")        ; "abracadabra"
        ("" "" "" "" "" "magic" "")                    ; "magic"
        ("" "a" "a" "a" "a" "" "algebra")))            ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span->string (span-suffix span1 span2)))
                    spans))
             spans))
      '(("" "" "" "" "" "" "")                         ; ""
        ("" "a" "" "" "a" "" "a")                      ; "a"
        ("" "" "abc" "" "" "c" "")                     ; "abc"
        ("" "" "" "abcdef" "" "" "")                   ; "abcdef"
        ("" "a" "" "" "abracadabra" "" "bra")          ; "abracadabra"
        ("" "" "c" "" "" "magic" "")                   ; "magic"
        ("" "a" "" "" "bra" "" "algebra")))            ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-prefix-length span1 span2))
                    spans))
             spans))
      '((0 0 0 0 0 0 0)                                ; ""
        (0 1 1 1 1 0 1)                                ; "a"
        (0 1 3 3 2 0 1)                                ; "abc"
        (0 1 3 6 2 0 1)                                ; "abcdef"
        (0 1 2 2 11 0 1)                               ; "abracadabra"
        (0 0 0 0 0 5 0)                                ; "magic"
        (0 1 1 1 1 0 7)))                              ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-suffix-length span1 span2))
                    spans))
             spans))
      '((0 0 0 0 0 0 0)                                ; ""
        (0 1 0 0 1 0 1)                                ; "a"
        (0 0 3 0 0 1 0)                                ; "abc"
        (0 0 0 6 0 0 0)                                ; "abcdef"
        (0 1 0 0 11 0 3)                               ; "abracadabra"
        (0 0 1 0 0 5 0)                                ; "magic"
        (0 1 0 0 3 0 7)))                              ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-cursor->index span2 (span-mismatch span1 span2)))
                    spans))
             spans))
      '((-1 0 0 0 0 0 0)                                ; ""
        (-1 1 1 1 1 0 1)                                ; "a"
        (-1 1 3 3 2 0 1)                                ; "abc"
        (-1 1 3 6 2 0 1)                                ; "abcdef"
        (-1 1 2 2 11 0 1)                               ; "abracadabra"
        (-1 0 0 0 0 5 0)                                ; "magic"
        (-1 1 1 1 1 0 7)))                              ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-cursor->index span2
                                          (span-mismatch-right span1 span2)))
                    spans))
             spans))
      '((-1 0 2 5 10 4 6)                              ; ""
        (-1 -1 2 5 9 4 5)                              ; "a"
        (-1 0 -1 5 10 3 6)                             ; "abc"
        (-1 0 2 -1 10 4 6)                             ; "abcdef"
        (-1 -1 2 5 -1 4 3)                             ; "abracadabra"
        (-1 0 1 5 10 -1 6)                             ; "magic"
        (-1 -1 2 5 7 4 -1)))                           ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-prefix? span1 span2))
                    spans))
             spans))
      '((#t #t #t #t #t #t #t)                         ; ""
        (#f #t #t #t #t #f #t)                         ; "a"
        (#f #f #t #t #f #f #f)                         ; "abc"
        (#f #f #f #t #f #f #f)                         ; "abcdef"
        (#f #f #f #f #t #f #f)                         ; "abracadabra"
        (#f #f #f #f #f #t #f)                         ; "magic"
        (#f #f #f #f #f #f #t)))                       ; "algebra"

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span-suffix? span1 span2))
                    spans))
             spans))
      '((#t #t #t #t #t #t #t)                         ; ""
        (#f #t #f #f #t #f #t)                         ; "a"
        (#f #f #t #f #f #f #f)                         ; "abc"
        (#f #f #f #t #f #f #f)                         ; "abcdef"
        (#f #f #f #f #t #f #f)                         ; "abracadabra"
        (#f #f #f #f #f #t #f)                         ; "magic"
        (#f #f #f #f #f #f #t)))                       ; "algebra"

;;; Searching.

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "syzygy" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span-count vowel? span1))
             spans))
      '(0 1 1 2 5 0 3))

(test (let* ((inputs '("" "a" "abc" "bcdef" "abracadabra" "syzygy" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span-cursor->index span1 (span-find vowel? span1)))
             spans))
      '(0 0 0 3 0 6 0))

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "syzygy" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span-cursor->index span1 (span-find-right vowel? span1)))
             spans))
      '(-1 0 0 4 10 -1 6))

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "syzygy" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span-cursor->index span1 (span-skip vowel? span1)))
             spans))
      '(0 1 1 1 1 0 1))

(test (let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "syzygy" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span-cursor->index span1 (span-skip-right vowel? span1)))
             spans))
      '(-1 -1 2 5 9 5 5))

;;; FIXME: it's a bit odd that there's no span-take-while-right or
;;; span-drop-while-right

(test (let* ((inputs '("" "a" "eegloo" "abc" "cba" "eeek" "scree"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span->string (span-take-while vowel? span1)))
             spans))
      '("" "a" "ee" "a" "" "eee" ""))

(test (let* ((inputs '("" "a" "eegloo" "abc" "cba" "eeek" "scree"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (span->string (span-drop-while vowel? span1)))
             spans))
      '("" "" "gloo" "bc" "cba" "k" "scree"))

(test (let* ((inputs '("" "a" "eegloo" "abc" "cba" "eeek" "scree"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (call-with-values
                (lambda () (span-span vowel? span1))
                (lambda (sp1 sp2)
                  (list (span->string sp1)
                        (span->string sp2)))))
             spans))
      '(("" "")
        ("a" "")
        ("ee" "gloo")
        ("a" "bc")
        ("" "cba")
        ("eee" "k")
        ("" "scree")))

(test (let* ((inputs '("" "a" "eegloo" "abc" "cba" "eeek" "scree"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (call-with-values
                (lambda () (span-break vowel? span1))
                (lambda (sp1 sp2)
                  (list (span->string sp1)
                        (span->string sp2)))))
             spans))
      '(("" "")
        ("" "a")
        ("" "eegloo")
        ("" "abc")
        ("cb" "a")
        ("" "eeek")
        ("scr" "ee")))

(test (let* ((inputs '("" "a" "abc" "bra" "defabc" "abracadabra" "algebra"))
             (spans (map string->span inputs))
             (vowel? (lambda (c) (memv c (string->list "aeiou")))))
        (map (lambda (span1)
               (map (lambda (span2)
                      (let ((probe (span-contains span1 span2)))
                        (if probe
                            (span-cursor->index span1 probe)
                            probe)))
                    spans))
             spans))
      '((#f #f #f #f #f #f #f)     ; ""
        (#f  0 #f #f #f #f #f)     ; "a"
        (#f  0  0 #f #f #f #f)     ; "abc"
        (#f  2 #f  0 #f #f #f)     ; "bra"
        (#f  3  3 #f  0 #f #f)     ; "defabc"
        (#f  0 #f  1 #f  0 #f)     ; "abracadabra"
        (#f  0 #f  4 #f #f  0)))   ; "algebra"

;;; The whole character span or string.

(test (let* ((inputs '("" "a" "abc" "bra" "defabc" "abracadabra" "algebra"))
             (spans (map string->span inputs)))
        (map span-length spans))
      '(0 1 3 3 6 11 7))

(test (let* ((inputs '("" "a" "abc" "bra" "defabc" "abracadabra" "algebra"))
             (spans (map string->span inputs)))
        (map span->string (map span-reverse spans)))
      '("" "a" "cba" "arb" "cbafed" "arbadacarba" "arbegla"))

(test (let* ((inputs '("" "a" "abc" "bra" "defabc" "abracadabra" "algebra"))
             (spans (map string->span inputs)))
        (map span->string (map span-append spans)))
      '("" "a" "abc" "bra" "defabc" "abracadabra" "algebra"))

(test (let* ((inputs '("" "a" "abc" "defabc"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span->string
                       (span-append span1 span2)))
                    spans))
             spans))
      '(("" "a" "abc" "defabc")
        ("a" "aa" "aabc" "adefabc")
        ("abc" "abca" "abcabc" "abcdefabc")
        ("defabc" "defabca" "defabcabc" "defabcdefabc")))

(test (span->string
       (apply span-append (map string->span '("ab" "cd" "ef" "gh"))))
      "abcdefgh")

(test (span->string
       (span-concatenate (map string->span '("ab" "cd" "ef" "gh"))))
      "abcdefgh")

(test (span->string
       (span-concatenate-reverse (map string->span '("ab" "cd" "ef" "gh"))))
      "ghefcdab")

;;; Folding and mapping.

(test (let* ((inputs '("" "a" "abc" "defabc"))
             (spans (map string->span inputs)))
        (map span->string
             (map (lambda (sp)
                    (span-map (lambda (c)
                                (integer->char (+ 1 (char->integer c))))
                              sp))
                  spans)))
      '("" "b" "bcd" "efgbcd"))

(test (let* ((inputs '("" "a" "abc" "dbabc"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (span->string
                       (span-map (lambda (c1 c2)
                                   (if (char<? c1 c2)
                                       c2
                                       c1))
                                 span1
                                 span2)))
                    spans))
             spans))
      '(("" "" "" "")
        ("" "a" "a" "d")
        ("" "a" "abc" "dbc")
        ("" "d" "dbc" "dbabc")))

(test (span->string
       (span-map (lambda (c1 c2 c3 c4)
                   (if (char<? c1 c2)
                       c3
                       c4))
                 (string->span "johnny")
                 (string->span "wehardly")
                 (string->span "knewye")
                 (string->span "newyear")))
      "kewyya")

(test (let* ((inputs '("" "a" "abc" "defabc"))
             (spans (map string->span inputs)))
        (map (lambda (sp)
               (let ((out (open-output-string)))
                 (span-for-each (lambda (c)
                                  (write-char
                                   (integer->char (+ 1 (char->integer c)))
                                   out))
                                sp)
                 (call-with-port out get-output-string)))
             spans))
      '("" "b" "bcd" "efgbcd"))

(test (let* ((inputs '("" "a" "abc" "dbabc"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (map (lambda (span2)
                      (let ((out (open-output-string)))
                        (span-for-each (lambda (c1 c2)
                                         (write-char
                                          (if (char<? c1 c2)
                                              c2
                                              c1)
                                          out))
                                       span1
                                       span2)
                        (call-with-port out get-output-string)))
                    spans))
             spans))
      '(("" "" "" "")
        ("" "a" "a" "d")
        ("" "a" "abc" "dbc")
        ("" "d" "dbc" "dbabc")))

(test (let ((out (open-output-string)))
        (span-for-each (lambda (c1 c2 c3 c4)
                         (write-char
                          (if (char<? c1 c2)
                              c3
                              c4)
                          out))
                       (string->span "johnny")
                       (string->span "wehardly")
                       (string->span "knewye")
                       (string->span "newyear"))
        (call-with-port out get-output-string))
      "kewyya")

(test (span-fold cons '() (string->span "abc"))
      '(#\c #\b #\a))

(test (span-fold-right cons '() (string->span "abc"))
      '(#\a #\b #\c))

(test (span-fold list '() (string->span "abc") (string->span "defg"))
      '(#\c #\f (#\b #\e (#\a #\d ()))))

(test (span-fold-right list '(x) (string->span "abc") (string->span "de"))
      '(#\b #\d (#\c #\e (x))))

;;; Parsing and unparsing.

(test (map span->string
           (span-split (string->span "The quick red fox ran over.")))
      '("The" "quick" "red" "fox" "ran" "over."))

(test (map span->string
           (span-split (string->span "\r\t and \t over\nand over\n")))
      '("and" "over" "and" "over"))

(test (map span->string
           (span-split (string->span " --r7rs--annoy-user--path . ")
                       (string->span "--")))
      '(" " "r7rs" "annoy-user" "path . "))

(test (map span->string
           (span-split (string->span " --r7rs--annoy-user--path . ")
                       (string->span "--")
                       10))
      '(" " "r7rs" "annoy-user" "path . "))

(test (map span->string
           (span-split (string->span " --r7rs--annoy-user--path . ")
                       (string->span "--")
                       2))
      '(" " "r7rs" "annoy-user--path . "))

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")))
      "He loved Big Brother.")

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")
                  (string->span "...")))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")
                  (string->span "...")
                  'infix))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")
                  (string->span "...")
                  'strict-infix))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")
                  (string->span "...")
                  'suffix))
      "He...loved...Big...Brother....")

(test (span->string
       (span-join '("He" "loved" "Big" "Brother.")
                  (string->span "...")
                  'prefix))
      "...He...loved...Big...Brother.")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))))
      "He loved Big Brother.")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))
                  (string->span "...")))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))
                  (string->span "...")
                  'infix))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))
                  (string->span "...")
                  'strict-infix))
      "He...loved...Big...Brother.")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))
                  (string->span "...")
                  'suffix))
      "He...loved...Big...Brother....")

(test (span->string
       (span-join (map string->span '("He" "loved" "Big" "Brother."))
                  (string->span "...")
                  'prefix))
      "...He...loved...Big...Brother.")

(test (span->string
       (span-join (map string->span '())))
      "")

(test (span->string
       (span-join (map string->span '())
                  (string->span "***")))
      "")

(test (span->string
       (span-join (map string->span '())
                  (string->span "***")
                  'infix))
      "")

(test (span->string
       (span-join (map string->span '())
                  (string->span "***")
                  'prefix))
      "")

(test (span->string
       (span-join (map string->span '())
                  (string->span "***")
                  'suffix))
      "")

;;; Filtering and partitioning.

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (span->string (span-filter char-lower-case? span1)))
             spans))
      '("" "a" "" "bdf"))

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (span->string (span-remove char-lower-case? span1)))
             spans))
      '("" "" "ABC" "ACEG"))

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map (lambda (span1)
               (call-with-values
                (lambda ()
                  (span-partition char-lower-case? span1))
                (lambda (span1 span2)
                  (list (span->string span1)
                        (span->string span2)))))
             spans))
      '(("" "")
        ("a" "")
        ("" "ABC")
        ("bdf" "ACEG")))

;;; Conversion.

(test (span->list (string->span " abc"))
      '(#\space #\a #\b #\c))

(test (span->vector (string->span " abc"))
      '#(#\space #\a #\b #\c))

(test (span->string (list->span '(#\space #\a #\b #\c)))
      " abc")

(test (span->string (vector->span '#(#\space #\a #\b #\c)))
      " abc")

(test (span->string (reverse-list->span '(#\space #\a #\b #\c)))
      "cba ")

;;; Case.

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map span->string
             (map span-upcase spans)))
      '("" "A" "ABC" "ABCDEFG"))

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map span->string
             (map span-downcase spans)))
      '("" "a" "abc" "abcdefg"))

(test (let* ((inputs '("" "a" "ABC" "AbCdEfG"))
             (spans (map string->span inputs)))
        (map span->string
             (map span-foldcase spans)))
      '("" "a" "abc" "abcdefg"))

;;; Comparison.

;;; FIXME: "In any implementation of this proposal based on R7RS,
;;; the results of the span procedures must behave as if the arguments
;;; were converted to strings and then passed to the corresponding
;;; string procedures."  The following tests assume that holds.

(let* ((inputs '("" "a" "abc" "abcdef" "abracadabra" "magic" "algebra"))
       (spans (map string->span inputs)))
  (define (mm pred list1 list2)
    (map (lambda (x)
           (map (lambda (y)
                  (pred x y))
                list2))
         list1))
  (test (mm span=? spans spans)
        (mm string=? inputs inputs))
  (test (mm span<? spans spans)
        (mm string<? inputs inputs))
  (test (mm span>? spans spans)
        (mm string>? inputs inputs))
  (test (mm span<=? spans spans)
        (mm string<=? inputs inputs))
  (test (mm span>=? spans spans)
        (mm string>=? inputs inputs)))

(let* ((inputs1 '("" "a" "aBc" "abCdef" "abRacadAbra" "magIc" "alGebra"))
       (spans1 (map string->span inputs1))
       (inputs2 '("" "A" "AbC" "ABcDEF" "ABrACADaBRA" "MAGiC" "ALgEBRA"))
       (spans2 (map string->span inputs2)))
  (define (mm pred list1 list2)
    (map (lambda (x)
           (map (lambda (y)
                  (pred x y))
                list2))
         list1))
  (test (mm span-ci=? spans1 spans2)
        (mm string-ci=? inputs1 inputs2))
  (test (mm span-ci<? spans1 spans2)
        (mm string-ci<? inputs1 inputs2))
  (test (mm span-ci>? spans1 spans2)
        (mm string-ci>? inputs1 inputs2))
  (test (mm span-ci<=? spans1 spans2)
        (mm string-ci<=? inputs1 inputs2))
  (test (mm span-ci>=? spans1 spans2)
        (mm string-ci>=? inputs1 inputs2)))

;;; Comparator.

(let* ((inputs1 '("" "a" "aBc" "abCdef" "abRacadAbra" "magIc" "alGebra"))
       (spans1 (map string->span inputs1))
       (inputs2 '("" "A" "AbC" "ABcDEF" "ABrACADaBRA" "MAGiC" "ALgEBRA"))
       (spans2 (map string->span inputs2))
       (numbers (map string-length inputs1)))
  (define (mm pred list1 list2)
    (map (lambda (x)
           (map (lambda (y)
                  (pred x y))
                list2))
         list1))
  (test (map (lambda (x)
               (comparator-test-type span-comparator x))
             numbers)
        (map (lambda (x)
               (comparator-test-type string-comparator x))
             numbers))
  (test (map (lambda (x)
               (comparator-test-type span-comparator x))
             spans1)
        (map (lambda (x)
               (comparator-test-type string-comparator x))
             inputs1))
  (test (mm (lambda (x y)
              (comparator-equal? span-comparator x y))
            spans1
            spans1)
        (mm (lambda (x y)
              (comparator-equal? string-comparator x y))
            inputs1
            inputs1))
  (test (map (lambda (x)
               (comparator-hash span-comparator x))
             spans1)
        (map (lambda (x)
               (comparator-hash string-comparator x))
             inputs1)))

(displayln "Done.")

; eof
