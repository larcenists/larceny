;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; John Cowan's reference implementation has been modified by
;;; making all three fields of the record into mutable fields.
;;; The mutators are not exported, so they can only be used by
;;; this file.
;;;
;;; The exported procedures have also been modified to detect
;;; major garbage collections.
;;;
;;; Finally, ephemerons whose key and datum are eq? have been
;;; special-cased.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define same-as-key (list 'same-as-key))

(define-record-type <ephemeron>
  (%make-ephemeron key datum broken?)
  ephemeron?
  (key %ephemeron-key %ephemeron-key!)
  (datum %ephemeron-datum %ephemeron-datum!)
  (broken? %ephemeron-broken? %ephemeron-broken!))

(define (make-ephemeron key datum)
  (if (> (major-gc-counter) next-breaking)
      (break-ephemera!))
  (if (eq? key datum)
      (%make-ephemeron key same-as-key #f)
      (%make-ephemeron key datum #f)))

(define (ephemeron-key x)
  (if (> (major-gc-counter) next-breaking)
      (break-ephemera!))
  (%ephemeron-key x))

(define (ephemeron-datum x)
  (if (> (major-gc-counter) next-breaking)
      (break-ephemera!))
  (let ((result (%ephemeron-datum x)))
    (if (eq? result same-as-key)
        (%ephemeron-key x)
        result)))

(define (ephemeron-broken? x)
  (if (> (major-gc-counter) next-breaking)
      (break-ephemera!))
  (%ephemeron-broken? x))

;;; The specification of reference-barrier in SRFI 124 makes no
;;; sense at all, and the only implementation given is this one.

(define (reference-barrier key) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of John Cowan's reference implementation, as modified
;;; for Larceny.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Breaking ephemerons is expensive in Larceny, on the order of
;;; several major garbage collections.  To reduce that overhead
;;; to a fraction of runtime at most 1/N, ephemerons are broken
;;; only after N major garbage collections have been performed.
;;; Furthermore the overhead is avoided altogether in programs
;;; that don't import (srfi 124).

(define gcs-between-breaking 10)

(define next-breaking
  (+ (major-gc-counter)
     gcs-between-breaking))

(define (break-ephemera!)

  (define (break-ephemeron! e)
    (%ephemeron-broken! e #t)
    (%ephemeron-key!    e #f)
    (%ephemeron-datum!  e #f))

  (let* ((v1 (sro 1 -1 1))
         (v3 (sro 3 -1 1))
         (v5 (sro 5 -1 1))
         (rl (sro 3  5 -1))
         (ht (make-eq-hashtable)))

    (define (walk-singly-referenced! x)
      (if (hashtable-contains? ht x)
          (break-ephemeron! (hashtable-ref ht x #f))))

    ;; ht will contain all ephemerons indexed by key

    (vector-for-each (lambda (x)
                       (if (ephemeron? x)
                           (hashtable-set! ht (%ephemeron-key x) x)))
                     ;; all record-like objects
                     rl)

    (vector-for-each walk-singly-referenced! v1)
    (vector-for-each walk-singly-referenced! v3)
    (vector-for-each walk-singly-referenced! v5))

  (set! next-breaking
        (+ (major-gc-counter)
           gcs-between-breaking)))

