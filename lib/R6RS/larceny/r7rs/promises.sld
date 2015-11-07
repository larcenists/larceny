;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of (scheme lazy).
;;;
;;; Imported by (rnrs r5rs) as well as (scheme lazy).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (larceny r7rs promises)

  (export

   ;; R7RS names

   delay delay-force force make-promise promise?

   ;; SRFI 45 names

   lazy eager)

  (import (rnrs base)
          (rnrs mutable-pairs)  ; FIXME
          (srfi :9 records)
          )

  (begin

   (define-record-type :promise make-promise0 promise?
     (contents promise-contents promise-contents!))

   (define (make-promise x)
     (eager x))

   (define (box x) (make-promise0 x))
   (define (unbox promise) (promise-contents promise))
   (define (set-box! promise x) (promise-contents! promise x)))

  (include "promises.body.scm")

  (begin

   (define-syntax delay-force
     (syntax-rules ()
      ((_ exp)
       (lazy exp))))))



