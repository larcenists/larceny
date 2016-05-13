;;; Kernel shared by SRFI 114 and SRFI 128
;;; so their comparators will be interoperable and interchangeable.

(define-library (srfi 128 kernel)

  (export make-comparator
          comparator?
          comparator-type-test-procedure ; see comment below
          comparator-type-test-predicate
          comparator-equality-predicate
          comparator-ordering-predicate
          comparator-comparison-procedure
          comparator-hash-function
          comparator-ordered?
          comparator-comparison-procedure?
          comparator-hashable?
          comparator-hash-function?)     ; see comment below

  (import (scheme base))

  (include "kernel.body.scm")

  (begin

   ;; SRFI 114 uses different name for these than SRFI 128.

   (define (comparator-type-test-procedure c)
     (comparator-type-test-predicate c))

   (define (comparator-hash-function? c)
     (comparator-hashable? c))

   )

)
