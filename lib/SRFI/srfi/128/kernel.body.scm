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

;;; In Larceny, SRFI 114 and SRFI 128 comparators are interoperable
;;; and interchangeable.  That is accomplished by creating this
;;; kernel, shared by both.
;;;
;;; The basic problem is that SRFI 114 uses a comparison procedure
;;; that returns -1, 0, or +1, while SRFI 128 uses an irreflexive
;;; ordering predicate returning #t or #f.  Both require an equality
;;; predicate, however, so an ordering predicate can be synthesized
;;; from a comparison procedure and vice versa.  That synthesis is
;;; lazy because we can't tell whether the third argument passed to
;;; make-comparator is a comparison procedure or an ordering predicate
;;; until we can examine the value it returns for some call, and we
;;; don't know its precise domain of definition so we have to wait
;;; for it to be called during the normal execution of the program.

;;; Definition of comparator records with accessors and basic comparator

(define-record-type comparator
  (make-raw-comparator
   type-test equality ordering comparison hash ordering? comparison? hash?)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (comparison comparator-comparison-procedure)
  (hash comparator-hash-function)
  (ordering? comparator-ordered?)
  (comparison? comparator-comparison-procedure?)
  (hash? comparator-hashable?))

;; Public constructor
;;
;; If type-test or equality is #t, then it's SRFI 114 style.
;; If ordering/comparison is #f, then it doesn't matter.

(define (make-comparator type-test equality ordering/comparison hash)
  (cond ((eq? ordering/comparison #f)
         (make-raw-comparator
          (if (eq? type-test #t) universal-type-test type-test)
          equality
          no-ordering
          no-comparison
          (if hash hash no-hash)
          #f
          #f
          (if hash #t #f)))
        ((or (eq? type-test #t)
             (eq? equality #t))
         (make-raw-comparator
          (if (eq? type-test #t) universal-type-test type-test)
          (if (eq? equality #t)
              (lambda (x y) (eqv? (ordering/comparison x y) 0))
              equality)
          (if ordering/comparison
              (lambda (x y) (eqv? (ordering/comparison x y) -1))
              no-ordering)
          (if ordering/comparison
              ordering/comparison
              no-comparison)
          (if hash hash no-hash)
          (if ordering/comparison #t #f)
          (if ordering/comparison #t #f)
          (if hash #t #f)))
        (else
         (letrec ((srfi-114-style!
                   (lambda ()
                     (set! comparison ordering/comparison)
                     (set! ordering
                           (lambda (x y)
                             (eqv? (comparison x y) -1)))))
                  (srfi-128-style!
                   (lambda ()
                     (set! comparison
                           (lambda (x y)
                             (cond ((ordering x y) -1)
                                   ((equality x y) 0)
                                   (else +1))))
                     (set! ordering ordering/comparison)))
                  (comparison
                   (lambda (x y)
                     (let ((result (ordering/comparison x y)))
                       (if (boolean? result)
                           (begin (srfi-128-style!)
                                  (cond (result -1)
                                        ((equality x y) 0)
                                        (else +1)))
                           (begin (srfi-114-style!)
                                  result)))))
                  (ordering
                   (lambda (x y)
                     (let ((result (ordering/comparison x y)))
                       (if (boolean? result)
                           (begin (srfi-128-style!)
                                  result)
                           (begin (srfi-114-style!)
                                  (eqv? result -1)))))))
           (make-raw-comparator
            type-test
            equality
            (lambda (x y) (ordering x y))
            (lambda (x y) (comparison x y))
            (if hash hash no-hash)
            #t
            #t
            (if hash #t #f))))))

(define universal-type-test
  (lambda (x) #t))

(define no-ordering
  (lambda (x y) (error "ordering not supported")))

(define no-comparison
  (lambda (x y) (error "comparison not supported")))

(define no-hash
  (lambda (x y) (error "hashing not supported")))

; eof
