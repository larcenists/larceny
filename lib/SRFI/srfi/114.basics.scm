;; SRFI 114: Comparators
;; Copyright (C) John Cowan 2013. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; Main part of the SRFI 114 reference implementation

;;; Not provided are the definition of the default comparator (which is
;;; inherently system-dependent) or the numerical routines (which depend
;;; on how much of the numeric tower is provided).

;;; "There are two ways of constructing a software design: One way is to
;;; make it so simple that there are obviously no deficiencies, and the
;;; other way is to make it so complicated that there are no *obvious*
;;; deficiencies." --Tony Hoare

;;; Comparison syntax (because syntax must be defined before it is used)

;; Undefined value
(define undef (if #f #f))

;; Arithmetic if
(define-syntax if3
  (syntax-rules ()
    ((if3 p less equal greater)
     (case p
       ((-1) less)
       ((0) equal)
       ((1) greater)
       (else (error "if3: bad comparison value" p))))))

;; If equal
(define-syntax if=?
  (syntax-rules ()
    ((if=? p equal unequal)
     (if3 p unequal equal unequal))
    ((if=? p equal)
     (if=? p equal undef))))

;; If less than
(define-syntax if<?
  (syntax-rules ()
    ((if<? p less notless)
     (if3 p less notless notless))
    ((if<? p less)
     (if<? p less undef))))

;; If greater than
(define-syntax if>?
  (syntax-rules ()
    ((if>? p greater notgreater)
     (if3 p notgreater notgreater greater))
    ((if>? p greater)
     (if>? p greater undef))))

;; If not equal
(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? p notequal equal)
     (if3 p notequal equal notequal))
    ((if-not=? p notequal)
     (if-not=? p notequal undef))))

;; If less than or equal
(define-syntax if<=?
  (syntax-rules ()
    ((if<=? p lessequal greater)
     (if3 p lessequal lessequal greater))
    ((if<=? p lessequal)
     (if>? p lessequal undef))))

;; If greater than or equal
(define-syntax if>=?
  (syntax-rules ()
    ((if>=? p greaterequal less)
     (if3 p less greaterequal greaterequal))
    ((if>=? p greaterequal)
     (if>=? p greaterequal undef))))

;;; Definition of comparator records with accessors and basic comparator
;;;
;;; These next two definitions are commented out because they've been
;;; replaced by (srfi 128 kernel), which allows the comparators of
;;; SRFI 114 and SRFI 128 to be interoperable and interchangeable.

#;
(define-record-type comparator
  (make-raw-comparator type-test equality comparison hash comparison? hash?)
  comparator?
  (type-test comparator-type-test-procedure)
  (equality comparator-equality-predicate)
  (comparison comparator-comparison-procedure)
  (hash comparator-hash-function)
  (comparison? comparator-comparison-procedure?)
  (hash? comparator-hash-function?))

#;
(define (make-comparator type-test equality comparison hash)
  (make-raw-comparator
    (if (eq? type-test #t) (lambda (x) #t) type-test)
    (if (eq? equality #t) (lambda (x y) (eqv? (comparison x y) 0)) equality)
    (if comparison comparison (lambda (x y) (error "comparison not supported")))
    (if hash hash (lambda (x y) (error "hashing not supported")))
    (if comparison #t #f)
    (if hash #t #f)))

;;; A dozen procedure definitions have been commented out
;;; because they are now imported from (srfi 128).
;;; These comments are marked by "; now imported from (srfi 128)"

;; Primitive applicators

;; Invoke the test type
#; ; now imported from (srfi 128)
(define (comparator-test-type comparator obj)
  ((comparator-type-test-procedure comparator) obj))

;; Invoke the test type and throw an error if it fails
#; ; now imported from (srfi 128)
(define (comparator-check-type comparator obj)
  (if (comparator-test-type comparator obj)
    #t
    (error "comparator type check failed" comparator obj)))

;; Invoke the equality predicate
(define (comparator-equal? comparator obj1 obj2)
  ((comparator-equality-predicate comparator) obj1 obj2))

;; Invoke the comparison procedure
(define (comparator-compare comparator obj1 obj2)
  ((comparator-comparison-procedure comparator) obj1 obj2))

;; Invoke the hash function
#; ; now imported from (srfi 128)
(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

;;; Comparison procedure comparators
;;; These construct comparison procedures based on comparison predicates.

(define (make-comparison< <)
  (lambda (a b)
    (cond
      ((< a b) -1)
      ((< b a) 1)
      (else 0))))

(define (make-comparison> >)
  (lambda (a b)
    (cond
      ((> a b) 1)
      ((> b a) -1)
      (else 0))))

(define (make-comparison<= <=)
  (lambda (a b)
    (if (<= a b) (if (<= b a) 0 -1) 1)))

(define (make-comparison>= >=)
  (lambda (a b)
    (if (>= a b) (if (>= b a) 0 1) -1)))

(define (make-comparison=/< = <)
  (lambda (a b)
    (cond
      ((= a b) 0)
      ((< a b) -1)
      (else 1))))

(define (make-comparison=/> = >)
  (lambda (a b)
    (cond
      ((= a b) 0)
      ((> a b) 1)
      (else -1))))

;;; Dummy definition of default-comparator for testing

#;(define default-comparator
  (make-comparator
    number?
    #t
    (make-comparison=/< = <)
    (lambda (a) (abs a))))

;;; Comparison predicate constructors

(define (make= comparator)
  (lambda args (apply =? comparator args)))

(define (make< comparator)
  (lambda args (apply <? comparator args)))

(define (make> comparator)
  (lambda args (apply >? comparator args)))

(define (make<= comparator)
  (lambda args (apply <=? comparator args)))

(define (make>= comparator)
  (lambda args (apply >=? comparator args)))

;;; Interval (ternary) comparison predicates

(define in-open-interval?
  (case-lambda
    ((comparator a b c)
      (and (<? comparator a b) (<? comparator b c)))
    ((a b c)
      (in-open-interval? default-comparator a b c))))

(define in-closed-interval?
  (case-lambda
    ((comparator a b c)
      (and (<=? comparator a b) (<=? comparator b c)))
    ((a b c)
      (in-closed-interval? default-comparator a b c))))

(define in-open-closed-interval?
  (case-lambda
    ((comparator a b c)
      (and (<? comparator a b) (<=? comparator b c)))
    ((a b c)
      (in-open-interval? default-comparator a b c))))

(define in-closed-open-interval?
  (case-lambda
    ((comparator a b c)
      (and (<=? comparator a b) (<? comparator b c)))
    ((a b c)
      (in-open-interval? default-comparator a b c))))

;;; Comparison predicates

#; ; now imported from (srfi 128)
(define (=? comparator a b . objs)
  (if (comparator-equal? comparator a b)
    (if (null? objs) #t (apply =? comparator b objs))
    #f))

#; ; now imported from (srfi 128)
(define (<? comparator a b . objs)
  (if (eqv? (comparator-compare comparator a b) -1)
    (if (null? objs) #t (apply <? comparator b objs))
    #f))

#; ; now imported from (srfi 128)
(define (>? comparator a b . objs)
  (if (eqv? (comparator-compare comparator a b) 1)
    (if (null? objs) #t (apply >? comparator b objs))
    #f))

#; ; now imported from (srfi 128)
(define (<=? comparator a b . objs)
  (if (not (eqv? (comparator-compare comparator a b) 1))
    (if (null? objs) #t (apply <=? comparator b objs))
    #f))

#; ; now imported from (srfi 128)
(define (>=? comparator a b . objs)
  (if (not (eqv? (comparator-compare comparator a b) -1))
    (if (null? objs) #t (apply >=? comparator b objs))
    #f))

;;; Minimum and maximum comparison predicate

(define comparator-min
  (case-lambda
    ((comparator a)
      a)
    ((comparator a b)
     (if (<? comparator a b) a b))
    ((comparator a b . objs)
     (comparator-min comparator a (apply comparator-min comparator b objs)))))

(define comparator-max
  (case-lambda
    ((comparator a)
      a)
    ((comparator a b)
     (if (>? comparator a b) a b))
    ((comparator a b . objs)
     (comparator-max comparator a (apply comparator-max comparator b objs)))))

