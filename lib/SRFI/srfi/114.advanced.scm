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

;;; Comparators constructed from other comparators

;;; Selecting comparator: finds the first one that type-tests

(define (matching-comparator obj comparators)
  (cond
    ((null? comparators) #f)
    ((comparator-test-type (car comparators) obj) (car comparators))
    (else (matching-comparator obj (cdr comparators)))))

(define (selected-type-test . comparators)
  (lambda (obj)
    (if (matching-comparator obj comparators) #t #f)))

(define (selected-equality-predicate comparators)
  (lambda (a b)
    (let ((comparator (matching-comparator a comparators)))
      (if comparator
        (comparator-equal? comparator a b)
        (error "no comparator can be selected")))))

(define (selected-comparison-procedure comparators)
  (lambda (a b)
    (let ((comparator (matching-comparator a comparators)))
      (if comparator
        (comparator-compare comparator a b)
        (error "no comparator can be selected")))))

(define (selected-hash-function comparators)
  (lambda (obj)
    (let ((comparator (matching-comparator obj comparators)))
      (if comparator
        (comparator-hash comparator obj)
        (error "no comparator can be selected")))))

(define (make-selecting-comparator . comparators)
  (make-comparator
    (selected-type-test comparators)
    (selected-equality-predicate comparators)
    (selected-comparison-procedure comparators)
    (selected-hash-function comparators)))

;;; Refining comparator: uses all type-matching comparators
;;; until one is found that can discriminate

(define (refined-equality-predicate comparators)
  (lambda (a b)
    (let loop ((comparator (matching-comparator a comparators))
               (first? #t))
      (if comparator
        (if (comparator-equal? comparator a b)
          (loop (matching-comparator a comparators) #f)
          #f)
        (if first? (error "no comparator can be selected") #t)))))

(define (refined-comparison-procedure comparators)
  (lambda (a b)
    (let loop ((comparator (matching-comparator a comparators))
               (first? #t))
      (if comparator
        (let ((result (comparator-compare comparator a b)))
          (if (eqv? result 0)
            (loop (matching-comparator a comparators) #f)
            result))
        (if first? (error "no comparator can be selected") 0)))))

(define (refined-hash-function comparators)
  (lambda (obj)
    (let loop ((comparators comparators) (last-comparator #f))
      (if (null? comparators)
         (if last-comparator
           (comparator-hash last-comparator obj)
           (error "no comparator can be selected"))
         (if (comparator-test-type (car comparators) obj)
           (loop (cdr comparators) (car comparators))
           (loop (cdr comparators) last-comparator))))))

(define (make-refining-comparator . comparators)
  (make-comparator
    (selected-type-test comparators)
    (refined-equality-predicate comparators)
    (refined-comparison-procedure comparators)
    (refined-hash-function comparators)))

;;; Reverse the sense of the comparator
(define (make-reverse-comparator comparator)
  (make-comparator
    (comparator-type-test-procedure comparator)
    (comparator-equality-predicate comparator)
    (lambda (a b) (- (comparator-compare comparator a b)))
    (comparator-hash-function comparator)))

;;; Handy debug-assert procedures for debugging comparators

(define (debug-assert bool who what)
  (if (not bool)
    (error (string-append
      (symbol->string what)
       " failure in "
       (symbol->string who)))))

(define (debug-deny bool who what) (debug-assert (not bool) who what))

;;; Checkers for debugging comparators

(define (check-type-test comparator a)
  (debug-assert (comparator-test-type comparator a) 'type 'validity))

(define (check-reflexive-equality comparator a)
  (debug-assert (comparator-equal? comparator a a) 'equality 'reflexive))

(define (check-reflexive-comparison comparator a)
  (debug-assert (eqv? (comparator-compare comparator a a) 0) 'comparison 'reflexive))

(define (check-symmetric-equality comparator a b)
  (if (comparator-equal? comparator a b)
    (debug-assert (comparator-equal? comparator b a) 'equality 'symmetric))
  (if (not (comparator-equal? comparator a b))
     (debug-deny (comparator-equal? comparator b a) 'equality 'symmetric)))

(define (check-asymmetric-comparison comparator a b)
  (debug-assert (eqv?
            (comparator-compare comparator a b)
            (- (comparator-compare comparator a b)))
    'comparison 'asymmetric))

(define (check-transitive-equality comparator a b c)
  (and (comparator-equal? comparator a b) (comparator-equal? comparator b c)
    (debug-assert (comparator-equal? comparator a c) 'equality 'transitive))
  (and (comparator-equal? comparator a b) (not (comparator-equal? comparator b c))
    (debug-deny (comparator-equal? comparator a c) 'equality 'transitive))
  (and (not (comparator-equal? comparator a b)) (comparator-equal? comparator b c)
    (debug-deny (comparator-equal? comparator a c) 'equality 'transitive)))

(define (check-transitive-comparison comparator a b c)
  (define (<= a b) (<=? comparator a b))
  (and (<= b a) (<= a c) (debug-assert (<= b c) 'comparison 'transitive))
  (and (<= c a) (<= a b) (debug-assert (<= c b) 'comparison 'transitive))
  (and (<= a b) (<= b c) (debug-assert (<= a c) 'comparison 'transitive))
  (and (<= c b) (<= b a) (debug-assert (<= c a) 'comparison 'transitive))
  (and (<= a c) (<= c b) (debug-assert (<= a b) 'comparison 'transitive))
  (and (<= b c) (<= c a) (debug-assert (<= b a) 'comparison 'transitive)))

(define (check-hash-value value)
  (debug-assert (and (positive? value) (exact-integer? value))
          'validity 'hash-value))

(define (check-all comparator a b c c?)
  (check-type-test comparator a)
  (check-type-test comparator b)
  (if c? (check-type-test comparator c))
  (check-reflexive-equality comparator a)
  (check-reflexive-equality comparator b)
  (if c? (check-reflexive-equality comparator c))
  (check-reflexive-comparison comparator a)
  (check-reflexive-comparison comparator b)
  (if c? (check-reflexive-comparison comparator c))
  (check-symmetric-equality comparator a b)
  (if c? (check-symmetric-equality comparator b c))
  (if c? (check-symmetric-equality comparator a c))
  (check-asymmetric-comparison comparator a b)
  (if c? (check-asymmetric-comparison comparator b c))
  (if c? (check-asymmetric-comparison comparator a c))
  (if c? (check-transitive-equality comparator a b c))
  (if c? (check-transitive-comparison comparator a b c)))

(define (make-debug-comparator comparator)
  (let ((c #f) (c? #f))
      (comparator-comparison-procedure? comparator)
    (make-comparator
      (comparator-type-test-procedure comparator)
      (lambda (a b)
        (check-all comparator a b c c?)
        (when (not c?) (set! c a) (set! c? #t))
        (comparator-equal? comparator a b))
      (if (comparator-comparison-procedure? comparator)
        (lambda (a b)
          (check-all comparator a b c c?)
          (when (not c?) (set! c b) (set! c? #t))
          (comparator-compare comparator a b))
        #f)
      (if (comparator-hash-function? comparator)
        (lambda (obj)
          (let ((value (comparator-hash comparator obj)))
            (check-hash-value value)
            value))
        #f))))
