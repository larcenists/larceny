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

;;;; Standard comparators and comparator constructors

;;; Standard atomic comparators

(define (boolean-comparison a b)
  (cond
    ((and a b) 0)
    (a 1)
    (b -1)
    (else 0)))

#; ; now imported from (srfi 128)
(define (boolean-hash obj) (if obj 1 0))

(define boolean-comparator
  (make-comparator boolean? boolean=? boolean-comparison boolean-hash))

(define char-comparison (make-comparison=/< char=? char<?))

#; ; now imported from (srfi 128)
(define (char-hash obj) (abs (char->integer obj)))

(define char-comparator
  (make-comparator char? char=? char-comparison char-hash))

(define char-ci-comparison (make-comparison=/< char-ci=? char-ci<?))

#; ; now imported from (srfi 128)
(define (char-ci-hash obj) (abs (char->integer (char-foldcase obj))))

(define char-ci-comparator
  (make-comparator char? char-ci=? char-ci-comparison char-ci-hash))

(define string-comparison (make-comparison=/< string=? string<?))

(define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define symbol-comparison (make-comparison=/< symbol=? symbol<?))

;; Comparison procedure for real numbers only
(define (real-comparison a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

;; Comparison procedure for non-real numbers.
(define (complex-comparison a b)
  (let ((real-result (real-comparison (real-part a) (real-part b))))
    (if (= real-result 0)
      (real-comparison (imag-part a) (imag-part b))
      real-result)))

;;; FIXME: this was broken in the reference implementation

;(define (number-hash obj) (exact (abs obj)))

#; ; now imported from (srfi 128)
(define (number-hash z)
  (cond ((exact-integer? z)
         (abs z))
        ((and (exact? z) (real? z))
         (+ (numerator z) (denominator z)))
        ((not (real? z))
         (+ (number-hash (real-part z))
            (* 3 (number-hash (imag-part z)))))
        ((nan? z)
         10286062)
        ((infinite? z)
         (if (= z +inf.0)
             11610730
             4191912))
        (else
         (+ 861625
            (number-hash (exact z))))))

(define number-comparator
  (make-comparator number? = complex-comparison number-hash))

(define complex-comparator
  (make-comparator complex? = complex-comparison number-hash))

(define real-comparator
  (make-comparator real? = real-comparison number-hash))

(define rational-comparator
  (make-comparator rational? = real-comparison number-hash))

(define integer-comparator
  (make-comparator integer? = real-comparison number-hash))

(define exact-integer-comparator
  (make-comparator exact-integer? = real-comparison number-hash))

;;; Inexact real comparator

;; Test procedure for inexact reals
(define (inexact-real? obj) (and (number? obj) (inexact? obj) (real? obj)))

;; Return a number appropriately rounded to epsilon
(define (rounded-to x epsilon rounding)
  (let ((quo (/ x epsilon)))
    (cond
      ((procedure? rounding) (rounding x epsilon))
      ((eq? rounding 'round) (round quo))
      ((eq? rounding 'ceiling) (ceiling quo))
      ((eq? rounding 'floor) (floor quo))
      ((eq? rounding 'truncate) (truncate quo))
      (else (error "invalid rounding specification" rounding)))))

;; Returns result of comparing a NaN with a non-NaN
(define (nan-comparison nan-handling which other)
  (cond
    ((procedure? nan-handling)
     (nan-handling other))
    ((eq? nan-handling 'error)
     (error "Attempt to compare NaN with non-NaN"))
    ((eq? nan-handling 'min)
     (if (eq? which 'a-nan) -1 1))
    ((eq? nan-handling 'max)
     (if (eq? which 'a-nan) 1 -1))
    (else
     (error "Invalid nan-handling specification"))))

(define (make-inexact-real-comparison epsilon rounding nan-handling)
  (lambda (a b)
    (let ((a-nan? (nan? a)) (b-nan? (nan? b)))
      (cond
        ((and a-nan? b-nan?) 0)
        (a-nan? (nan-comparison nan-handling 'a-nan b))
        (b-nan? (nan-comparison nan-handling 'b-nan a))
        (else (real-comparison
                (rounded-to a epsilon rounding)
                (rounded-to b epsilon rounding)))))))

;; Return 0 for NaN, number-hash otherwise
(define (make-inexact-real-hash epsilon rounding)
  (lambda (obj)
    (if (nan? obj) 0 (number-hash (rounded-to obj epsilon rounding)))))

(define (make-inexact-real-comparator epsilon rounding nan-handling)
  (make-comparator
    inexact-real?
    #t
    (make-inexact-real-comparison epsilon rounding nan-handling)
    (make-inexact-real-hash epsilon rounding)))

;;; Sequence comparator constructors and comparators
;;; The hash functions are based on djb2, but
;;; modulo 2^20 instead of 2^32 in hopes of sticking to fixnums.

(define limit (expt 2 20))

;; Makes a comparison procedure that works listwise
(define (make-listwise-comparison comparison null? car cdr)
  (letrec ((proc
    (lambda (a b)
      (let ((a-null? (null? a)) (b-null? (null? b)))
        (cond
          ((and a-null? b-null?) 0)
          (a-null? -1)
          (b-null? 1)
          (else (let ((result (comparison (car a) (car b))))
            (if (= result 0) (proc (cdr a) (cdr b)) result))))))))
    proc))

;; Makes a hash function that works listwise
(define (make-listwise-hash hash null? car cdr)
  (lambda (obj)
    (let loop ((obj obj) (result 5381))
      (if (null? obj)
        0
        (let* ((prod (modulo (* result 33) limit))
               (sum (+ prod (hash (car obj)))))
          (loop (cdr obj) sum))))))

;; Makes a comparison procedure that works vectorwise
;;
;; FIXME: the reference implementation blew up when comparing two empty vectors

(define (make-vectorwise-comparison comparison length ref)
  (lambda (a b)
    (let* ((a-length (length a))
           (b-length (length b)))
      (cond
        ((< a-length b-length) -1)
        ((> a-length b-length) 1)
        (else
         (let loop ((index 0))
           (if (= index a-length)
               0
               (let ((result (comparison (ref a index) (ref b index))))
                 (if (= result 0)
                     (loop (+ index 1))
                     result)))))))))

;; Makes a hash function that works vectorwise
;;
;; FIXME: the reference implementation ignored element 0

(define (make-vectorwise-hash hash length ref)
  (lambda (obj)
    (let loop ((index (- (length obj) 1)) (result 5381))
      (if (< index 0)
        result
        (let* ((prod (modulo (* result 33) limit))
               (sum (modulo (+ prod (hash (ref obj index))) limit)))
          (loop (- index 1) sum))))))

#; ; now imported from (srfi 128)
(define string-hash
  (make-vectorwise-hash char-hash string-length string-ref))

(define string-comparator
  (make-comparator string? string=? string-comparison string-hash))

#; ; now imported from (srfi 128)
(define (string-ci-hash obj) (string-hash (string-foldcase obj)))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

#; ; now imported from (srfi 128)
(define (symbol-hash obj) (string-hash (symbol->string obj)))

(define symbol-comparator
  (make-comparator symbol? symbol=? symbol-comparison symbol-hash))

(define (make-listwise-comparator test comparator null? car cdr)
  (make-comparator
    test
    #t
    (make-listwise-comparison
      (comparator-comparison-procedure comparator) null? car cdr)
    (make-listwise-hash
      (comparator-hash-function comparator) null? car cdr)))

(define (make-vectorwise-comparator test comparator length ref)
  (make-comparator
    test
    #t
    (make-vectorwise-comparison
      (comparator-comparison-procedure comparator) length ref)
    (make-vectorwise-hash
      (comparator-hash-function comparator) length ref)))

#; ; now imported from (srfi 128) with a different number of arguments
(define (make-list-comparator comparator)
   (make-listwise-comparator
     (lambda (obj) (or (null? obj) (pair? obj)))
     comparator null? car cdr))

;;; SRFI 128 adds four more formal parameters to make-list-comparator

;(define list-comparator (make-list-comparator default-comparator))

(define list-comparator
  (make-list-comparator default-comparator
                        (lambda (obj) (or (null? obj) (pair? obj)))
                        null?
                        car
                        cdr))

#; ; now imported from (srfi 128) with a different number of arguments
(define (make-vector-comparator comparator)
  (make-vectorwise-comparator vector? comparator vector-length vector-ref))

;;; SRFI 128 adds three more formal parameters to make-vector-comparator

;(define vector-comparator (make-vector-comparator default-comparator))

(define vector-comparator
  (make-vector-comparator default-comparator vector? vector-length vector-ref))

(define vector-comparison (comparator-comparison-procedure vector-comparator))

(define vector-hash (comparator-hash-function vector-comparator))

(define (make-bytevector-comparator comparator)
  (make-vectorwise-comparator
    bytevector? comparator bytevector-length bytevector-u8-ref))

(define bytevector-comparator (make-bytevector-comparator default-comparator))

(define bytevector-comparison
  (comparator-comparison-procedure bytevector-comparator))

(define bytevector-hash
  (comparator-hash-function bytevector-comparator))

;;; Pair comparator constructors

(define (make-car-comparator comparator)
  (make-comparator
    pair?
    #t
    (lambda (a b)
       (comparator-compare comparator (car a) (car b)))
    (lambda (obj) (comparator-hash-function comparator))))

(define (make-cdr-comparator comparator)
  (make-comparator
    pair?
    #t
    (lambda (a b)
       (comparator-compare comparator (cdr a) (cdr b)))
    (lambda (obj) (comparator-hash comparator obj))))

(define (make-pair-comparison car-comparator cdr-comparator)
  (lambda (a b)
    (let ((result (comparator-compare car-comparator (car a) (car b))))
      (if (= result 0)
        (comparator-compare cdr-comparator (cdr a) (cdr b))
        result))))

(define pair-comparison
  (make-pair-comparison default-comparator default-comparator))

(define (make-pair-hash car-comparator cdr-comparator)
  (lambda (obj)
    (+
      (comparator-hash car-comparator (car obj))
      (comparator-hash cdr-comparator (cdr obj)))))

#; ; now imported from (srfi 128)
(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator
    pair?
    #t
    (make-pair-comparison car-comparator cdr-comparator)
    (make-pair-hash car-comparator cdr-comparator)))

(define pair-comparator
  (make-pair-comparator default-comparator default-comparator))
(define pair-hash (comparator-hash-function pair-comparator))


;; Compute type index for inexact list comparisons
(define (improper-list-type obj)
  (cond
    ((null? obj) 0)
    ((pair? obj) 1)
    (else 2)))

(define (make-improper-list-comparison comparator)
  (let ((pair-comparison (make-pair-comparison comparator comparator)))
    (lambda (a b)
      (let* ((a-type (improper-list-type a))
            (b-type (improper-list-type b))
            (result (real-comparison a-type b-type)))
        (cond
           ((not (= result 0)) result)
           ((null? a) 0)
           ((pair? a) (pair-comparison a b))
           (else (comparator-compare comparator a b)))))))

(define (make-improper-list-hash comparator)
  (lambda (obj)
    (cond
      ((null? obj) 0)
      ((pair? obj) (+ (comparator-hash comparator (car obj))
                      (comparator-hash comparator (cdr obj))))
      (else (comparator-hash comparator obj)))))

(define (make-improper-list-comparator comparator)
  (make-comparator
    #t
    #t
    (make-improper-list-comparison comparator)
    (make-improper-list-hash comparator)))

;;; Wrapped equality predicates
;;; These comparators don't have comparison functions.

#|

(define eq-comparator
  (make-comparator
    #t
    eq?
    #f
    default-hash-function))

(define eqv-comparator
  (make-comparator
    #t
    eqv?
    #f
    default-hash-function))

(define equal-comparator
  (make-comparator
    #t
    equal?
    #f
    default-hash-function))

|#

(define eq-comparator (make-eq-comparator))
(define eqv-comparator (make-eqv-comparator))
(define equal-comparator (make-equal-comparator))
