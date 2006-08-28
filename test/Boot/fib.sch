;;; The amazing Fibonacci!!
;;;
;;; $Id$

(define (go symlist argv)
  (test6))

;;; Arithmetic (no allocation).
;;; Should return fixnum '2'.

(define (test1)
  (remainder (gcd (fact (fib 6)) 81) 7))

(define (gcd x y)
  (cond ((or (zero? x) (zero? y))
	 (+ x y))
	((< x y)
	 (let ((q (quotient y x)))
	   (gcd x (- y (* q x)))))
	((> x y)
	 (let ((q (quotient x y)))
	   (gcd (- x (* q y)) y)))
	(else
	 x)))

(define (fact n)
    (if (= n 1)
	1
	(* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

;;; Storage allocation 1: lots of allocation, but no stack growth,
;;; and little live data.  Provokes several GCs.
;;;
;;; Should return fixnum '75025'.

(define (test2)
  (cpsfib 25))

(define (cpsfib n)
  (cpsfib2 n (lambda (x) x)))

(define (cpsfib2 n k)
  (if (< n 2)
      (k n)
      (cpsfib2 (- n 1) 
	       (lambda (v)
		 (cpsfib2 (- n 2) 
			  (lambda (w)
			    (k (+ v w))))))))

;;; Storage allocation 2: lots of allocation, stack growth, and a
;;; great deal of live data.  Provokes several GCs.
;;;
;;; Should return fixnum '196418'.

(define (test3)
  (sumtree (fibtree 27)))

(define (fibtree n)
  (if (< n 2)
      n
      (cons (fibtree (- n 1)) (fibtree (- n 2)))))

(define (sumtree t)
  (if (number? t)
      t
      (+ (sumtree (car t)) (sumtree (cdr t)))))


;;; Perversion 1: fib using first-class continuations and non-tail calls.
;;; Provokes several GCs.
;;;
;;; Should return fixnum '10946'.

(define (test4)
  (ccfib 21))

(define (ccfib n)
  (call-with-current-continuation
   (lambda (k)
     (ccfib2 n k))))

(define (ccfib2 n k)
  (if (< n 2)
      (k n)
      (k (+ (call-with-current-continuation
	     (lambda (k1)
	       (ccfib2 (- n 1) k1)))
	    (call-with-current-continuation
	     (lambda (k2)
	       (ccfib2 (- n 2) k2)))))))


;;; Perversion #2: Fibonnaci with unary numbers represented as vectors.
;;; Enough weirdness to test all the vector primitives.

(define (test5)
  (vecfib 10))

(define (vecfib n)
  (vecfib-loop (number->vector n) vec-zero vec-one))

(define (vecfib-loop n fib-2 fib-1)
  (if (vector<? n vec-two)
      (vector->number fib-1)
      (vecfib-loop (vector- n vec-one) fib-1 (vector+ fib-2 fib-1))))

(define (number->vector n)
  (make-vector n 1))

(define (vector->number v)
  (let loop ((sum 0) (i 0))
    (if (= i (vector-length v))
	sum
	(loop (+ sum (vector-ref v i)) (+ i 1)))))

(define (vector<? a b)
  (< (vector->number a) (vector->number b)))

(define (vector+ a b)
  (vector-append a b))

(define (vector-append a b)
  (let ((la (vector-length a))
	(lb (vector-length b)))
    (let ((v (make-vector (+ la lb) 0)))
      (do ((i 0 (+ i 1))
	   (j 0 (+ j 1)))
	  ((= i la))
	(vector-set! v j (vector-ref a i)))
      (do ((i 0  (+ i 1))
	   (j la (+ j 1)))
	  ((= i lb))
	(vector-set! v j (vector-ref b i)))
      v)))

(define (vector- a b)
  (number->vector (- (vector->number a) (vector->number b))))

(define vec-zero (number->vector 0))
(define vec-one (number->vector 1))
(define vec-two (number->vector 2))


;;; Perversion #3: Like above, but with strings, and using a binary
;;; representation for maximal gross-out effect.  Also tests char
;;; predicates somewhat.

(define (test6)
  (strfib 10)
  )

(define (strfib n)
  (strfib-loop (number->str n) str-zero str-one))

(define (strfib-loop n fib-2 fib-1)
  (if (str<? n str-two)
      (str->number fib-1)
      (strfib-loop (str- n str-one) fib-1 (str+ fib-2 fib-1))))

(define (str<? a b)
  (cond ((< (string-length a) (string-length b)) #t)
	((> (string-length a) (string-length b)) #f)
	(else
	 (let loop ((i (- (string-length a) 1)))
	   (cond ((< i 0) #f)
		 ((char<? (string-ref a i) (string-ref b i)) #t)
		 ((char>? (string-ref a i) (string-ref b i)) #f)
		 (else
		  (loop (- i 1))))))))

(define (str+ a b)
  (number->str (+ (str->number a) (str->number b))))

(define (str- a b)
  (number->str (- (str->number a) (str->number b))))

(define (number->str n)
  (cond ((zero? n)
	 "")
	((zero? (remainder n 2))
	 (string-append (number->str (quotient n 2)) "0"))
	(else
	 (string-append (number->str (quotient n 2)) "1"))))

(define (str->number n)
  (let loop ((i 0) (sum 0))
    (cond ((= i (string-length n))
	   sum)
	  ((char=? (string-ref n i) #\0)
	   (loop (+ i 1) (* sum 2)))
	  (else
	   (loop (+ i 1) (+ (* sum 2) 1))))))

(define (string-append a b)
  (let ((la (string-length a))
	(lb (string-length b)))
    (let ((s (make-string (+ la lb) #\space)))
      (do ((i 0 (+ i 1))
	   (j 0 (+ j 1)))
	  ((= i la))
	(string-set! s j (string-ref a i)))
      (do ((i 0  (+ i 1))
	   (j la (+ j 1)))
	  ((= i lb))
	(string-set! s j (string-ref b i)))
      s)))

; Yow!

(define (make-string n c)
  (let ((x (make-bytevector n)))
    (bytevector-fill! x (char->integer c))
    (typetag-set! x 1)
    x))

(define str-zero (number->str 0))
(define str-one (number->str 1))
(define str-two (number->str 2))

;(define (ifib n)
;  (ifib-loop n 0 1))
;
;(define (ifib-loop n fib-2 fib-1)
;  (if (< n 2)
;      fib-1
;      (ifib-loop (- n 1) fib-1 (+ fib-2 fib-1))))

