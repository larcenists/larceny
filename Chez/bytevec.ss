; Bytevector primitives for Chez Scheme.
;
; We simulate bytevectors with vectors. It's so easy...

; The primitives should be macros!

(define make-bytevector make-vector)
(define bytevector-ref vector-ref)
(define bytevector-set! vector-set!)
(define bytevector-length vector-length)
(define bytevector-copy vector-copy)
(define (bytevector? x) #f)

(define bytevector-tag-set! (lambda (x y) '()))

(define (integer->bytevector f)
  (let ((b (bignum-alloc (inexact->exact (ceiling (/ (log (abs f))
						     (log bignum-base)))))))
    (let loop ((i 0) (n (abs f)))
      (if (zero? n)
	  (begin (bignum-length-set! b i)
		 (if (negative? f)
		     (bignum-sign-set! b negative-sign))
		 b)
	  (begin (bignum-set! b i (remainder n bignum-base))
		 (loop (+ i 1) (quotient n bignum-base)))))))


(define (bytevector->integer b)
  (let ((l (bignum-length b)))
    (let loop ((f 0) (i (- l 1)))
      (if (>= i 0)
	  (loop (+ (* f bignum-base) (bignum-ref b i)) (- i 1))
	  (if (sign-negative? (bignum-sign b))
	      (- f)
	      f)))))

; eof
