; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Bytevector primitives for Chez Scheme.
; We simulate bytevectors with vectors. It's so easy...

; The primitives should be macros!

(define *bv-key* (vector 'bytevector))

(define (make-bytevector n)
  (let ((v (make-vector (+ n 1) '*bv-init*)))
    (vector-set! v 0 *bv-key*)
    v))

(define (bytevector-ref bv k) (vector-ref bv (+ k 1)))
(define (bytevector-set! bv k v) (vector-set! bv (+ k 1) v))
(define (bytevector-length bv) (- (vector-length bv) 1))
(define bytevector-copy vector-copy)
(define (bytevector? x) 
  (and (vector? x)
       (> (vector-length x) 0)
       (equal? (vector-ref x 0) *bv-key*)))   ; [sic]

(define (list->bytevector l)
  (list->vector (cons *bv-key* l)))

(define bytevector-tag-set! (lambda (x y) '()))

#;(define (integer->bytevector f)
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


#;(define (bytevector->integer b)
  (let ((l (bignum-length b)))
    (let loop ((f 0) (i (- l 1)))
      (if (>= i 0)
	  (loop (+ (* f bignum-base) (bignum-ref b i)) (- i 1))
	  (if (sign-negative? (bignum-sign b))
	      (- f)
	      f)))))

(define (write-bytevector-like bv . rest)
  (cond ((bytevector? bv)
	 (let ((limit (vector-length bv))
	       (port  (if (null? rest) (current-output-port) (car rest))))
	   (do ((i 1 (+ i 1)))
	       ((= i limit))
	     (write-char (integer->char (vector-ref bv i)) port))))
	((string? bv)
	 (apply display bv rest))
	(else
	 ???)))

(define bytevector-word-ref 
  (let ((two^8  (expt 2 8))
	(two^16 (expt 2 16))
	(two^24 (expt 2 24)))
    (lambda (bv i)
      (+ (* (bytevector-ref bv i) two^24)
	 (* (bytevector-ref bv (+ i 1)) two^16)
	 (* (bytevector-ref bv (+ i 2)) two^8)
	 (bytevector-ref bv (+ i 3))))))

; eof
