; Copyright Lightship Software.
;
; $Id: num2str.sch,v 1.5 1997/07/18 13:55:49 lth Exp $
;
;                                       16 April 1990
;
;
; NOT TRUE anymore:
;  An IEEE-conforming implementation of number->string.
;  Assumes a fixnum/bignum/flonum implementation using
;  IEEE double precision flonums in a MacScheme-specific
;  format.  Uses a MacScheme-specific routine for bignums.
;

($$trace "num2str")

(define number->string
  
  (let ()
    
    (define (number->string x . radix)
      (cond ((null? radix)
             (number2string x 10))
            (else (let ((radix (car radix)))
                    (if (and (< 1 radix 37)
                             (exact? radix)
                             (integer? radix))
                        (number2string x radix)
                        (begin (error "Bad radix" radix) #t))))))
    
    (define (number2string x radix)
      (cond ((fixnum? x)
             (integer->string x radix))
	    ((flonum? x)
	     (flonum->string x radix))
	    ((compnum? x)
	     (compnum->string x radix))
            ((bignum? x)
             (bignum->string x radix))
	    ((ratnum? x)
	     (ratnum->string x radix))
	    ((rectnum? x)
	     (rectnum->string x radix))
	    (else
	     (error "number->string: not a number: " x)
	     #t)))

    (define (compnum->string x radix)
      (let ((r (real-part x))
	    (i (imag-part x)))
	(cond ((= i 0.0)
	       (flonum->string i radix))
	      ; FIXME: could convert to rectnum, then do #i.
	      ((not (= radix 10))
	       (error "number->string: can't do complexes in non-10 radix: " 
		      x)
	       #t)
	      (else
	       ; A little mysterious, to deal with +/-inf.0, +nan.0
	       (let ((rr (flonum->string r 10))
		     (ii (flonum->string i 10)))
		 (string-append rr
				(let ((c (string-ref ii 0)))
				  (if (and (not (char=? c #\+))
					   (not (char=? c #\-)))
				      "+"
				      ""))
				ii
				"i"))))))

    (define (flonum->string x radix)
      (let ((exp (float-exponent x)))
	(cond ((= exp flonum:maxexponent)
	       (cond ((not (= x x))
		      (string-copy "+nan.0"))
		     ((positive? x)
		      (string-copy "+inf.0"))
		     (else (string-copy "-inf.0"))))
	      ((= radix 10)
	       (if (= x 0.0)
		   (string-copy (if (= (float-sign x) 0) "0.0" "-0.0"))
		   (string-append (if (negative? x) "-" "")
				  (dragon (float-significand x)
					  exp))))
	      ((= x 0.0)
	       (string-copy "#i0"))
	      (else
	       (let* ((p (abs (inexact->exact (numerator x))))
		      (q (inexact->exact (denominator x))))
		 (string-append "#i"
				(if (negative? x) "-" "")
				(number2string p radix)
				(if (not (= q 1))
				    (string-append "/"
						   (number2string q radix))
				    "")))))))
    
    (define flonum:infinity 1e500)
    (define flonum:maxexponent 972)
    (define flonum:minexponent -1023)
    (define two^n-1 4503599627370496)
    
    (define (integer->string n radix)
      (cond ((not (integer? n)) ???number2string)
            ((not (exact? n))
             (string-append "#i" (integer->string (inexact->exact n) radix)))
            ((not (fixnum? n)) (bignum->string n radix))
            ((negative? n)
             (string-append "-" (integer->string-loop (- n) radix '())))
            ((zero? n) "0")
            (else (integer->string-loop n radix '()))))
    
    (define (integer->string-loop n radix chars)
      (cond ((zero? n) (list->string chars))
            (else (let ((q (quotient n radix)))
                    (integer->string-loop 
		     q
		     radix
		     (cons (vector-ref **digit-characters**
				       (- n (* radix q)))
			   chars))))))
    
    (define **digit-characters**
      '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
         #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
         #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
    
    ; bignum->string takes two arguments:  a bignum and a radix
    ; For Larceny, bignum->string is defined in "bignums.sch".

;    (define bignum->string
;      (->procedure
;       (list (cons (list->bytevector
;                    '(149 2      ; args=   2
;                          157        ; locals  1
;                          161        ; local   1
;                          151        ; push
;                          160        ; local   0
;                          116        ; bigdigits
;                          152))      ; restore
;                   '#(#f)))))
    
    ; "Simplified Floating-Point Printout Algorithm"
    ; from Tables 12 and 13 of draft of Steele and White.
    ;
    ; Given significand f >= 0 and exponent e as exact integers,
    ; returns a string representing f*2^e.
    
    (define (dragon f e)
      (cond ((zero? f) (format '(0) 0))
            ((negative? e)
             (dragon-fixup f f (expt 2 (- e)) 1 1))
            (else
             (let ((shift (expt 2 e)))
               (dragon-fixup f (* f shift) 1 shift shift)))))
    
    (define (dragon-fixup f R S M- M+)
      (if (= f two^n-1)
          (dragon-fixup-loop1 f (* 2 R) (* 2 S) M- (* 2 M+) 0)
          (dragon-fixup-loop1 f R S M- M+ 0)))
    
    (define (dragon-fixup-loop1 f R S M- M+ k)
      (if (< R (quotient (+ S 9) 10))
          (dragon-fixup-loop1 f (* 10 R) S (* 10 M-) (* 10 M+) (- k 1))
          (dragon-fixup-loop2 f R S M- M+ k)))
    
    (define (dragon-fixup-loop2 f R S M- M+ k)
      (if (>= (+ (* 2 R) M+)
              (* 2 S))
          (dragon-fixup-loop2 f R (* 10 S) M- M+ (+ k 1))
          (dragon-loop f R S M- M+ k '())))
    
    ;(define (dragon-loop f R S M- M+ k digits)
    ;  (let* ((k (- k 1))
    ;         (R10 (* 10 R))
    ;         (U (quotient R10 S))
    ;         (R (- R10 (* U S)))
    ;         (M- (* 10 M-))
    ;         (M+ (* 10 M+))
    ;         (R2 (* 2 R)))
    ;    (if (and (>= R2 M-)
    ;             (<= R2 (- (* 2 S) M+)))
    ;        (dragon-loop f R S M- M+ k (cons U digits))
    ;        (dragon-done f R S M- M+ k digits U))))
    ;
    ;(define (dragon-done f R S M- M+ k digits U)
    ;  (let* ((R2 (* 2 R))
    ;         (low (< R2 M-))
    ;         (high (> R2 (- (* 2 S) M+))))
    ;    (cond ((and low (not high))
    ;           (format (cons U digits) k))
    ;          ((and high (not low))
    ;           (format (cons (+ U 1) digits) k))
    ;          ((and low high)
    ;           (cond ((< R2 S)  (format (cons U digits) k))
    ;                 ((> R2 S)  (format (cons (+ U 1) digits) k))
    ;                 ((even? U) (format (cons U digits) k))
    ;                 (else      (format (cons (+ U 1) digits) k)))))))
    
    ; In MacScheme, string->number reliably breaks ties by rounding to an
    ; even significand.  The following modifications take advantage of this
    ; to generate fewer digits in some cases.  Test cases: 1e23, 4e23,
    ; which lie exactly halfway between their closest approximations
    ; using IEEE double precision.
    
    (define (dragon-loop f R S M- M+ k digits)
      (let* ((k (- k 1))
             (R10 (* 10 R))
             (U (quotient R10 S))
             (R (- R10 (* U S)))
             (M- (* 10 M-))
             (M+ (* 10 M+))
             (R2 (* 2 R)))
        (cond ((and (>= R2 M-)
                    (< R2 (- (* 2 S) M+)))
               (dragon-loop f R S M- M+ k (cons U digits)))
              ((and (= R2 (- (* 2 S) M+)) (odd? f))
               (dragon-loop f R S M- M+ k (cons U digits)))
              (else (dragon-done f R S M- M+ k digits U)))))
    
    (define (dragon-done f R S M- M+ k digits U)
      (let* ((R2 (* 2 R))
             (low (< R2 M-))
             (high (> R2 (- (* 2 S) M+))))
        (cond ((and low (not high))
               (format (cons U digits) k))
              ((and high (not low))
               (format (cons (+ U 1) digits) k))
              ((and low high)
               (cond ((< R2 S)  (format (cons U digits) k))
                     ((> R2 S)  (format (cons (+ U 1) digits) k))
                     ((even? U) (format (cons U digits) k))
                     (else      (format (cons (+ U 1) digits) k))))
              (else (format (cons (+ U 1) digits) k)))))
    
    ; MacScheme's traditional heuristic-only format.
    ;
    ; Given a nonempty list of digits (not characters) in reverse order,
    ; and an exponent n such that the value is f*10^n, where f is the
    ; integer represented by the digits, returns a string.
    
    (define (format digits e)
      (let* ((s (list->string
                 (reverse
                  (map (lambda (digit)
                         (vector-ref **digit-characters** digit))
                       digits))))
             (n (+ -1 e (string-length s))))
        (cond ((< n -5) (exponential-format s n))
              ((> n 8)  (exponential-format s n))
              (else     (decimal-format s e)))))
    
    (define (exponential-format s n)
      (string-append (substring s 0 1)
                     "."
                     (if (> (string-length s) 1)
                         (substring s 1 (string-length s))
                         "0")
                     "e"
                     (integer->string n 10)))
    
    (define (decimal-format s n)
      (let ((k (string-length s)))
        (cond ((negative? n)
               (if (positive? (+ n k))
                   (string-append (substring s 0 (+ n k))
                                  "."
                                  (substring s (+ n k) k))
                   (string-append "0."
                                  (make-string (- (+ n k)) #\0)
                                  s)))
              (else (string-append s (make-string n #\0) ".0")))))
    
; These are global in Larceny.
;
;    ; MacScheme-specific code.
;    ; Depends on the representations of bignums as well as flonums.
;    
;    (define (float-significand x)
;      (let ((x (->bytevector x))
;            (n (make-bytevector 12)))
;        (bytevector-set! n 0 0)
;        (bytevector-set! n 1 1)   ; sign
;        (bytevector-set! n 2 0)
;        (bytevector-set! n 3 4)   ; size (in bignum digits)
;        (bytevector-set! n 4 (bytevector-ref x 6))
;        (bytevector-set! n 5 (bytevector-ref x 7))
;        (bytevector-set! n 6 (bytevector-ref x 4))
;        (bytevector-set! n 7 (bytevector-ref x 5))
;        (bytevector-set! n 8 (bytevector-ref x 2))
;        (bytevector-set! n 9 (bytevector-ref x 3))
;        (bytevector-set! n 10 0)
;        (bytevector-set! n 11 (logior 16 (logand 15 (bytevector-ref x 1))))
;        
;        ; subtract hidden bit if x is denormalized
;        
;        (if (and (zero? (bytevector-ref x 0))
;                 (zero? (logand -16 (bytevector-ref x 1))))
;            (- (typetag-set! n (typetag two^n-1)) two^n-1)
;            (typetag-set! n (typetag two^n-1)))))
;    
;    (define (float-exponent x)
;      (let* ((x (->bytevector x))
;             (e (logior (lsh (logand 127 (bytevector-ref x 0)) 4)
;                        (lsh (bytevector-ref x 1) -4))))
;        (if (zero? e)
;            (- flonum:minexponent 51)          ; no hidden bit
;            (- e (+ 1023 52)))))
 
    number->string))
