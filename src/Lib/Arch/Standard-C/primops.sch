; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Procedure definitions for integrable procedures.
;
; NOTE! This file must be compiled with integrate-usual-procedures on.

($$trace "primops")

; Special primops

(define .check!
  (let ((check (lambda (x y z exn)
                 (error "Check exception: " exn x y z))))
    (lambda (x . rest)
      (if (null? rest)
          (check #f #f #f x)
          (let ((y (car rest))
                (rest (cdr rest)))
            (if (null? rest)
                (check y #f #f x)
                (let ((z (car rest))
                      (rest (cdr rest)))
                  (if (null? rest)
                      (check y z #f x)
                      (check y z (car rest) x)))))))))

; General predicates

(define not (lambda (x) (not x)))
(define eq? (lambda (x y) (eq? x y)))
(define eqv? (lambda (x y) (eqv? x y)))

; Pairs and lists

(define null? (lambda (x) (null? x)))
(define pair? (lambda (x) (pair? x)))
(define car (lambda (x) (car x)))
(define cdr (lambda (x) (cdr x)))
(define car:pair (lambda (x) (car x)))
(define cdr:pair (lambda (x) (cdr x)))
(define cons (lambda (x y) (cons x y)))
(define set-car! (lambda (x y) (set-car! x y)))
(define set-cdr! (lambda (x y) (set-cdr! x y)))
'(define sys$partial-list->vector
  (lambda (x y) (sys$partial-list->vector x y)))

; Numbers

(define number? (lambda (x) (number? x)))
(define complex? (lambda (x) (complex? x)))
(define real? (lambda (x) (real? x)))
;(define rational? (lambda (x) (rational? x)))
(define integer? (lambda (x) (integer? x)))
(define fixnum? (lambda (x) (fixnum? x)))
(define flonum? (lambda (x) (flonum? x)))
(define compnum? (lambda (x) (compnum? x)))
(define exact? (lambda (x) (exact? x)))
(define inexact? (lambda (x) (inexact? x)))
(define exact->inexact (lambda (x) (exact->inexact x)))
(define inexact->exact (lambda (x) (inexact->exact x)))
(define round (lambda (x) (round x)))
(define truncate (lambda (x) (truncate x)))
(define zero? (lambda (x) (zero? x)))
(define -- (lambda (x) (-- x)))
(define fxlognot (lambda (x) (fxlognot x)))
(define real-part (lambda (x) (real-part x)))
(define imag-part (lambda (x) (imag-part x)))
(define quotient (lambda (x y) (quotient x y)))
(define fxlogand (lambda (x y) (fxlogand x y)))
(define fxlogior (lambda (x y) (fxlogior x y)))
(define fxlogxor (lambda (x y) (fxlogxor x y)))
(define fxlsh (lambda (x y) (fxlsh x y)))
(define fxrshl (lambda (x y) (fxrshl x y)))
(define fxrsha (lambda (x y) (fxrsha x y)))
; (define rot (lambda (x y) (rot x y)))
(define remainder (lambda (x y) (remainder x y)))

(define +
  (letrec ((loop (lambda (sum args)
		   (if (null? args)
		       sum
		       (loop (+ sum (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  0
	  (loop (car args) (cdr args))))))

(define - 
  (letrec ((loop (lambda (diff args)
		   (if (null? args)
		       diff
		       (loop (- diff (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (-- arg)
	  (loop arg args)))))

(define * 
  (letrec ((loop (lambda (prod args)
		   (if (null? args)
		       prod
		       (loop (* prod (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  1
	  (loop (car args) (cdr args))))))

(define /
  (letrec ((loop (lambda (quot args)
		   (if (null? args)
		       quot
		       (loop (/ quot (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (/ 1 arg)
	  (loop arg args)))))

(define (make-nary-comparison name binop)
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((binop first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (binop a b)
	  (and (binop a b)
	       (loop b rest))))))

(define =  (make-nary-comparison '=  (lambda (x y) (=  x y))))
(define <  (make-nary-comparison '<  (lambda (x y) (<  x y))))
(define <= (make-nary-comparison '<= (lambda (x y) (<= x y))))
(define >  (make-nary-comparison '>  (lambda (x y) (>  x y))))
(define >= (make-nary-comparison '>= (lambda (x y) (>= x y))))

(define =:fix:fix (lambda (x y) (= x y)))
(define <:fix:fix (lambda (x y) (< x y)))
(define <=:fix:fix (lambda (x y) (<= x y)))
(define >:fix:fix (lambda (x y) (> x y)))
(define >=:fix:fix (lambda (x y) (>= x y)))

; Characters

(define char? (lambda (x) (char? x)))
(define char->integer (lambda (x) (char->integer x)))
(define integer->char (lambda (x) (integer->char x)))
(define char=?  (make-nary-comparison 'char=?  (lambda (x y) (char=?  x y))))
(define char<?  (make-nary-comparison 'char<?  (lambda (x y) (char<?  x y))))
(define char<=? (make-nary-comparison 'char<=? (lambda (x y) (char<=? x y))))
(define char>?  (make-nary-comparison 'char>?  (lambda (x y) (char>?  x y))))
(define char>=? (make-nary-comparison 'char>=? (lambda (x y) (char>=? x y))))

; Strings

(define make-string
  (lambda (x . rest)
    (if (null? rest)
	(make-string x #\space)
	(make-string x (car rest)))))
(define string? (lambda (x) (string? x)))
(define string-length (lambda (x) (string-length x)))
(define string-ref (lambda (x y) (string-ref x y)))
(define string-set! (lambda (x y z) (string-set! x y z)))

; Ustrings (temporary)

(define make-ustring
  (lambda (x . rest)
    (if (null? rest)
	(make-ustring x #\space)         ; fixme
	(make-ustring x (car rest)))))
(define ustring? (lambda (x) (ustring? x)))
(define ustring-length (lambda (x) (ustring-length x)))
(define ustring-ref (lambda (x y) (ustring-ref x y)))
(define ustring-set! (lambda (x y z) (ustring-set! x y z)))

; Vectors

(define make-vector
  (lambda (x . rest) 
    (if (null? rest)
	(make-vector x #f)
	(make-vector x (car rest)))))
(define vector? (lambda (x) (vector? x)))
(define vector-length (lambda (x) (vector-length x)))
(define vector-ref (lambda (x y) (vector-ref x y)))
(define vector-length:vec (lambda (x) (vector-length x)))
(define vector-ref:trusted (lambda (x y) (vector-ref x y)))
(define vector-set!:trusted (lambda (x y z) (vector-set! x y z)))

; Vector-like

(define vector-set! (lambda (x y z) (vector-set! x y z)))
(define vector-like? (lambda (x) (vector-like? x)))
(define vector-like-ref (lambda (x y) (vector-like-ref x y)))
(define vector-like-set! (lambda (x y z) (vector-like-set! x y z)))
(define vector-like-length (lambda (x) (vector-like-length x)))

; Bytevectors

(define bytevector? (lambda (x) (bytevector? x)))
(define bytevector-length (lambda (x) (bytevector-length x)))
(define make-bytevector
  (lambda (x . rest) 
    (if (null? rest)
	(make-bytevector x)
        (let ((bv (make-bytevector x)))
          (bytevector-fill! bv (car rest))
          bv))))
(define bytevector-ref (lambda (x y) (bytevector-ref x y)))
(define bytevector-set! (lambda (x y z) (bytevector-set! x y z)))
(define bytevector-fill! (lambda (x y) (bytevector-fill! x y)))

; Bytevector-like

(define bytevector-like? (lambda (x) (bytevector-like? x)))
(define bytevector-like-ref (lambda (x y) (bytevector-like-ref x y)))
(define bytevector-like-set! (lambda (x y z) (bytevector-like-set! x y z)))
(define bytevector-like-length (lambda (x) (bytevector-like-length x)))

(define bytevector-like-compare
  (lambda (x y)
    (if (and (bytevector-like? x)
	     (bytevector-like? y))
	(sys$bvlcmp x y)
	(begin (error "bytevector-like-compare: bad arguments: " x " " y)
	       #t))))

; Structures and typetags

(define structure? (lambda (x) (structure? x)))
(define typetag (lambda (x) (typetag x)))
(define typetag-set! (lambda (x y) (typetag-set! x y)))

; Procedures

(define procedure? (lambda (x) (procedure? x)))
(define procedure-length (lambda (x) (procedure-length x)))
(define make-procedure (lambda (x) (make-procedure x)))
(define procedure-ref (lambda (x y) (procedure-ref x y)))
(define procedure-set! (lambda (x y z) (procedure-set! x y z)))

; Cells

(define .make-cell (lambda (x) (.make-cell x)))
(define .cell-ref (lambda (x) (.cell-ref x)))
(define .cell-set! (lambda (x y) (.cell-set! x y)))

; I/O system support

(define port? (lambda (x) (port? x)))
(define eof-object (lambda () (eof-object)))
(define eof-object? (lambda (x) (eof-object? x)))

; Miscellaneous

(define symbol? (lambda (x) (symbol? x)))
(define larceny-break (lambda () (larceny-break)))
(define .creg (lambda () (.creg)))                   ; FIXME
(define .creg-set! (lambda (x) (.creg-set! x)))      ; FIXME
(define undefined (lambda () (undefined)))
(define unspecified (lambda () (unspecified)))
(define enable-interrupts (lambda (n) (enable-interrupts n)))
(define disable-interrupts (lambda () (disable-interrupts)))
(define gc-counter (lambda () (gc-counter)))
(define major-gc-counter (lambda () (major-gc-counter)))
(define .internal:machine-address (lambda (x) (.internal:machine-address x)))

; Fixnum primitives

(define most-negative-fixnum (lambda () (- (- #x1FFFFFFF) 1)))
(define most-positive-fixnum (lambda () #x1FFFFFFF))

; Bignum primitives.

;;; bignum-add-step!
;;;
;;; Given the byte index of the next 32-bit word in b and c,
;;; and a 1-bit carry, sets c[j] and the following bytes of c
;;; to the sum of those bytes and b[i...] and the carry.
;;; Returns the new 1-bit carry.

(define (bignum-add-step! b c i j carry)

; (.bignum-add-step!)

  (let* ((bi0 (bytevector-like-ref b i))
         (bi1 (bytevector-like-ref b (+ i 1)))
         (bi2 (bytevector-like-ref b (+ i 2)))
         (bi3 (bytevector-like-ref b (+ i 3)))

         (blo16 (fxlogior (fxlsh bi1 8) bi0))
         (bhi16 (fxlogior (fxlsh bi3 8) bi2))

         (cj0 (bytevector-like-ref c j))
         (cj1 (bytevector-like-ref c (+ j 1)))
         (cj2 (bytevector-like-ref c (+ j 2)))
         (cj3 (bytevector-like-ref c (+ j 3)))

         (clo16 (fxlogior (fxlsh cj1 8) cj0))
         (chi16 (fxlogior (fxlsh cj3 8) cj2))

         (t0 (+ blo16 clo16 carry))
         (lo16 (fxlogand #xffff t0))
         (t1 (+ bhi16 chi16 (fxrshl t0 16)))
         (hi16 (fxlogand #xffff t1))
         (carry (fxrshl t1 16))

         (cj0 (fxlogand #xff lo16))
         (cj1 (fxrshl lo16 8))
         (cj2 (fxlogand #xff hi16))
         (cj3 (fxrshl hi16 8)))

    (bytevector-like-set! c j cj0)
    (bytevector-like-set! c (+ j 1) cj1)
    (bytevector-like-set! c (+ j 2) cj2)
    (bytevector-like-set! c (+ j 3) cj3)

    carry))

;;; bignum-subtract-step!
;;;
;;; Given the byte index of the next 32-bit word in b and c,
;;; and a 1-bit borrow, sets c[j] and the following bytes of c
;;; to the difference of those bytes and b[i...] and the borrow.
;;;
;;; Returns the new 1-bit borrow.

(define (bignum-subtract-step! b c i j borrow)

; (.bignum-subtract-step!)

  (let* ((bi0 (bytevector-like-ref b i))
         (bi1 (bytevector-like-ref b (+ i 1)))
         (bi2 (bytevector-like-ref b (+ i 2)))
         (bi3 (bytevector-like-ref b (+ i 3)))

         (blo16 (fxlogior (fxlsh bi1 8) bi0))
         (bhi16 (fxlogior (fxlsh bi3 8) bi2))

         (cj0 (bytevector-like-ref c j))
         (cj1 (bytevector-like-ref c (+ j 1)))
         (cj2 (bytevector-like-ref c (+ j 2)))
         (cj3 (bytevector-like-ref c (+ j 3)))

         (clo16 (fxlogior (fxlsh cj1 8) cj0))
         (chi16 (fxlogior (fxlsh cj3 8) cj2))

         (t0 (- clo16 blo16 borrow))
         (lo16 (fxlogand #xffff t0))
         (t1 (+ (- chi16 bhi16) (fxrsha t0 16)))
         (hi16 (fxlogand #xffff t1))
         (borrow (fxlogand 1 (fxrshl t1 16)))

         (cj0 (fxlogand #xff lo16))
         (cj1 (fxrshl lo16 8))
         (cj2 (fxlogand #xff hi16))
         (cj3 (fxrshl hi16 8)))

    (bytevector-like-set! c j cj0)
    (bytevector-like-set! c (+ j 1) cj1)
    (bytevector-like-set! c (+ j 2) cj2)
    (bytevector-like-set! c (+ j 3) cj3)

    borrow))


;;; bignum-multiply-step!
;;;
;;;     b is a bytevector-like bignum (little-endian 32-bit bigits)
;;;     c is a bytevector-like bignum (little-endian 32-bit bigits)
;;;     i is a bytevector (not bignum) index into b
;;;     j is a bytevector (not bignum) index into c
;;;     k is a 4-byte bytevector
;;;     carry is a 4-byte bytevector
;;;
;;; The step
;;; 
;;; 	multiplies the 32 bits starting at b[i] by the 32 bits in k
;;;     adds the 32 bits starting at c[j]
;;;     adds the 32 bits in carry
;;;     stores the high-order 32 bits of that result into carry
;;;     stores the low-order 32 bits of that result into c[j]
;;;
;;; No result is returned.

(define (bignum-multiply-step! b c i j k carry)

; (.bignum-multiply-step!)

  (let* ((khi (bytevector-u16-native-ref k 2))
         (klo (bytevector-u16-native-ref k 0))
         (carry-hi (bytevector-u16-native-ref carry 2))
         (carry-lo (bytevector-u16-native-ref carry 0))

         (bi0 (bytevector-like-ref b i))
         (bi1 (bytevector-like-ref b (+ i 1)))
         (bi2 (bytevector-like-ref b (+ i 2)))
         (bi3 (bytevector-like-ref b (+ i 3)))

         (cj0 (bytevector-like-ref c j))
         (cj1 (bytevector-like-ref c (+ j 1)))
         (cj2 (bytevector-like-ref c (+ j 2)))
         (cj3 (bytevector-like-ref c (+ j 3)))

         ; 24-bit intermediate results

         (bi0*klo (* bi0 klo))              ; shifted 0
         (bi1*klo (* bi1 klo))              ; shifted 8
         (bi2*klo (* bi2 klo))              ; shifted 16
         (bi3*klo (* bi3 klo))              ; shifted 24
         (bi0*khi (* bi0 khi))              ; shifted 16
         (bi1*khi (* bi1 khi))              ; shifted 24
         (bi2*khi (* bi2 khi))              ; shifted 32
         (bi3*khi (* bi3 khi))              ; shifted 40

         (t0 (+ carry-lo
                bi0*klo
                cj0))
         (cj0 (fxlogand #xff t0))
         (t1 (+ (fxrshl t0 8)
                bi1*klo
                cj1))
         (cj1 (fxlogand #xff t1))
         (t2 (+ (fxrshl t1 8)
                carry-hi
                bi2*klo
                bi0*khi
                cj2))
         (cj2 (fxlogand #xff t2))
         (t3 (+ (fxrshl t2 8)
                bi3*klo
                bi1*khi
                cj3))
         (cj3 (fxlogand #xff t3))
         (t16 (+ (fxrshl t3 8)
                 bi2*khi
                 (fxlsh (fxlogand #xff bi3*khi) 8)))
         (carry-lo (fxlogand #xffff t16))
         (carry-hi (+ (fxrshl t16 16)
                   (fxrshl bi3*khi 8))))

    (bytevector-u16-native-set! carry 2 carry-hi)
    (bytevector-u16-native-set! carry 0 carry-lo)

    (bytevector-like-set! c j cj0)
    (bytevector-like-set! c (+ j 1) cj1)
    (bytevector-like-set! c (+ j 2) cj2)
    (bytevector-like-set! c (+ j 3) cj3)))

; eof
