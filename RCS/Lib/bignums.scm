; -*- Scheme -*-
;
; Scheme 313 runtime system
; Scheme code for bignum arithmetic (on Sun Sparc).
;
; $Id: bignums.scm,v 1.1 91/08/04 19:06:16 lth Exp Locker: lth $
;
; The layout of a bignum is this (recall that the Sparc is big-endian):
;
;    +------------------------+--------+
;    |   bytevector length    | hdrtag |
;    +----------------+-------+--------+
;    |   sign         |   digitcount   |
;    +----------------+----------------+
;    |   digit 1      |   digit 0      |
;    +----------------+----------------+
;    |   digit 3      |   digit 2      |
;    +----------------+----------------+
;    ...
;
; where the digitcount is the number of 32-bit bignum digits, and the sign
; is 0 for positive and 1 for negative. If the bignum is 0, then the
; sign is immaterial; the `digitcount' field must be 0.
;
; The following code operates on 16-bit digits since these fit conveniently
; in a fixnum; a 32-bit digit is split into 16-bit digits as outlined above.
; This creates a little bit of hairyness in the access pattern; this hair is
; localized in the procedures flagged below as "machine-dependent".
; In particular, the bytevector index of a 16-bit digit with logical index `i'
; is given by the formula
;
;    (+ (* i 2) (if (odd? i) 2 6))
;
; Machine-dependent procedures will later be recognized by the compiler and
; will have their code generated in-line; the machine dependent procedures
; can then go away.


; Here's the list of what procedures which are conceptually exported from
; this module. They do *not* check the types of their arguments!
;
; (export bignum-add 
;         bignum-subtract
;         bignum-multiply
;         bignum-quotient
;         bignum-remainder
;         bignum-divide
;         bignum-=
;         bignum-<=
;         bignum-<
;         bignum->=
;         bignum->
;         bignum-zero?
;         bignum-positive?
;         bignum-negative?
;         bignum->fixnum
;         fixnum->bignum
;         bignum->ratnum
;         ratnum->bignum
;         bignum->rectnum
;         rectnum->bignum
;         bignum->flonum
;         flonum->bignum
;         bignum->compnum
;         compnum->bignum)

;-----------------------------------------------------------------------------
; MACHINE-DEPENDENT STUFF, SOME OF WHICH GOES AWAY WHEN THE COMPILER IS 
; GOOD ENOUGH.

(define byte-base 256)                         ; range of a byte
(define bignum-base (* byte-base byte-base))   ; range of a bignum digit
(define negative-sign 1)                       ; the sign of a negative bignum
(define positive-sign 0)                       ; ditto of a positive one
(define smallest-positive-bignum (expt 2 30))
(define largest-negative-bignum (- (+ (expt 2 30) 1)))
(define bignum-digits-in-a-fixnum 2)

; `Bignum-ref' does zero-based referencing of a bignum structure, returning
; a 16-bit digit (adjusted to be a fixnum) from the bignum. 
; This procedure goes away when `big+' etc. are recognized by the compiler.
; Ditto for `bignum-set!'.

(define (bignum-ref a i)
  (let ((base (+ (* i 2) (if (odd? i) 2 6))))
    (+ (* byte-base (bytevector-ref a base))
       (bytevector-ref a (+ base 1)))))

(define (bignum-set! a i v)
  (let ((base (+ (* i 2) (if (odd? i) 2 6))))
    (bytevector-set! a base (quotient v byte-base))
    (bytevector-set! a (+ base 1) (remainder v byte-base))))

; Allocate a bignum given the count of 16-bit digits.

(define (bignum-alloc digits)
  (let* ((l (roundup4 (* digits 2)))
	 (v (make-bytevector (+ l 4) 0)))
    (bignum-length-set! v (quotient l 4))
    v))

; Return the number of 16-bit digits. We check if the high 16-bit digit of
; the high 32-bit digit is 0 (which it may validly be) and return length-1
; if so. The need for this is a result of the way 16-bit digits are mapped
; onto 32-bit digits (or vice versa...).

(define (bignum-length b)
  (let ((l (* 2 (+ (* byte-base (bytevector-ref b 2))
		   (bytevector-ref b 3)))))
    (if (zero? (bignum-ref (- l 1)))
	(- l 1)
	l)))

; Set the number of 16-bit digits. The number is converted to 32-bit digits,
; which may involve adding a 0 digit at the high end; see comments above.

(define (bignum-length-set! b l)
  (let ((l (quotient (roundup4 l) 4)))
    (bytevector-set! b 2 (quotient l byte-base))
    (bytevector-set! b 3 (remainder l byte-base))))

; Get the sign.

(define (bignum-sign b)
  (bytevector-ref b 1))

; Set the sign.

(define (bignum-sign-set! b s)
  (bytevector-set! b 1 s))

(define (roundup4 n)
  (* (quotient n 4) 4))

;-----------------------------------------------------------------------------
; MACHINE-INDEPENDENT STUFF WHICH GOES AWAY WHEN THE COMPILER IS GOOD ENOUGH
;
; `big+', `big-', and the others will later be generated directly by the 
; compiler and this (probably abysmally slow) code will go away.

; Given bignums `a', `b', and `c', a bignum index `i', and a carry digit,
; compute the sum of the ith digits of a and b, with the carry, and put
; that in the ith digit of c, returning the carry.
; The carry is always 1 or 0.

(define (big+ a b c i carry)
  (let ((r (+ (bignum-ref a i) (bignum-ref b i) carry)))
    (bignum-set! c i (remainder r bignum-base))
    (quotient r bignum-base)))

; Given ditto, compute c[i] = a[i] - b[i] - borrow, returning the new borrow.
; The borrow is always 0 or 1.

(define (big- a b c i borrow)
  (let ((r (- (bignum-ref a i) (bignum-ref b i) borrow)))
    (if (negative? r)
	(let ((r (+ r bignum-base)))
	  (bignum-set! c i (remainder r bignum-base))
	  1)
	(begin
	  (bignum-set! c i r)
	  0))))


;-----------------------------------------------------------------------------
; MACHINE-INDEPENDENT STUFF
;
; Normalization is done in the toplevel routines because that gives us the
; opportunity to fiddle the sign at will, knowing that what we're dealing
; with is a bignum.

; add two bignums, producing an integer.

(define (bignum-add a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (cond ((= sa sb)
	   (let ((c (big-add-digits a b)))
	     (bignum-sign-set! c sa)
	     (bignum-normalize c)))
	  ((sign-negative? sa)
	   (bignum-subtract b a))
	  (else
	   (bignum-subtract a b)))))


; subtract one bignum from another, producing an integer.

(define (bignum-subtract a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (if (= sa sb)
		 (big-subtract-digits a b)
		 (big-add-digits a b))))
      (if (sign-negative? sa)
	  (flip-sign! c))
      (bignum-normalize c))))


; multiply two bignums, producing an integer.

(define (bignum-multiply a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (big-multiply-digits a b)))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (bignum-normalize c))))


; divide two bignums, returning the quotient

(define (bignum-quotient a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (car (big-divide-digits a b))))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (bignum-normalize c))))


; divide two bignums, returning the remainder

(define (bignum-remainder a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (cdr (big-divide-digits a b))))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (bignum-normalize c))))


; divide two bignums, returning a bignum if the remainder is 1, and otherwise
; a ratnum.

(define (bignum-divide a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (big-divide-digits a b)))
      (if (not (= sa sb))
	  (bignum-sign-set! (car c) negative-sign))
      (make-rat (bignum-normalize (car c)) (bignum-normalize (cdr c))))))


(define (bignum-= a b)
  (zero? (big-compare a b)))

(define (bignum-<= a b)
  (<= (big-compare a b) 0))

(define (bignum-< a b)
  (< (big-compare a b) 0))

(define (bignum->= a b)
  (>= (big-compare a b) 0))

(define (bignum-> a b)
  (> (big-compare a b) 0))

(define (bignum-zero? b)
  (zero? (bignum-length b)))

(define (bignum-negative? b)
  (and (sign-negative? (bignum-sign b))
       (not (zero? (bignum-length b)))))

(define (bignum-positive? b)
  (and (sign-positive? (bignum-sign b))
       (not (zero? (bignum-length b)))))

; Assumes the bignum fits in a fixnum...

(define (bignum->fixnum b)
  (let loop ((i (- (bignum-length b) 1)) (n 0))
    (if (negative? i)
	(if (sign-negative? (bignum-sign b))
	    (- n)
	    n)
	(loop (- i 1) (+ (* bignum-base n) (bignum-ref b i))))))

; can't use bignum-normalize because it'd convert it back to a fixnum...

(define (fixnum->bignum b)
  (let ((b (bignum-alloc bignum-digits-in-a-fixnum)))
    (let loop ((i 0) (n (abs f)))
      (if (zero? n)
	  (begin (bignum-length-set! b i)
		 (if (negative? f)
		     (bignum-sign-set! b negative-sign))
		 b)
	  (begin (bignum-set! b i (remainder n bignum-base))
		 (loop (+ i 1) (quotient n bignum-base)))))))

  
;-----------------------------------------------------------------------------
; Helpers

(define (sign-negative? sign)
  (= sign negative-sign))

(define (sign-positive? sign)
  (= sign positive-sign))

(define (flip-sign! b)
  (if (sign-negative? (bignum-sign b))
      (bignum-set-sign! b negative-sign)
      (bignum-set-sign! b positive-sign)))


; add the digits of two bignums, producing a third, positive, bignum.

(define (big-add-digits a b)
  (let* ((la   (bignum-length a))
	 (lb   (bignum-length b))
	 (lmax (max la lb))
	 (lmin (min la lb))
	 (c    (bignum-alloc (+ lmax 1))))

      ; add common segments

      (let loop ((i 0) (carry 0))
	(if (< i lmin)
	    (loop (+ i 1) (big+ a b c i carry))
	    
	    ; add carry thru longest number

	    (let ((rest (if (= i la) b a)))
	      (let loop ((i i) (carry carry))
		(if (< i lmax)
		    (loop (+ i 1) (big+ a b c i carry))
		    (begin (bignum-set! c i carry)
			   c))))))))


; subtract the digits of bignum b from the digits of bignum a, producing 
; a third, possibly negative, bignum c.

(define (big-subtract-digits a b)
  (let* ((la   (bignum-length a))
	 (lb   (bignum-length b))
	 (lmax (max la lb))
	 (lmin (min la lb))
	 (c    (bignum-alloc (+ lmax 1))))     ; are you sure?

    ; subtract common segments

    (let loop ((i 0) (borrow 0))
      (if (< i lmin)
	  (loop (+ i 1) (big- a b c i borrow))
	  
	  ; subtract borrow through longest number

	  (let ((rest (if (= i la) b a)))
	    (let loop ((i i) (borrow borrow))
	      (if (< i lmax)
		  (loop (+ i 1) (big- a b c i borrow))
		  (begin (if (not (zero? borrow))
			     (bignum-sign-set! c negative-sign))
			 c))))))))


; multiply the digits of two positive bignums, producing a third bignum.

(define (big-multiply-digits a b)
  '())


; divide two positive bignums, producing a pair, both elements of which are 
; bignums, the car being the quotient and the cdr being the remainder.
; (btw, we're computing a / b here, not b / a, in case there was a doubt...)

(define (big-divide-digits a b)
  '())


; normalize a bignum -- this involves removing leading zeroes, and, if the
; number is small enough to fit in a fixnum, converting it to a fixum.

(define (bignum-normalize! b)
  (let loop ((i (- (bignum-length b) 1)))
    (cond ((negative? i)
	   0)
	  ((zero? (bignum-ref b i))
	   (loop (- i 1)))
	  (else
	   (bignum-length-set! b (+ i 1))
	   (if (and (bignum-> b largest-negative-fixnum)
		    (bignum-< b smallest-positive-fixnum))
	       (bignum->fixnum b)
	       b)))))

