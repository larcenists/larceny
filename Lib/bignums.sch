; -*- Scheme -*-
;
; Scheme 313 runtime system
; Scheme code for bignum arithmetic.
;
; $Id: bignums.scm,v 1.11 1992/05/15 22:17:56 lth Exp $
;
; This file has four sections:
;
; The first section contains bignum creators, accessors, and mutators,
; which are machine-dependent in the sense that they `know' the
; endianness of a particular architecture and the layout of a bignum;
; bignums, as seen by these routines, are implemented in terms of bytevectors.
; At some point we probably want the compiler to know about these, but it is
; not vital for performance -- these procedures are called mostly from
; the procedures in section 2.
;
; The second section contains machine-independent procedures which do
; basic operations on parts of bignums, like adding two digits and a
; carry to produce a third digit and a carry. At some level of sophistication,
; the compiler will replace calls to these procedures with an optimized
; sequence of machine instructions at the point of the call.
;
; The third section contains the basic bignum operations as viewed from
; the outside of this module; the procedures defined in this section are
; listed in the `export' list below.
;
; The fourth section contains helper procedures for the procedures in
; section 3.
;
; Logically, a bignum consists of a sign, a digit count, and a number of
; digits. The representation of the sign is abstracted in the variables
; `negative-sign' and `positive-sign'; the signs are fixnum quantities.
; The size (and hence base) of each digit is abstracted in the variable
; `bignum-base'. 
;
; Internally, bignums are created using `bignum-alloc', which allocates
; a bignum of the requested number of digits with a positive sign and a
; value of all-0-digits. The length is extracted using `bignum-length',
; and the sign ditto using `bignum-sign'. The sign can be set using
; `bignum-sign-set!', and the length can be set (and hence bignums can be
; truncated) using `bignum-length-set!'. It is an error to extend a bignum
; beyond its allocated space. Bignum digits are accessed using `bignum-ref'
; and set using `bignum-set!'.
;
; The implementation uses the "classical" algorithms from Knuth, vol II, 2nd
; edition, section 4.3.1, and was also inspired by the techniques discussed
; in a paper by Jon L White: "Reconfigurable, Retargetable Bignums: A Case
; Study in Efficient, Portable Lisp System Building", Proceedings of the ACM
; conference on Lisp & FP, 1986.

; Here's the list of what procedures which are conceptually exported from
; this module. They do *not* check the types of their arguments!
;
; (export bignum-add 
;         bignum-subtract
;         bignum-multiply
;         bignum-quotient
;         bignum-remainder
;         bignum-divide
;         bignum-negate
;         bignum-abs
;         bignum=?
;         bignum<=?
;         bignum<?
;         bignum>=?
;         bignum>?
;         bignum-zero?
;         bignum-positive?
;         bignum-negative?
;         bignum-even?
;         bignum-odd?
;         bignum->fixnum
;         fixnum->bignum
;         bignum->string
;         bignum?)


;-----------------------------------------------------------------------------
; Section 1. All the world's a Sparc.
;
; MACHINE-DEPENDENT STUFF, SOME OF WHICH GOES AWAY WHEN THE COMPILER IS 
; GOOD ENOUGH.
;
; The procedures in Section 1 work on a 32-bit-word, big-endian architecture,
; of which the Sparc is one example. On such an architecture, the layout of
; a bignum is this:
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

(define byte-base 256)                         ; range of a byte
(define bignum-base (* byte-base byte-base))   ; range of a bignum digit

(define negative-sign 1)                       ; the sign of a negative bignum
(define positive-sign 0)                       ; ditto of a positive one
(define bignum-digits-in-a-fixnum 2)
(define max-bignum-bytes (* 65535 4))         ; a lot of digits
(define bits-per-bigit 16)

; Some parameters, which have to be hardcoded to avoid using bignum
; arithmetic at load time (sigh).

; (define smallest-positive-bignum (expt 2 29))
; (define largest-negative-bignum (- (+ (expt 2 29) 1)))

(define smallest-positive-bignum 536870912)
(define largest-negative-bignum -536870913)

; `Bignum-ref' does zero-based referencing of a bignum structure, returning
; a 16-bit digit (adjusted to be a fixnum) from the bignum. 

(define (bignum-ref a i)
  (let ((base (+ (* i 2) (if (odd? i) 2 6))))
    (+ (* byte-base (bytevector-like-ref a base))
       (bytevector-like-ref a (+ base 1)))))


; Ditto for `bignum-set!'.

(define (bignum-set! a i v)
  (let ((base (+ (* i 2) (if (odd? i) 2 6))))
    (bytevector-like-set! a base (quotient v byte-base))
    (bytevector-like-set! a (+ base 1) (remainder v byte-base))))


; Allocate a bignum given the count of 16-bit digits.

(define (bignum-alloc digits)
  (let ((l (roundup4 (* digits 2))))
    (if (> l max-bignum-bytes)
	(error 'generic-arithmetic "Bignum too large.")
	(let ((v (make-bytevector (+ l 4))))
	  (bytevector-fill! v 0)
          (typetag-set! v sys$tag.bignum-typetag)
	  (bignum-length-set! v (+ (quotient l 2) (remainder l 2)))
	  v))))


; Return the number of 16-bit digits. We check if the high 16-bit digit of
; the high 32-bit digit is 0 (which it may validly be) and return length-1
; if so. The need for this is a result of the way 16-bit digits are mapped
; onto 32-bit digits (or vice versa...).

(define (bignum-length b)
  (let ((l (* 2 (+ (* byte-base (bytevector-like-ref b 2))
		   (bytevector-like-ref b 3)))))
    (cond ((zero? l) l)
	  ((zero? (bignum-ref b (- l 1))) (- l 1))
	  (else l))))


; Set the number of 16-bit digits. The number is converted to 32-bit digits,
; which may involve adding a 0 digit at the high end; see comments above.
;
; `l' is the number of 16-bit digits. To get the number of 32-bit digits,
; we must round up to an even number, then divide by 2. This is equivalent
; to adding 1 and dividing by 2.

(define (bignum-length-set! b l)
  (let ((l (quotient (+ l 1) 2)))
    (bytevector-like-set! b 2 (quotient l byte-base))
    (bytevector-like-set! b 3 (remainder l byte-base))))


; Get the sign.

(define (bignum-sign b)
  (bytevector-like-ref b 1))


; Set the sign.

(define (bignum-sign-set! b s)
  (bytevector-like-set! b 1 s))


; Copy. Should be expressed in terms of bytevector-like-copy. FIXME.

(define (big-copy! from to)
  (let loop ((i (- (bignum-length from) 1)))
    (if (>= i 0)
	(begin (bignum-set! to i (bignum-ref from i))
	       (loop (- i 1))))))

; eventually a primop

(define (bignum? x)
  (and (bytevector-like? x)
       (= (typetag x) sys$tag.bignum-typetag)))

; misc

(define (roundup4 n)
  (* (quotient (+ n 3) 4) 4))


;-----------------------------------------------------------------------------
; Section 2.
;
; MACHINE-INDEPENDENT STUFF WHICH GOES AWAY WHEN THE COMPILER IS GOOD ENOUGH
;
; These procedures will later be generated directly by the compiler and this
; (probably abysmally slow, but portable) code will go away.

; ADDITION

; Given bignums `a', `b', and `c', a bignum index `i', and a carry digit,
; compute the sum of the ith digits of a and b, with the carry, and put
; that in the ith digit of c, returning the carry.
; The carry is always 1 or 0.

(define (big2+ a b c i carry)
  (let ((r (+ (bignum-ref a i) (bignum-ref b i) carry)))
    (bignum-set! c i (remainder r bignum-base))
    (quotient r bignum-base)))


; Special case: carry propagation.

(define (big1+ a c i carry)
  (let ((r (+ (bignum-ref a i) carry)))
    (bignum-set! c i (remainder r bignum-base))
    (quotient r bignum-base)))


; SUBTRACTION

; Given ditto, compute c[i] = a[i] - b[i] - borrow, returning the new borrow.
; The borrow is always 0 or 1.

(define (big2- a b c i borrow)
  (let ((r (- (bignum-ref a i) (bignum-ref b i) borrow)))
    (bignum-set! c i (remainder (+ r bignum-base) bignum-base))
    (if (negative? r) 1 0)))


; Special case: borrow propagation.

(define (big1- a c i borrow)
  (let ((r (- (bignum-ref a i) borrow)))
    (bignum-set! c i (remainder (+ r bignum-base) bignum-base))
    (if (negative? r) 1 0)))


; MULTIPLICATION

; Given bignums a, b, c, indices i, j, and a carry, compute
;   c[ i+j ] = (a[i]*b[j]+c[i+j]+carry) mod bignum-base
; and return (a[i]*b[j]+c[i+j]+carry) div bignum-base
;
; Since 16bits * 16bits -> 32bits, the straight Scheme implementation of
; this procedure may overflow the fixnum and invoke the bignum routines
; recursively (unless caught by the daisy-chain in the millicode, which will
; happen in the Sparc implementation). This is ugly, but well-defined.
; The machine-dependent implementation of this procedure will be able to do
; significantly better.
;
; If the intermediate result is a bignum, then it has two or three bigits.

(define (big2*+ a b c i j carry)

  (define (rem r)
    (if (fixnum? r) (remainder r bignum-base) (bignum-ref r 0)))

  (define (quo r)
    (if (fixnum? r)
	(quotient r bignum-base)
	(cond ((= (bignum-length r) 2)
	       (bignum-ref r 1))
	      ((= (bignum-length r) 3)
	       (+ (* (bignum-ref r 2) bignum-base) (bignum-ref r 1)))
	      (else
	       (fixnum->bignum 0)))))

  (let ((p (* (bignum-ref a i) (bignum-ref b j))))
    (let ((r (+ p (bignum-ref c (+ i j)) carry)))
      (bignum-set! c (+ i j) (rem r))
      (quo r))))

; This is potentially faster, but "machine-dependent"; for benchmarking only.
; It should pay off if procedure calls are expensive.
;
; (define (big2*+ a b c i j carry)
;   (let ((i+j (+ i j)))
;     (let ((base1 (+ (* i 2) (if (odd? i) 2 6)))
; 	  (base2 (+ (* j 2) (if (odd? j) 2 6)))
; 	  (base3 (+ (* i+j 2) (if (odd? i+j) 2 6))))
;       (let ((adigit (+ (* byte-base (bytevector-like-ref a base1))
; 		       (bytevector-like-ref a (+ base1 1))))
; 	    (bdigit (+ (* byte-base (bytevector-like-ref b base2))
; 		       (bytevector-like-ref b (+ base2 1))))
; 	    (cdigit (+ (* byte-base (bytevector-like-ref c base3))
; 		       (bytevector-like-ref c (+ base3 1)))))
; 	(let ((r (+ (* adigit bdigit) cdigit carry)))
; 	  (let ((r (remainder r bignum-base)))
; 	    (bytevector-like-set! c base3 (quotient r byte-base))
; 	    (bytevector-like-set! c (+ base3 1) (remainder r byte-base)))
; 	  (quotient r bignum-base))))))


; DIVISION

; The following are used in implementing bignum division. All references
; are to Algorithm D in Knuth vol II, 2nd ed, pp 257-258.

; Step D3: calculate an approximation to q_j, then adjust it if necessary.

(define (big~q u v j)
  (let* ((lv   (bignum-length v))
	 (v1   (bignum-ref v (- lv 1)))
	 (v2   (bignum-ref v (- lv 2)))
	 (uj   (bignum-ref u j))
	 (uj+1 (bignum-ref u (- j 1)))
	 (uj+2 (bignum-ref u (- j 2))))

    (define (toobig? ~q)
      (let ((a (* ~q v2))
	    (b (+ (* (- (+ (* uj bignum-base) uj+1) (* ~q v1)) bignum-base)
		  uj+2)))
	(> a b)))

    (define (approximate)
      (if (= uj v1)
	  (- bignum-base 1)
	  (quotient (+ (* uj bignum-base) uj+1)
		    v1)))

    (let loop2 ((~q (approximate)))
      (display "foo")
      (if (toobig? ~q)
	  (loop2 (- ~q 1))
	  (begin (display "fum")
		 ~q)))))


; step D4: multiply and subtract, returning borrow

(define (big*- u v j ~q)
  (let ((newv (big-multiply-through-by v ~q)))
    (let loop ((i (- j (bignum-length newv))) (k 0) (borrow 0))
      (if (<= i j)
	  (let ((r (- (bignum-ref u i) (bignum-ref newv k) borrow)))
	    (bignum-set! u i (remainder (+ r bignum-base) bignum-base))
	    (if (negative? r)
		(loop (+ i 1) (+ k 1) 1)
		(loop (+ i 1) (+ k 1) 0)))
	  borrow))))


; step D6: add back

(define (big-addback u v j)
  (let loop ((i (- j (+ (bignum-length v) 1))) (k 0) (carry 0))
    (cond ((< i j)
	   (let ((r (+ (bignum-ref u i) (bignum-ref v k) carry)))
	     (bignum-set! u (remainder r bignum-base))
	     (loop (+ i 1) (+ k 1) (quotient r bignum-base))))
	  ((= i j)
	   (let ((r (+ (bignum-ref u i) carry)))
	     (bignum-set! u (remainder r bignum-base))))
	  (else
	   '()))))

; Mutliply through by a fixnum.
; Must normalize here, since division algorithm depends on the high digit
; being non-zero (take it from someone who knows...)
; Also, it is not safe to use the standard normalizer, because the number
; might fit in a fixnum, something we cannot afford.

(define (big-multiply-through-by b f)
  (let* ((l (bignum-length b))
	 (q (bignum-alloc (+ l 1))))
    (let loop ((i 0) (carry 0))
      (if (< i l)
	  (let ((r (+ (* (bignum-ref b i) f) carry)))
	    (bignum-set! q i (remainder r bignum-base))
	    (loop (+ i 1) (quotient r bignum-base)))
	  (begin (bignum-set! q i carry)
		 (big-limited-normalize! q))))))


; copy bignum with an extra 0 as the most significant digit.

(define (big-extend-with-zero b)
  (let ((c (bignum-alloc (+ (bignum-length b) 1))))
    (big-copy! b c)
    (bignum-sign-set! c (bignum-sign b))
    c))


; Copy a bignum

(define (bignum-copy b)
  (let ((c (bignum-alloc (bignum-length b))))
    (big-copy! b c)
    (bignum-sign-set! c (bignum-sign b))
    c))


;-----------------------------------------------------------------------------
; Section 3.
;
; MACHINE-INDEPENDENT STUFF
;
; Normalization is done in the toplevel routines because that gives us the
; opportunity to fiddle the sign at will, knowing that what we're dealing
; with is a bignum.

; Add two bignums, producing an integer.
;       a + b  ==   a + b
;       a + -b ==   a - b
;      -a + b  == -(a - b)
;      -a + -b == -(a + b)

(define (bignum-add a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (if (= sa sb)
		 (big-add-digits a b)
		 (big-subtract-digits a b))))
      (if (sign-negative? sa)
	  (flip-sign! c))
      (big-normalize! c))))


; Subtract bignum `b' from bignum `a', producing an integer.
;       a - b  ==   a - b
;       a - -b ==   a + b
;      -a - b  == -(a + b)
;      -a - -b == -(a - b)

(define (bignum-subtract a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (if (= sa sb)
		 (big-subtract-digits a b)
		 (big-add-digits a b))))
      (if (sign-negative? sa)
	  (flip-sign! c))
      (big-normalize! c))))


; Multiply two bignums, producing an integer.

(define (bignum-multiply a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (big-multiply-digits a b)))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (big-normalize! c))))


; Divide bignum `a' by bignum `b', returning the quotient.

(define (bignum-quotient a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (car (big-divide-digits a b))))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (big-normalize! c))))


; Divide bignum `a' by bignum `b', returning the remainder.
;
; This is wrong; handling of the sign needs to be different, and if the
; sign of the remainder is negative, then the remainder needs to be
; adjusted.

(define (bignum-remainder a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (cdr (big-divide-digits a b))))
      (if (not (= sa sb))
	  (bignum-sign-set! c negative-sign))
      (big-normalize! c))))


; Divide bignum `a' by bignum `b', returning an integer if the remainder is 1,
; and a ratnum otherwise.

(define (bignum-divide a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let ((c (big-divide-digits a b)))
      (if (not (= sa sb))
	  (bignum-sign-set! (car c) negative-sign))
      (make-reduced-ratnum (big-normalize! (car c))
			   (big-normalize! (cdr c))))))

; Return the negation of the argument (a new bignum).

(define (bignum-negate a)
  (let ((b (bignum-copy a)))
    (flip-sign! b)
    b))

; relational operators

(define (bignum=? a b)
  (zero? (big-compare a b)))

(define (bignum<=? a b)
  (<= (big-compare a b) 0))

(define (bignum<? a b)
  (< (big-compare a b) 0))

(define (bignum>=? a b)
  (>= (big-compare a b) 0))

(define (bignum>? a b)
  (> (big-compare a b) 0))

; unary predicates

(define (bignum-zero? b)
  (zero? (bignum-length b)))

(define (bignum-negative? b)
  (and (sign-negative? (bignum-sign b))
       (not (zero? (bignum-length b)))))

(define (bignum-positive? b)
  (and (sign-positive? (bignum-sign b))
       (not (zero? (bignum-length b)))))

(define (bignum-even? b)
  (or (bignum-zero? b)
      (even? (bignum-ref b 0))))

(define (bignum-odd? b)
  (and (not (bignum-zero? b))
       (odd? (bignum-ref b 0))))

(define (bignum-abs b)
  (if (bignum-negative? b)
      (bignum-negate b)
      b))

; Coercions

; Assumes the bignum fits in a fixnum.

(define (bignum->fixnum b)
  (let loop ((i (- (bignum-length b) 1)) (n 0))
    (if (negative? i)
	(begin (if (not (fixnum? n))
		   (error "bignum->fixnum: disaster."))
	       (if (sign-negative? (bignum-sign b))
		   (- n)
		   n))
	(loop (- i 1) (+ (* bignum-base n) (bignum-ref b i))))))

; Can't use `big-normalize!' because it'd convert it back to a fixnum.
; (Could use big-limited-normalize, though.)

(define (fixnum->bignum f)
  (if (not (fixnum? f))
      (error "fixnum->bignum:" f "not a fixnum."))
  (let ((b (bignum-alloc bignum-digits-in-a-fixnum)))
    (let loop ((i 0) (n (abs f)))
      (if (zero? n)
	  (begin (bignum-length-set! b i)
		 (if (negative? f)
		     (bignum-sign-set! b negative-sign))
		 b)
	  (begin (bignum-set! b i (remainder n bignum-base))
		 (loop (+ i 1) (quotient n bignum-base)))))))

; Takes a bignum and a radix and returns the string which is the printable
; representation of the bignum in that radix.
;
; Uses brute force with extreme prejudice.
;
; Note that the use of big-divide-digits guarantees that the resulting values
; are bignums regardless of magnitude.

(define (bignum->string b r)
  (if (bignum-zero? b)
      "0"
      (let ((r (fixnum->bignum r))
	    (d "0123456789abcdef")
	    (s (bignum-negative? b)))
	(let loop ((b (bignum-abs b)) (l '()))
	  (if (bignum-zero? b)
	      (if s
		  (list->string (cons #\- l))
		  (list->string l))
	      (let ((tmp (big-divide-digits b r)))
		(loop (car tmp)
		      (cons (string-ref d (bignum->fixnum (cdr tmp))) l))))))))


;-----------------------------------------------------------------------------
; Section 4.
;
; Helpers.

(define (sign-negative? sign)
  (= sign negative-sign))

(define (sign-positive? sign)
  (= sign positive-sign))

(define (flip-sign! b)
  (if (sign-negative? (bignum-sign b))
      (bignum-sign-set! b positive-sign)
      (bignum-sign-set! b negative-sign)))


; Add the digits of two bignums, producing a third, positive, bignum.

(define (big-add-digits a b)
  (let* ((la   (bignum-length a))
	 (lb   (bignum-length b))
	 (lmax (max la lb))
	 (lmin (min la lb))
	 (c    (bignum-alloc (+ lmax 1))))

    ;; add common segments

;    (bigdump a)
;    (bigdump b)
;    (display "-----------------") (newline)
    (let loop ((i 0) (carry 0))
;      (bigdump c)
      (if (< i lmin)
	  (loop (+ i 1) (big2+ a b c i carry))
	    
	  ;; add carry thru longest number

	  (let ((rest (if (= i la) b a)))
	    (let loop ((i i) (carry carry))
;	      (bigdump c)
	      (if (< i lmax)
		  (loop (+ i 1) (big1+ rest c i carry))
		  (begin (bignum-set! c i carry)
;			 (bigdump c)
			 c))))))))


; Subtract the digits of bignum b from the digits of bignum a, producing 
; a third, possibly negative, bignum c.

(define (big-subtract-digits a b)
  (let ((x (big-compare-digits a b)))
    (let ((a (if (negative? x) b a))
	  (b (if (negative? x) a b)))
      (let* ((la   (bignum-length a))
	     (lb   (bignum-length b))
	     (lmax (max la lb))
	     (lmin (min la lb))
	     (c    (bignum-alloc (+ lmax 1)))) ; are you sure?

	; subtract common segments

	(let loop ((i 0) (borrow 0))
	  (if (< i lmin)
	      (loop (+ i 1) (big2- a b c i borrow))
	  
	      ; subtract borrow through longest number

	      (let ((rest (if (= i la) b a)))
		(let loop ((i i) (borrow borrow))
		  (if (< i lmax)
		      (loop (+ i 1) (big1- rest c i borrow))
		      (begin (if (negative? x)
				 (flip-sign! c))
			     c))))))))))


; Multiply the digits of two positive bignums, producing a third,
; positive, bignum.

(define (big-multiply-digits a b)
  (let* ((la (bignum-length a))
	 (lb (bignum-length b))
	 (c  (bignum-alloc (+ la lb))))
;    (bigdump a)
;    (bigdump b)
    (let loop1 ((ai 0))
      (if (< ai la)
	  (let loop2 ((bi 0) (carry 0))
;	    (bigdump c)
	    (if (< bi lb)
		(loop2 (+ bi 1) (big2*+ a b c ai bi carry))
		(begin (bignum-set! c (+ ai bi) carry)
;		       (bigdump c)
		       (loop1 (+ ai 1)))))
	  c))))


; Divide two positive bignums, producing a pair, both elements of which are 
; bignums, the car being the quotient and the cdr being the remainder.
;
; See Knuth vol II, 2nd ed, p 257,258. There's extra hair here, however,
; due to the different indexing scheme.

(define (big-divide-digits a b)

  ; `a' is a bignum, `b' is a fixnum. Produces a pair of bignums, even if the
  ; remainder is always a fixnum.
  ;
  ; A bit of hair associated with dowing the quotient in here.
  ; The problem is that 'rem' can be as large as (- b 1), and b can be
  ; as large as bignum-base. This, in turn, can make 'd' a bignum.
  ; This means that we can get into a loop trying to calculate (quotient d b).
  ;
  ; This gets *much* simpler once we move into assembly.

  (define (fast-divide a b)

    ; Turns out that if you have a two-bigit bignum and a single-bigit
    ; fixnum, then the following will calculate the quotient.

    (define (b2fquot d b)
      (let ((q1 (quotient bignum-base b))
	    (q2 (quotient (bignum-ref d 0) b)))
	(let ((r1 (- bignum-base (* q1 b)))
	      (r2 (- (bignum-ref d 0) (* q2 b))))
	  (+ (* (bignum-ref d 1) q1) q2 (if (>= (+ r1 r2) b) 1 0)))))

    (let ((q (bignum-alloc (bignum-length a))))
      (let loop ((rem 0) (i (- (bignum-length a) 1)))
	(if (>= i 0)
	    (let* ((d (+ (* rem bignum-base) (bignum-ref a i)))
		   (quot (if (fixnum? d) 
			     (quotient d b)
			     (b2fquot d b)))
		   (remd (- d (* quot b))))
	      (bignum-set! q i quot)
	      (loop remd (- i 1)))
	    (begin (big-limited-normalize! q)
		   (cons q (fixnum->bignum rem)))))))

  ; `a' and `b' are both bignums, with (length a) >= (length b) and
  ; (length b) > 1. Produces a pair of bignums.
  ;
  ; May need to normalize? FIXME.

  (define (slow-divide a b)
    (let* ((d  (quotient bignum-base
			 (+ (bignum-ref b (- (bignum-length b) 1)) 1)))
	   (u  (if (= 1 d)
		   (big-extend-with-zero a)
		   (big-multiply-through-by a d)))
	   (v  (big-multiply-through-by b d))
	   (lu (bignum-length u))
	   (lv (bignum-length v))
	   (q  (bignum-alloc (- lu lv)))
	   (lq (- lu lv)))                ; don't use (bignum-length q)
      (let loop1 ((j (- lu 1)) (p (- lq 1)))
	(if (>= p 0)
	    (let ((~q (big~q u v j)))
	      (let ((borrow (big*- u v j ~q)))
		(bignum-set! q p ~q)
		(if (not (zero? borrow))
		    (begin (bignum-set! q p (- (bignum-ref q p) 1))
			   (big-addback u v j)))
		(loop1 (- j 1) (- p 1))))
	    (cons q (car (fast-divide u d)))))))  ; wrong U

  ; maintain some invariants and catch the easy cases.

  (cond ((bignum-zero? b)
	 (error 'generic-arithmetic "Bignum division by zero"))
	((bignum-zero? a)
	 (cons (fixnum->bignum 0) (fixnum->bignum 0)))
	(else
	 (let ((la (bignum-length a))
	       (lb (bignum-length b)))
	   (cond ((> lb la)
		  (let ((r (bignum-copy b)))
		    (bignum-sign-set! r positive-sign)
		    (cons (fixnum->bignum 0) r)))
		 ((= lb 1)
		  (fast-divide a (bignum-ref b 0)))
		 (else
		  (slow-divide a b)))))))


; Compare two bignums, and return 0 if they are equal, a negative number if
; the first is less than the second, and a positive number if the first is
; greater than the second.

(define (big-compare a b)
  (let* ((la (bignum-length a))
	 (lb (bignum-length b))
	 (sa (if (zero? la) positive-sign (bignum-sign a)))
	 (sb (if (zero? lb) positive-sign (bignum-sign b))))
    (cond ((not (= sa sb))
	   (if (sign-negative? sa)
	       -1
	       1))
	  (else
	   (if (sign-negative? sa)
	       (- (big-compare-digits a b))
	       (big-compare-digits a b))))))

(define (big-compare-digits a b)
  (let ((la (bignum-length a))
	(lb (bignum-length b)))
    (if (not (= la lb))
	(- la lb)
	(let loop ((i (- la 1)))
	  (cond ((< i 0)
		 0)
		((= (bignum-ref a i) (bignum-ref b i))
		 (loop (- i 1)))
		(else
		 (- (bignum-ref a i) (bignum-ref b i))))))))

    
; Normalize a bignum -- this involves removing leading zeroes, and, if the
; number is small enough to fit in a fixnum, converting it to a fixum.

(define (big-normalize! b)
  (let loop ((i (- (bignum-length b) 1)))
    (cond ((negative? i)
	   0)
	  ((zero? (bignum-ref b i))
	   (loop (- i 1)))
	  (else
	   (bignum-length-set! b (+ i 1))
	   (if (and (bignum>? b largest-negative-bignum)
		    (bignum<? b smallest-positive-bignum))
	       (bignum->fixnum b)
	       b)))))

(define (big-limited-normalize! b)
  (let loop ((i (- (bignum-length b) 1)))
    (cond ((negative? i)
	   (bignum-length-set! b 0)
	   b)
	  ((zero? (bignum-ref b i))
	   (loop (- i 1)))
	  (else
	   (bignum-length-set! b (+ i 1))
	   b))))

; For debugging under Chez.
;
;(display "; redefining debugging values") (newline)
;(set! smallest-positive-bignum
;  (integer->bytevector smallest-positive-bignum))
;(set! largest-negative-bignum
;  (integer->bytevector largest-negative-bignum))

; For debugging in general

(define (bigdump bignum)

  (define (pr n)
    (let ((s (number->string n 16)))
      (let loop ((i (- 4 (string-length s))))
	(if (> i 0)
	    (begin (display "0")
		   (loop (- i 1)))
	    (display s)))))

  (if dump-is-on
      (let loop ((i (- (bignum-length bignum) 1)))
	(if (>= i 0)
	    (begin (pr (bignum-ref bignum i))
		   (loop (- i 1)))
	    (newline)))))

(define dump-is-on #f)

(define (dumpon)
  (set! dump-is-on #t))

(define (dumpoff)
  (set! dump-is-on #f))

