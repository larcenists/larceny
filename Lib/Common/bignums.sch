; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Scheme code for bignum arithmetic.
;
; The bignum code consists of four sections.
;
; The first section contains bignum creators, accessors, and mutators.
; These procedures depend on the byte-level layout of a bignum, in 
; particular on word size and endianness.  Therefore, the procedures
; are broken out into two other files: bignums-be.sch and bignums-el.sch.
;
; The second section contains machine-independent procedures that perform
; basic operations on parts of bignums, like adding two digits and a
; carry to produce a third digit and a carry.  These procedures are to
; be replaced by primitives at some point.
;
; The third section contains the basic bignum operations as viewed from
; the outside of this module; the procedures defined in this section are
; listed in the `export' list below.
;
; The fourth section contains helper procedures for the procedures in
; section 3.
;
; Representation.
;
; A bignum consists of a sign, a digit count, and a number of digits.
; The representation of the sign is abstracted in the variables
; `negative-sign' and `positive-sign'; the signs are fixnum quantities.
; The size (and hence base) of each digit is abstracted in the variable
; `bignum-base'. 
;
; Implementation.
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
;
; Invariants.
;
; Knuth describes three primitive operations, named a0, b0, and c0:
;
;   a0 - addition or subtraction of one-place integers, giving a 
;        one-place answer and a carry
;   b0 - multiplication of a one-place integer by another one-place
;        integer, giving a two-place answer
;   c0 - division of a two-place integer by a one-place integer, provided
;        that the quotient is a one-place integer, and yielding also a 
;        one-place remainder.
;     
; These must be implemented by the assembler or the millicode, i.e.,
; if the representation of a bigit uses 16 bits, then a 16-by-16
; multiplication yielding a more-than-29-bit number (i.e., a bignum)
; must be created as a bignum by the primitive * operation without 
; entering the bignum code to do this, and division by a 2-bigit bignum 
; by a fixnum must also be transparently handled.

($$trace "bignums")

; Here is the list of procedures that are exported by this module. 
;
;  bignum-add, bignum-subtract, bignum-multiply, bignum-quotient,
;  bignum-remainder, bignum-divide, bignum-negate, bignum-abs,
;  bignum=?, bignum<=?, bignum<?, bignum>=?, bignum>?, bignum-zero?,
;  bignum-positive?, bignum-negative?, bignum-even?, bignum-odd?,
;  bignum->fixnum, fixnum->bignum, bignum->string, bignum?


;---------------------------------------------------------------------------
; Section 0. System-independent constants (not really).

(define smallest-positive-bignum 536870912)  ; 2^29
(define largest-negative-bignum -536870913)  ; -(2^29+1)
(define max-negative-fixnum     -536870912)  ; -(2^29)
(define max-positive-fixnum      536870911)  ; 2^29-1

(define negative-sign 1)                     ; the sign of a negative bignum
(define positive-sign 0)                     ; ditto of a positive one

(define max-bignum-bytes (* 65535 4))        ; a lot of digits


;-----------------------------------------------------------------------------
; Section 1. 
;
; Representation-dependent stuff.  
;
; Most of this section is defined in bignums-be.sch and bignums-el.sch.  
; Procedures and variables defined there are:
;
;    bignum-base
;    bignum-base/2
;    bigit-mask
;    bytes-per-bigit
;    bits-per-bigit
;    bigit-shift
;    bigits-per-fixnum
;
;    (bignum-sign bignum)  =>  fixnum
;    (bignum-sign-set! bignum fixnum)
;    (bignum-ref bignum idx) => fixnum
;    (bignum-set! bignum idx fixnum)
;    (bignum-length bignum) => idx
;    (bignum-length-set! bignum idx)
;    (bignum-truncate-length! bignum length)
;    (big-fits-in-fix? bignum) => boolean

; The following procedures depend only on knowning that a bignum is
; bytevector-like, so they are not factored out.

; Copy data from one bignum into another, terminating when either
; the source or destination has been exhausted.

(define (big-copy-into! from to)
  (do ((i (- (min (bytevector-like-length from) (bytevector-like-length to))
	     1)
	  (- i 1)))
      ((< i 0))
    (bytevector-like-set! to i (bytevector-like-ref from i))))

; Is it a bignum?

(define (bignum? x)
  (and (bytevector-like? x)
       (= (typetag x) sys$tag.bignum-typetag)))

; Allocate a bignum

(define (bignum-alloc bigits)

  (define (roundup4 n)
    (logand (+ n 3) (lognot 3)))

  (let ((l (roundup4 (* bigits bytes-per-bigit))))
    (if (> l max-bignum-bytes)
	(begin (error "Bignum too large: " bigits " bigits.")
	       #t)
	(let ((v (make-bytevector (+ l 4))))
	  (bytevector-fill! v 0)
	  (typetag-set! v sys$tag.bignum-typetag)
	  (bignum-length-set! v bigits)
	  v))))


;-----------------------------------------------------------------------------
; Section 2.
;
; Representation-independent stuff that should be reimplemented as
; primitives in the compiler.

; Assumes that word size is w and that bigit size is w/2.

; ADDITION

; Given bignums `a', `b', and `c', a bignum index `i', and a carry digit,
; compute the sum of the ith digits of a and b, with the carry, and put
; that in the ith digit of c, returning the carry.
; The carry is always 1 or 0.

(define (big2+ a b c i carry)
  (let ((r (+ (bignum-ref a i) (bignum-ref b i) carry)))
    (bignum-set! c i (logand r bigit-mask))
    (rshl r bigit-shift)))


; Special case: carry propagation.

(define (big1+ a c i carry)
  (let ((r (+ (bignum-ref a i) carry)))
    (bignum-set! c i (logand r bigit-mask))
    (rshl r bigit-shift)))


; SUBTRACTION

; Given ditto, compute c[i] = a[i] - b[i] - borrow, returning the new borrow.
; The borrow is always 0 or 1.

(define (big2- a b c i borrow)
  (let ((r (- (bignum-ref a i) (bignum-ref b i) borrow)))
    (bignum-set! c i (logand (+ r bignum-base) bigit-mask))
    (if (< r 0) 1 0)))


; Special case: borrow propagation.

(define (big1- a c i borrow)
  (let ((r (- (bignum-ref a i) borrow)))
    (bignum-set! c i (logand (+ r bignum-base) bigit-mask))
    (if (< r 0) 1 0)))


; MULTIPLICATION

; Given bignums a, b, c, indices i, j, and a carry, compute
;   c[ i+j ] = (a[i]*b[j]+c[i+j]+carry) mod bignum-base
; and return (a[i]*b[j]+c[i+j]+carry) div bignum-base
;
; The mod/div case is hard because the intermediate result may not
; always fit in a fixnum, but in a 32-bit bignum.

(define (big2*+ a b c i j carry)
  (let ((t (+ (* (bignum-ref a i) (bignum-ref b j))
	      (bignum-ref c (+ i j))
	      carry)))
    (if (fixnum? t)
	(begin (bignum-set! c (+ i j) (logand t bigit-mask))
	       (rshl t bigit-shift))
	(begin (bignum-set! c (+ i j) (bignum-ref t 0))
	       (bignum-ref t 1)))))

; Old code for (if (fixnum? t) ...) part:
;
; (bignum-set! c (+ i j) (remainder t bignum-base))
; (quotient t bignum-base)


; DIVISION

; The following are used in implementing bignum division. All references
; are to Algorithm D in Knuth vol II, 2nd ed, pp 257-258.

; Step D3: calculate an approximation to q_j, then adjust it if necessary.
;
; Here, uj, uj+1, and uj+2 are the current most significant bigits of
; the dividend, and v1 and v2 are the two most significant bigits of the
; divisor.
;
; FIXME: can we get rid of multiplies by bignum-base? They might overflow
;        a fixnum?

(define (big~q uj uj+1 uj+2 v1 v2)

  (define t (+ (* uj bignum-base) uj+1))

  (define (loop ~q)
    (if (> (* v2 ~q)
	   (+ (* (- t (* ~q v1)) bignum-base) uj+2))
	(loop (- ~q 1))
	~q))

  (loop (if (= uj v1)
	    (- bignum-base 1)
	    (quotient t v1))))


; Step D4: multiply and subtract, returning borrow.
;
; Subtle: newv may be longer than v. Ignore high digit, and do not use
; (bignum-length newv) for (bignum-length v). [Justification for ignoring?]

(define (big*- u v j ~q)
  (let ((newv (big-multiply-through-by v ~q)))
    (letrec ((loop
	      (lambda (i k borrow)
		(if (<= i j)
		    (let ((r (- (bignum-ref u i) (bignum-ref newv k) borrow)))
		      (bignum-set! u i (logand (+ r bignum-base) bigit-mask))
		      (if (negative? r)
			  (loop (+ i 1) (+ k 1) 1)
			  (loop (+ i 1) (+ k 1) 0)))
		    borrow))))
      (loop (- j (bignum-length v)) 0 0))))


; Step D6: add back.
;
; This procedure is called only with very low probability. It will be
; invoked if the two arguments to quotient are e.g.
; 299999999999999999999999 and 100000000000000000000000, and also
; for any even multiple of that first argument, with the second argument.
; (I discovered this by accident.)
; Another way is to feed 0.9999...9 to the reader, for more than 19 9s.
;
; FIXME: get rid of quotient, remainder

(define (big-addback u v j)
  (letrec ((loop 
	    (lambda (i k carry)
	      (cond ((< i j)
		     (let ((r (+ (bignum-ref u i) (bignum-ref v k) carry)))
		       (bignum-set! u i (remainder r bignum-base))
		       (loop (+ i 1) (+ k 1) (quotient r bignum-base))))
		    ((= i j)
		     (let ((r (+ (bignum-ref u i) carry)))
		       (bignum-set! u i (remainder r bignum-base))))
		    (else
		     '())))))
    (loop (- j (bignum-length v)) 0 0)))


; Multiply through by a fixnum < bignum-base, producing new bignum.
;
; Must normalize here, since division algorithm depends on the high 
; digit being non-zero (length field must be valid). But must also
; not produce fixnum.

(define (big-multiply-through-by b f)
  (let* ((l (bignum-length b))
	 (q (bignum-alloc (+ l 1))))
    (letrec ((loop
	      (lambda (i carry)
		(if (= i l)
		    (bignum-set! q i carry)
		    (let ((r (+ (* (bignum-ref b i) f) carry)))
		      (if (fixnum? r)
			  (let ((lo (logand r bigit-mask))
				(hi (rsha r bigit-shift)))
			    (bignum-set! q i lo)
			    (loop (+ i 1) hi))
			  (let ((lo (bignum-ref r 0))
				(hi (bignum-ref r 1)))
			    (bignum-set! q i lo)
			    (loop (+ i 1) hi))))))))
      (loop 0 0)
      (big-limited-normalize! q))))


; Step of fast-divide algorithm.
;
; The multiply by bignum-base can be converted to either a shift or a
; bignum-alloc and bignum-set! by determining, like big-fits-in-fix? does,
; whether the multiplication will overflow.

(define (fast-div-step src dest i divisor rem1)
  (let* ((d    (+ (* rem1 bignum-base) (bignum-ref src i)))
	 (q    (quotient d divisor))
	 (rem2 (- d (* q divisor))))
    (bignum-set! dest i q)
    rem2))


; Coercions

; Assumes that the number fits in a fixnum.

(define (bignum->fixnum b)
  (let ((d0 (bignum-ref b 0))
	(d1 (bignum-ref b 1))
	(n  (sign-negative? (bignum-sign b))))
    (if (>= (lsh d1 2) bignum-base/2)
	max-negative-fixnum
	(let ((c (logior (lsh d1 bigit-shift) d0)))
	  (if n
	      (- c)
	      c)))))

; Can't use `big-normalize!' because it'd convert it back to a fixnum.
; (Could use big-limited-normalize, though.)

(define (fixnum->bignum f)
  (if (not (fixnum? f))
      (error "fixnum->bignum: " f " is not a fixnum." ))
  (let ((b (bignum-alloc bigits-per-fixnum))
	(a (abs f)))
    ; (abs f) will be a bignum if f is the most negative fixnum.
    (if (bignum? a)
	(let ((a (bignum-copy a)))
	  (bignum-sign-set! a negative-sign)
	  a)
	(begin (bignum-set! b 0 (logand a bigit-mask))
	       (bignum-set! b 1 (rsha a bigit-shift))
	       (bignum-length-set! b 2)
	       (if (< f 0) (bignum-sign-set! b negative-sign))
	       b))))


;-----------------------------------------------------------------------------
; Section 3.
;
; Machine-independent stuff.
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
	  (big-flip-sign! c))
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
	  (big-flip-sign! c))
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
; FIXME
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


; Divide bignum `a' by bignum `b', returning an integer if the 
; remainder is 0, and a ratnum otherwise.

(define (bignum-divide a b)
  (let ((sa (bignum-sign a))
	(sb (bignum-sign b)))
    (let* ((c (big-divide-digits a b))
	   (q (big-normalize! (car c)))
	   (r (big-normalize! (cdr c))))
      (if (zero? r)
	  (if (not (= sa sb))
	      (if (bignum? q)
		  (begin (bignum-sign-set! q negative-sign)
			 q)
		  (- q))
	      q)
	  (make-reduced-ratnum a b)))))


; Return the negation of the argument (a new bignum).

(define (bignum-negate a)
  (let ((b (bignum-copy a)))
    (big-flip-sign! b)
    (big-normalize! b)))

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

; predicates

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

; Takes a bignum and a radix and returns the string which is the printable
; representation of the bignum in that radix.
;
; Uses brute force with extreme prejudice.
;
; Note that the use of big-divide-digits guarantees that the resulting values
; are bignums regardless of magnitude.

(define (bignum->string b r)
  (if (bignum-zero? b)
      (string-copy "0")
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

; Copy a bignum

(define (bignum-copy b)
  (let ((c (bignum-alloc (bignum-length b))))
    (big-copy-into! b c)
    (bignum-sign-set! c (bignum-sign b))
    c))

(define (sign-negative? sign)
  (= sign negative-sign))

(define (sign-positive? sign)
  (= sign positive-sign))

(define (big-flip-sign! b)
  (if (sign-negative? (bignum-sign b))
      (bignum-sign-set! b positive-sign)
      (bignum-sign-set! b negative-sign)))


; Add the digits of two bignums, producing a third, positive, bignum.

(define (big-add-digits a b)
  (let* ((la   (bignum-length a))
	 (lb   (bignum-length b))
	 (lmax (if (> la lb) la lb))
	 (lmin (if (< la lb) la lb))
	 (c    (bignum-alloc (+ lmax 1))))

    ;; add common segments

    (letrec ((loop
	      (lambda (i carry)
		(if (< i lmin)
		    (loop (+ i 1) (big2+ a b c i carry))

		    ;; add carry thru longest number
		    
		    (let ((rest (if (= i la) b a)))
		      (letrec ((loop2 
				(lambda (i carry)
				  (if (< i lmax)
				      (loop2 (+ i 1) (big1+ rest c i carry))
				      (begin (bignum-set! c i carry)
					     c)))))
			(loop2 i carry)))))))
      (loop 0 0))))

; Subtract the digits of bignum b from the digits of bignum a, producing 
; a third, possibly negative, bignum c.

(define (big-subtract-digits a b)
  (let ((x (big-compare-magnitude a b)))  ; FIXME: Potentially expensive.
    ;; Set up so that abs(a) >= abs(b)
    (let ((a    (if (negative? x) b a))
	  (b    (if (negative? x) a b)))
      (let* ((la   (bignum-length a))
	     (lb   (bignum-length b))
	     (lmax la)
	     (lmin lb)
	     (c    (bignum-alloc (+ lmax 1)))) ; FIXME: are you sure?

	; Subtract common segments

	(define (loop-common i borrow)
	  (if (< i lmin)
	      (loop-common (+ i 1) (big2- a b c i borrow))
	      (loop-rest (if (= i la) b a) i borrow)))

	; Subtract borrow through longest number

	(define (loop-rest rest i borrow)
	  (if (< i lmax)
	      (loop-rest rest (+ i 1) (big1- rest c i borrow))
	      (begin (if (negative? x)
			 (big-flip-sign! c))
		     c)))
	  
	(loop-common 0 0)))))


; Multiply the digits of two positive bignums, producing a third,
; positive, bignum.

(define (big-multiply-digits a b)
  (let* ((la (bignum-length a))
	 (lb (bignum-length b))
	 (c  (bignum-alloc (+ la lb))))
    (letrec ((loop1
	      (lambda (ai)
		(if (< ai la)
		    (letrec ((loop2
			      (lambda (bi carry)
				(if (< bi lb)
				    (loop2 (+ bi 1) (big2*+ a b c ai bi carry))
				    (begin (bignum-set! c (+ ai bi) carry)
					   (loop1 (+ ai 1)))))))
		      (loop2 0 0))
		    c))))
      (loop1 0))))


; Divide two positive bignums, producing a pair, both elements of which are 
; bignums, the car being the quotient and the cdr being the remainder.
;
; The arguments may be signed, but the signs will be ignored.

(define (big-divide-digits a b)

  ; Copy bignum with an extra 0 as the most significant digit.
  ; Notice that this invalidates the length field, and the length
  ; of the resulting bignum cannot be taken without first 
  ; normalizing!

  (define (big-extend-with-zero b)
    (let ((c (bignum-alloc (+ (bignum-length b) 1))))
      (big-copy-into! b c)
      (bignum-sign-set! c (bignum-sign b))
      c))

  ; `a' is a bignum, `b' is a fixnum < bignum-base. Produces a pair
  ; of bignums, even if the remainder is always a fixnum.

  (define (fast-divide a b)
    (let ((q (bignum-alloc (bignum-length a))))
      (letrec ((loop
		(lambda (rem i)
		  (if (< i 0)
		      rem
		      (loop (fast-div-step a q i b rem) (- i 1))))))
	(let ((rem (loop 0 (- (bignum-length a) 1))))
	  (big-limited-normalize! q)
	  (cons q (if (fixnum? rem)
		      (fixnum->bignum rem)
		      (begin (error "fast-divide: impossible: " rem) #t)))))))

  ; `a' and `b' are both bignums, with (length a) >= (length b) and
  ; (length b) > 1. Produces a pair of bignums.

  (define (slow-divide a b)
    (let* ((d  (quotient bignum-base
			 (+ (bignum-ref b (- (bignum-length b) 1)) 1)))
	   (u  (if (= 1 d)
		   (big-extend-with-zero a)
		   (big-multiply-through-by a d)))
	   (v  (big-multiply-through-by b d))
	   (lu (+ (bignum-length a) 1))       ; may not take length of u
	   (lv (bignum-length v))
	   (v1 (bignum-ref v (- lv 1)))
	   (v2 (bignum-ref v (- lv 2)))
	   (lq (- lu lv))
	   (q  (bignum-alloc lq)))

      ; - Uj+1 and Uj+2 can be reused in future iterations, but the
      ;   time is spent other places, so it hardly matters.

      (define (loop j p)
	(if (>= p 0)
	    (let* ((uj     (bignum-ref u j))
		   (uj+1   (bignum-ref u (- j 1)))
		   (uj+2   (bignum-ref u (- j 2)))
		   (~q     (big~q uj uj+1 uj+2 v1 v2))
		   (borrow (big*- u v j ~q)))
	      (bignum-set! q p ~q)
	      (if (not (zero? borrow))
		  (begin
		    ;(display "slow-divide: addback") (newline)
		    ;(display "arg 1: ") (display a) (newline)
		    ;(display "arg 2: ") (display b) (newline)
		    (bignum-set! q p (- ~q 1))
		    (big-addback u v j)))
	      (loop (- j 1) (- p 1)))))

      (loop (- lu 1) (- lq 1))
      (big-limited-normalize! q)
      (bignum-truncate-length! u lv)
      (big-limited-normalize! u)
      (cons q (car (fast-divide u d)))))

  ; Maintain some invariants and catch the easy cases.

  (cond ((bignum-zero? b)
	 (error 'generic-arithmetic "Bignum division by zero")
	 #t)
	((bignum-zero? a)
	 (cons (fixnum->bignum 0) (fixnum->bignum 0)))
	(else
	 (let ((la (bignum-length a))
	       (lb (bignum-length b)))
	   (cond ((> lb la)
		  (let ((r (bignum-copy b)))
		    (bignum-sign-set! r positive-sign) ; b may be signed
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
	       (- (big-compare-magnitude a b))
	       (big-compare-magnitude a b))))))

; FIXME: this loop is a natural for optimization:
;  (big-cmp-magnitude a b i) => integer

(define (big-compare-magnitude a b)
  (let ((la (bignum-length a))
	(lb (bignum-length b)))
    (if (not (= la lb))
	(- la lb)
	(letrec ((loop
		  (lambda (i)
		    (cond ((< i 0)
			   0)
			  ((= (bignum-ref a i) (bignum-ref b i))
			   (loop (- i 1)))
			  (else
			   (- (bignum-ref a i) (bignum-ref b i)))))))
	  (loop (- la 1))))))
    
; Normalize a bignum -- this involves removing leading zeroes, and, if the
; number is small enough to fit in a fixnum, converting it to a fixum.

(define (big-normalize! b)
  (letrec ((loop
	    (lambda (i)
	      (cond ((< i 0)
		     0)
		    ((= (bignum-ref b i) 0)
		     (loop (- i 1)))
		    (else
		     (bignum-length-set! b (+ i 1))
		     (if (big-fits-in-fix? b)
			 (bignum->fixnum b)
			 b))))))
    (loop (- (bignum-length b) 1))))

; Normalize, but do not convert.

(define (big-limited-normalize! b)
  (letrec ((loop
	    (lambda (i)
	      (cond ((< i 0)
		     (bignum-length-set! b 0)
		     b)
		    ((= (bignum-ref b i) 0)
		     (loop (- i 1)))
		    (else
		     (bignum-length-set! b (+ i 1))
		     b)))))
    (loop (- (bignum-length b) 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Debugging code

; Dump a bignum in Hex; requires a 'sane' bignum.

(define (bigdump bignum)

  (define (pr n)
    (let ((s (number->string n 16)))
      (let loop ((i (- 4 (string-length s))))
	(if (> i 0)
	    (begin (display "0")
		   (loop (- i 1)))
	    (display s)))))

  (let loop ((i (- (bignum-length bignum) 1)))
    (if (>= i 0)
	(begin (pr (bignum-ref bignum i))
	       (loop (- i 1)))
	(newline))))

; Dump the bytevector, anything goes. Very useful for debugging.

(define (bigdump* bignum)
  (let ((l (bytevector-like-length bignum))
	(v "0123456789abcdef"))
    (if (zero? l)
	(display "0")
	(let loop ((i 0))
	  (if (< i l)
	      (let ((n (bytevector-like-ref bignum i)))
		(if (and (zero? (remainder i 4))
			 (not (zero? i)))
		    (display " "))
		(display (string-ref v (quotient n 16)))
		(display (string-ref v (remainder n 16)))
		(loop (+ i 1))))))))

; In production code, remove all calls to bntrace from the bignum code.

(define bn-trace-enabled #t)
(define (bntrace . args)
  (if bn-trace-enabled
      (begin (for-each display args)
	     (newline))))

; eof
