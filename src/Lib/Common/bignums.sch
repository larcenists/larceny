; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Scheme code for bignum arithmetic.
;
; The bignum code consists of five sections, not counting a section
; that defines constants.
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
; The fifth section implements Karatsuba's algorithm for multiplication.
;
; Representation.
;
; A bignum consists of a sign, a digit count, and a number of digits.
; The representation of the sign is abstracted in the variables
; `negative-sign' and `positive-sign'; the signs are fixnum quantities.
; The size (and hence base) of each digit is abstracted in the variable
; `bignum-base'.
;
; The bignum-base must be 2^n, where n is 8, 16, or 32.  Regardless
; of the bignum-base, bignums can also be viewed as having 32-bit
; digits.  The bignum-length32 procedure returns the length in units
; of 32-bit digits.
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
; When multiplying large bignums, the implementation now uses
; Karatsuba's algorithm (Knuth vol II, 2nd edition, section 4.3.3A).
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: bignum code is in transition to 32-bit bigits.
; Most things still assume 16-bit bigits, but the following
; procedures take arguments expressed in terms of 32-bit bigits:
;     bignum-add!
;     bignum-subtract!
;     bignum-multiply-by-scalar-and-add!

($$trace "bignums")

; Here is the list of procedures that are exported by this module. 
;
;  bignum-add, bignum-subtract, bignum-multiply, bignum-quotient,
;  bignum-remainder, bignum-divide, bignum-negate, bignum-abs,
;  bignum=?, bignum<=?, bignum<?, bignum>=?, bignum>?, bignum-zero?,
;  bignum-positive?, bignum-negative?, bignum-even?, bignum-odd?,
;  bignum->fixnum, fixnum->bignum, bignum->string, bignum?,
;  bitwise-length:bignum


;---------------------------------------------------------------------------
; Section 0. System-independent constants (not really).

(define smallest-positive-bignum 536870912)  ; 2^29
(define largest-negative-bignum -536870913)  ; -(2^29+1)
(define max-negative-fixnum     -536870912)  ; -(2^29)
(define max-positive-fixnum      536870911)  ; 2^29-1

(define negative-sign 1)                     ; the sign of a negative bignum
(define positive-sign 0)                     ; ditto of a positive one

(define max-bignum-bytes 16000000)           ; don't let the allocator panic


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
    (fxlogand (+ n 3) (fxlognot 3)))

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
    (bignum-set! c i (fxlogand r bigit-mask))
    (fxrshl r bigit-shift)))


; Special case: carry propagation.

(define (big1+ a c i carry)
  (let ((r (+ (bignum-ref a i) carry)))
    (bignum-set! c i (fxlogand r bigit-mask))
    (fxrshl r bigit-shift)))


; SUBTRACTION

; Given ditto, compute c[i] = a[i] - b[i] - borrow, returning the new borrow.
; The borrow is always 0 or 1.

(define (big2- a b c i borrow)
  (let ((r (- (bignum-ref a i) (bignum-ref b i) borrow)))
    (bignum-set! c i (fxlogand (+ r bignum-base) bigit-mask))
    (if (< r 0) 1 0)))


; Special case: borrow propagation.

(define (big1- a c i borrow)
  (let ((r (- (bignum-ref a i) borrow)))
    (bignum-set! c i (fxlogand (+ r bignum-base) bigit-mask))
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
        (begin (bignum-set! c (+ i j) (fxlogand t bigit-mask))
               (fxrshl t bigit-shift))
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
                      (bignum-set! u i (fxlogand (+ r bignum-base) bigit-mask))
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
                          (let ((lo (fxlogand r bigit-mask))
                                (hi (fxrsha r bigit-shift)))
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
    (if (>= (fxlsh d1 2) bignum-base/2)
        max-negative-fixnum
        (let ((c (fxlogior (fxlsh d1 bigit-shift) d0)))
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
        (begin 
          (if (zero? f)
              (bignum-length-set! b 0)
              (begin
                (bignum-set! b 0 (fxlogand a bigit-mask))
                (bignum-set! b 1 (fxrsha a bigit-shift))))
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
    (let ((c (let ((la (bignum-length32 a))
                   (lb (bignum-length32 b)))
               (cond ((and (< karatsuba:threshold la)
                           (< karatsuba:threshold lb))
                      (let ((a (bignum-copy a))
                            (b (bignum-copy b)))
                        (bignum-sign-set! a positive-sign)
                        (bignum-sign-set! b positive-sign)
                        (karatsuba-algorithm a b)))
                     ((< la lb)
                      (big-multiply-digits a b))
                     (else
                      (big-multiply-digits b a))))))
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

(define (bignum-remainder a b)
  (let ((sa (bignum-sign a))
        (sb (bignum-sign b)))
    (let ((c (cdr (big-divide-digits a b))))
      (if (sign-negative? sa)
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
      (cond ((zero? r)
             (if (not (= sa sb))
                 (if (bignum? q)
                     (begin (bignum-sign-set! q negative-sign)
                            q)
                     (- q))
                 q))
            ((sign-positive? sb)
             (make-reduced-ratnum a b))
            ((= sa sb)
             (make-reduced-ratnum (abs a) (abs b)))
            (else
             (make-reduced-ratnum (- a) (abs b)))))))


; Return the negation of the argument (a new bignum).

(define (bignum-negate a)
  (let ((b (bignum-copy a)))
    (big-flip-sign! b)
    (big-normalize! b)))

; logical operators

(define integer-logand)
(define integer-logior)
(define integer-logxor)

(let ()
  ;; fxop : op for each bigit of a and b (w/ 2's complement semantics)
  ;; sign : predetermined sign of the result value
  (define (integer-logop fxop sign a b)
    (define (safe-ref n i)
      (cond ((< i (bignum-length n))
             (bignum-ref n i))
            (else
             0)))
    (define (mask-ref n@i cn mn)
      (fxlogand bigit-mask (+ cn (fxlogxor n@i mn))))
    (define (next-carry n i previous-carry)
      (if (and (= previous-carry 1)
               (= (bignum-ref n i) 0))
          1
          0))
    (let* ((a (if (fixnum? a) (fixnum->bignum a) a))
           (b (if (fixnum? b) (fixnum->bignum b) b))
           (la (bignum-length a))
           (lb (bignum-length b))
           (d (bignum-alloc (+ 1 (max la lb))))
           (ma (if (negative? a) bigit-mask 0))
           (mb (if (negative? b) bigit-mask 0)))
      (let loop ((i 0) 
                 (ca (if (negative? a) 1 0)) 
                 (cb (if (negative? b) 1 0))
                 (cd (if (sign-negative? sign) 1 0)))
        (cond 
         ((= i (+ 1 (max la lb)))
          (bignum-sign-set! d sign)
          (big-normalize! d))
         (else
          (let* ((a@i (safe-ref a i))
                 (b@i (safe-ref b i))
                 (a@i^m (mask-ref a@i ca ma))
                 (b@i^m (mask-ref b@i cb mb))
                 (r (fxop a@i^m b@i^m))
                 (r2 (- r cd))
                 (r3 (if (< r2 0) bigit-mask r2))
                 (r4 (fxlogand bigit-mask (fxlognot r3))))
            (if (sign-negative? sign)
                (bignum-set! d i r4)
                (bignum-set! d i r))
            (loop (+ i 1) 
                  (next-carry a i ca) 
                  (next-carry b i cb)
                  (if (< r2 0) 1 0))))))))

  ;; An optimized version that assumes non-negative a, b, and result,
  ;; which allows for a simpler implementation.  (unused for now)
  (define (integer-logop/pos fxop a b)
    (let* ((a (if (fixnum? a) (fixnum->bignum a) a))
           (b (if (fixnum? b) (fixnum->bignum b) b))
           (la (bignum-length a))
           (lb (bignum-length b))
           (d (bignum-alloc (max la lb))))
      ;; 1. Common digits
      (do ((i 0 (+ i 1)))
          ((= i (min la lb)))
        (bignum-set! d i (fxop (bignum-ref a i) (bignum-ref b i))))
      ;; 2. Remaining digits
      (let ((ld (max la lb)))
        (do ((i (min la lb) (+ i 1))
             (e (if (= la ld) a b)))
            ((= i ld))
          (bignum-set! d i (fxop (bignum-ref e i) 0))))
      ;; 3. Normalize
      (big-normalize! d)))

  (define (exact-integers? a b)
    (and (exact? a) (integer? b) 
         (exact? b) (integer? b)))
  
  (define (%integer-logand a b)
    (cond 
     ((not (exact-integers? a b)) 
      (error 'integer-logand ": Arguments must be exact integers"))
     ((= -1 a) b)
     ((= -1 b) a)
     (else
      (let ((sign (if (and (negative? a) (negative? b))
                      negative-sign
                      positive-sign)))
        (integer-logop fxlogand sign a b)))))

  (define (%integer-logior a b)
    (cond 
     ((not (exact-integers? a b)) 
      (error 'integer-logior ": Arguments must be exact integers"))
     ((= 0 a) b)
     ((= 0 b) a)
     (else
      (let ((sign (if (or (negative? a) (negative? b))
                      negative-sign
                      positive-sign)))
        (integer-logop fxlogior sign a b)))))

  (define (%integer-logxor a b)
    (cond 
     ((not (exact-integers? a b)) 
      (error 'integer-logxor ": Arguments must be exact integers"))
     ((= 0 a) b)
     ((= 0 b) a)
     (else
      (let ((sign (if (not (eqv? (negative? a) (negative? b)))
                      negative-sign
                      positive-sign)))
        (integer-logop fxlogxor sign a b)))))

  (set! integer-logand %integer-logand)
  (set! integer-logior %integer-logior)
  (set! integer-logxor %integer-logxor))

(define (integer-lognot n)
  (if (fixnum? n)
      (fxlognot n)
      (- -1 n)))

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

; bitwise-length for a bignum argument.

(define (bitwise-length:bignum b)
  (let* ((n (bignum-length b))
         (bigit (bignum-ref b (- n 1)))
         (k (+ (bitwise-length bigit)
               (* (- n 1) bits-per-bigit))))
    (if (= positive-sign (bignum-sign b))
        k
        (let ((t (bignum-copy b)))
          (bignum-shift-right! t t (- k 1))
          (bignum-length-set! t 1)
          (bignum-shift-left! t t (- k 1))
          (bignum-length-set! t n)
          (if (bignum=? b t)
              (- k 1)
              k)))))

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
;
;(define (big-add-digits a b)
;  (let* ((la   (bignum-length a))
;         (lb   (bignum-length b))
;         (lmax (if (> la lb) la lb))
;         (lmin (if (< la lb) la lb))
;         (c    (bignum-alloc (+ lmax 1))))
;
;    ;; add common segments
;
;    (letrec ((loop
;              (lambda (i carry)
;                (if (< i lmin)
;                    (loop (+ i 1) (big2+ a b c i carry))
;
;                    ;; add carry thru longest number
;                    
;                    (let ((rest (if (= i la) b a)))
;                      (letrec ((loop2 
;                                (lambda (i carry)
;                                  (if (< i lmax)
;                                      (loop2 (+ i 1) (big1+ rest c i carry))
;                                      (begin (bignum-set! c i carry)
;                                             c)))))
;                        (loop2 i carry)))))))
;      (loop 0 0))))

; FIXME: this transitional code computes with 32-bit bigits.
; The size of c is increased by 1 16-bit bigit to make sure it has
; a full 32-bit bigit at its most significant end.

(define (big-add-digits a b)
  (let* ((la   (bignum-length a))
         (lb   (bignum-length b))
         (lmax (if (> la lb) la lb))
         (lmin (if (< la lb) la lb))
         (c    (bignum-alloc (+ lmax 2))))

    ;; copy a to c

    (bignum-shift-left! a c 0)

    ;; add b to c

    (bignum-add! b c 0 0)

    ;; return c

    c))

; Subtract the digits of bignum b from the digits of bignum a, producing 
; a third, possibly negative, bignum c.

'; FIXME
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

; FIXME: this transitional code computes with 32-bit bigits.
; The size of c is increased by 1 16-bit bigit to make sure it has
; a full 32-bit bigit at its most significant end.

; FIXME
(define (big-subtract-digits a b)
  (let ((x (big-compare-magnitude a b)))  ; FIXME: Potentially expensive.
    ;; Set up so that abs(a) >= abs(b)
    (let ((a    (if (negative? x) b a))
          (b    (if (negative? x) a b)))
      (let* ((la   (bignum-length a))
             (lb   (bignum-length b))
             (lmax (if (> la lb) la lb))
             (lmin (if (< la lb) la lb))
             (c    (bignum-alloc (+ lmax 2))))

        ;; copy a to c

        (bignum-shift-left! a c 0)

        ;; subtract b from c

        (bignum-subtract! b c 0 0)

        ;; adjust sign if necessary

        (if (negative? x)
            (big-flip-sign! c))

        ;; return c

        c))))


; Multiply the digits of two positive bignums, producing a third,
; positive, bignum.

;(define (big-multiply-digits a b)
;  (let* ((la (bignum-length a))
;         (lb (bignum-length b))
;         (c  (bignum-alloc (+ la lb))))
;    (letrec ((loop1
;              (lambda (ai)
;                (if (< ai la)
;                    (letrec ((loop2
;                              (lambda (bi carry)
;                                (if (< bi lb)
;                                    (loop2 (+ bi 1) (big2*+ a b c ai bi carry))
;                                    (begin (bignum-set! c (+ ai bi) carry)
;                                           (loop1 (+ ai 1)))))))
;                      (loop2 0 0))
;                    c))))
;      (loop1 0))))

; FIXME: this transitional code mixes 16-bit with 32-bit indexing.
; The size of c is increased by 1 16-bit bigit to make sure it has
; a full 32-bit bigit at its most significant end.
; The index for loop1 is a 16-bit index, so it is halved
; for the call to bignum-multiply-by-scalar-and-add!

(define (big-multiply-digits a b)
  (let* ((la (bignum-length a))
         (lb (bignum-length b))
         (c  (bignum-alloc (+ la lb 1))))
    (define (loop1 i)
      (if (>= i la)
          c
          (let ((lo (bignum-ref a i))
                (hi (bignum-ref a (+ i 1)))
                (i/2 (fxrshl i 1)))
            (bignum-multiply-by-scalar-and-add! b c 0 i/2 hi lo)
            (loop1 (+ i 2)))))
    (loop1 0)))

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
                  (let ((r (bignum-copy a)))
                    (bignum-sign-set! r positive-sign) ; a may be signed
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
; FIXME: commented out.

';'  FIXME
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
; FIXME: commented out.

';'  FIXME
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

;-----------------------------------------------------------------------------
; Section 5.
;
; Karatsuba multiplication.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Karatsuba's algorithm for multiplication of bignums.
;
; See Knuth Volume II, section 4.3.3A.
;
; Algorithm: Given bignums u and v, both representable
; in 2n bits, reduce the problem to n-bit operations
; as follows.  Let
;
;     u = 2^n U1 + U0
;     v = 2^n V1 + V0
;
; Then
;
;     u * v = (2^{2n} + 2^n) U1 V1
;           + 2^n (U1 - U0) (V0 - V1)
;           + (2^n + 1) U0 V0
;
; That formula uses 3 n-bit multiplications, plus some
; shifts and additions.
;
; When the algorithm is applied recursively, it reduces
; the time to multiply two n-bit numbers from O(n^2) to
; O(n^{lg 3}) \approx O(n^1.585).
;
; The Toom-Cook method is a generalization of Karatsuba's.
;
; FIXME: assumes bytes-per-bigit is a divisor of 4.
; FIXME: also assumes knowledge of bignum representation.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given two numbers, returns their product.
; Mainly for testing.

';FIXME
(define (karatsuba-multiplication u v)
  (cond ((fixnum? u)
         (* u v))
        ((fixnum? v)
         (* u v))
        ((and (bignum? u) (bignum? v))
         (cond ((< u 0)
                (- (karatsuba-multiplication (- u) v)))
               ((< v 0)
                (- (karatsuba-multiplication u (- v))))
               (else
                (let ((ulength (bignum-length32 u))
                      (vlength (bignum-length32 v)))
                  (if (and (< karatsuba:threshold ulength)
                           (< karatsuba:threshold vlength))
                      (let ((result (karatsuba-algorithm u v)))
                        ';FIXME
                        (if (not (= result (* u v)))
                            (begin (set! *u* u)
                                   (set! *v* v)
                                   (assert (begin 'karatsuba #f))))
                        (big-normalize! result))
                      (* u v))))))
        (else
         (* u v))))

; Given two positive bignums, returns their product as a bignum.
; FIXME: should take advantage of zeroes and ones.

(define (karatsuba-algorithm u v)
  (let* ((ulength (bignum-length32 u))
         (vlength (bignum-length32 v))
         (zlength (+ ulength vlength))
         (digits (quotient (+ 1 (max ulength vlength)) 2))

         (u1 (karatsuba:bignum-hi u digits))
         (u0 (karatsuba:bignum-lo u digits))
         (v1 (karatsuba:bignum-hi v digits))
         (v0 (karatsuba:bignum-lo v digits)))

    ' ;FIXME
    (g u v u1 u0 v1 v0 digits)

    (let* ((u0v0 (karatsuba:multiplication u0 v0))
           (u1v1 (karatsuba:multiplication u1 v1))

           (fixme:ignored '(assert (= (* u0 v0) u0v0)))
           (fixme:ignored '(assert (= (* u1 v1) u1v1)))

           (u1-u0 (karatsuba:subtract! u1 u0))
           (v0-v1 (karatsuba:subtract! v0 v1)))

      (let* ((sign:u1-u0 (bignum-sign u1-u0))
             (sign:v0-v1 (bignum-sign v0-v1))
             (positive:u1-u0*v0-v1? (= sign:u1-u0 sign:v0-v1)))

        (bignum-sign-set! u1-u0 positive-sign)
        (bignum-sign-set! v0-v1 positive-sign)

        (let* ((u1-u0*v0-v1 (karatsuba:multiplication u1-u0 v0-v1))

               (fixme:ignored '(assert (= (* u1-u0 v0-v1) u1-u0*v0-v1)))

               (z (bignum-alloc (* karatsuba:bigits-per-digit zlength))))

          (bignum-shift-left! u0v0 z 0)

          ; z = u0v0
          '
          (assert (zero? (- z u0v0)))

          (bignum-add! u0v0 z 0 digits)

          ; z = (2^n + 1) u0v0
          '
          (assert (zero? (- z (+ (* (expt (expt 2 32) digits) u0v0) u0v0))))

          (bignum-add! u1v1 z 0 digits)

          ; z = (2^n + 1) u0v0 + 2^n u1v1
          '
          (assert (zero? (- z (+ (* (expt (expt 2 32) digits) u0v0)
                                 u0v0
                                 (* (expt (expt 2 32) digits) u1v1)))))

          (bignum-add! u1v1 z 0 (+ digits digits))

          ; z = (2^n + 1) u0v0 + 2^n u1v1 + 2^{2n} u1v1
          '
          (assert (zero? (- z (+ (* (expt (expt 2 32) digits) u0v0)
                                 u0v0
                                 (* (expt (expt 2 32) digits) u1v1)
                                 (* (expt (expt 2 32) (+ digits digits))
                                    u1v1)))))

          (if positive:u1-u0*v0-v1?
              (bignum-add! u1-u0*v0-v1 z 0 digits)
              (let ((t (bignum-alloc (* karatsuba:bigits-per-digit
                                        (bignum-length32 z))))
                    (shift (* karatsuba:bits-per-digit digits)))
                (bignum-shift-left! u1-u0*v0-v1 t shift)
                (big-limited-normalize! z)
                (big-limited-normalize! t)
                (if (<= (big-compare-magnitude z t) 0)
                    (begin (bignum-subtract! z t 0 0)
                           (bignum-shift-left! t z 0)
                           (bignum-sign-set! z negative-sign))
                    (bignum-subtract! t z 0 0))))

          ; z = (2^n + 1) u0v0 + 2^n u1v1 + 2^{2n} u1v1 + 2^n(u1-u0)(v0-v1)
          '
          (assert (zero? (- z (* u v))))

          (big-limited-normalize! z)
          z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level constants and operations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define karatsuba:bits-per-digit 32)

(define karatsuba:bigits-per-digit
  (quotient 4 bytes-per-bigit))

; Maximum number of 32-digits that should be handled without
; using the Karasuba algorithm.  Must be at least 2, else we
; would encounter fixnums when dividing the number of digits
; by 2.

(define karatsuba:threshold 10)

; Given a positive bignum, returns its least significant digits
; as a bignum.

(define (karatsuba:bignum-lo u digits)
  (assert (bignum? u))
  (let* ((nbigits (* karatsuba:bigits-per-digit digits))
         (z (bignum-alloc nbigits))
         (n (bytevector-like-length z)))
    (do ((i 4 (+ i 1)))
        ((>= i n)
         (big-limited-normalize! z))
      (bytevector-like-set! z i (bytevector-like-ref u i)))))

; Given a positive bignum, returns its most significant digits
; as a bignum.

(define (karatsuba:bignum-hi u digits)
  (assert (bignum? u))
  (let* ((d (- (bignum-length32 u) digits))
         (nbigits (* karatsuba:bigits-per-digit d))
         (z (bignum-alloc nbigits)))
    (bignum-shift-right! u z (* karatsuba:bits-per-digit digits))
    (big-limited-normalize! z)))

; Returns u-v, potentially destroying both u and v.

(define (karatsuba:subtract! u v)
  (set! *u* (bignum-copy u))
  (set! *v* (bignum-copy v))
  (assert (bignum? u))
  (assert (bignum? v))
  (if (negative? (big-compare-magnitude u v))  ; FIXME: may be expensive
      (begin (bignum-subtract! u v 0 0)
             (big-flip-sign! v)
             (big-limited-normalize! v))
      (begin (bignum-subtract! v u 0 0)
             (big-limited-normalize! u))))

; Given two bignums, returns their product as a bignum.
; Both arguments must be preserved, but it is okay to
; change their signs temporarily.

(define (karatsuba:multiplication u v)
  (assert (bignum? u))
  (assert (bignum? v))
  (cond ((= negative-sign (bignum-sign u))
         (bignum-sign-set! u positive-sign)
         (let ((w (karatsuba:multiplication u v)))
           (bignum-sign-set! u negative-sign)
           (big-flip-sign! w)
           w))
        ((= negative-sign (bignum-sign v))
         (bignum-sign-set! v positive-sign)
         (let ((w (karatsuba:multiplication u v)))
           (bignum-sign-set! v negative-sign)
           (big-flip-sign! w)
           w))
        (else
         (let ((ulength (bignum-length32 u))
               (vlength (bignum-length32 v)))
           (if (and (< karatsuba:threshold ulength)
                    (< karatsuba:threshold vlength))
               (let ((result (karatsuba-algorithm u v)))
                 ;FIXME
                 '
                 (if (not (= result (* u v)))
                     (begin (set! *u* u)
                            (set! *v* v)
                            (assert (begin 'karatsuba:multiplication #f))))
                 result)
               (let ((w (* u v)))
                 (if (fixnum? w)
                     (fixnum->bignum w)
                     w)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Crude tests.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

';FIXME
(define (test-karatsuba n m)
  (define (test name actual expected)
    (if (not (equal? actual expected))
        (begin (display "***** TEST FAILED ***** ")
               (display name)
               (newline)
               (display "Actual:   ")
               (write actual)
               (newline)
               (display "Expected: ")
               (write expected)
               (newline))))
  (if (positive? n)
      (let ((u (random m))
            (v (random m)))
        (set! *u* u)
        (set! *v* v)
        (test "karatsuba" (karatsuba-multiplication u v) (* u v))
        (test-karatsuba (- n 1) m))))


; For debugging.
;
;     u * v = (2^{2n} + 2^n) U1 V1
;           + 2^n (U1 - U0) (V0 - V1)
;           + (2^n + 1) U0 V0

';FIXME
(define (g u v u1 u0 v1 v0 digits)
  (show u)
  (show v)
  (show u1 u0)
  (show v1 v0)
  (let* ((z0 (* u0 v0))
         (z1 (* (expt 2 (* 32 digits)) u0 v0))
         (z2 (* (expt 2 (* 32 digits)) (- u1 u0) (- v0 v1)))
         (z3 (* (expt 2 (* 32 digits)) u1 v1))
         (z4 (* (expt 2 (* 64 digits)) u1 v1))
         (z (+ z0 z1 z2 z3 z4)))
    (show z0)
    (show z1)
    (show z2)
    (show z3)
    (show z4)
    (show z)
    z))

';FIXME
(define (benchmark-karatsuba n bits)
  (let* ((m (expt 2 bits))
         (us (vector->list (make-vector n 0)))
         (us (map (lambda (x) (random m)) us))
         (vs (map (lambda (x) (random m)) us))
         (nstr (number->string n))
         (mstr (number->string bits))
         (s1 (string-append "karatsuba:" mstr ":" nstr))
         (s2 (string-append "classical:" mstr ":" nstr))
         (r1 '())
         (r2 '()))
    (run-benchmark s1
                   1
                   (lambda ()
                     (set! r1 (map karatsuba-multiplication us vs)))
                   (lambda (x) #t))
    (run-benchmark s2
                   1
                   (lambda ()
                     (set! r2 (map * us vs)))
                   (lambda (x) #t))
    (if (not (equal? r1 r2))
        (begin (set! *us* us)
               (set! *vs* vs)
               (set! *r1* r1)
               (set! *r2* r2)
               (display "***** INCORRECT RESULTS *****")
               (newline)))))

; eof
