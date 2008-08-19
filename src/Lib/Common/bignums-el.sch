; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Endianness-dependent code for bignum arithmetic: little-endian.
;
; A bignum is a bytevector-like structure whose length is divisible by
; four (really: by the number of bytes in a word) and whose contents are
; always interpreted as complete machine words with native endianness.
;
; The first word contains bignum metadata: sign and bignum-length.
; The sign is kept in the upper byte and is 0 (positive) or 1 (negative).
; The bignum-length is kept in the lower three bytes and denotes the number
; of 32-bit "bigits" in the number.  
;
; If the bignum's value is zero, then the sign is ignored and the length
; field must be 0.  Zero-valued bignums should never be allowed to be
; visible to user code.
;
; Most of the following code operates on 16-bit bigits since these fit 
; conveniently in a fixnum.  Thus a 32-bit bigit is split into two 
; 16-bit bigits.  This results in a little hair, as the length field 
; still denotes 32-bit bigits.
;
; FIXME:  This code is in transition to 32-bit bigits.
; The inner loops (at the end of this file) already work with 32-bit
; bigits.
;
; FIXME:  A lot of this code depends upon 30-bit fixnums.
;
; FIXME (see similar comments in bignums-be.sch):
;  - Replace bignum-length-set! with the code from bignum-truncate-length!
;    and remove references to the latter.  Haven't done this because it
;    needs careful testing.
;  - Replace slow definition of big-fits-in-fix? with faster code.  Must
;    also be tested carefully.

($$trace "bignums-el")

(define (bignum-sign b)
  (bytevector-like-ref b 3))

(define (bignum-sign-set! b s)
  (bytevector-like-set! b 3 s))

; System constants

(define bignum-base       65536)        ; number system base
(define bignum-base/2     32767)        ; ditto, divided by two
(define bigit-mask        #xFFFF)       ; to mask off a bigit
(define bytes-per-bigit   2)            ; number of bytes in a bigit
(define bits-per-bigit    16)           ; number of bits in a bigit
(define bigit-shift       16)           ; another name for it
(define bigits-per-fixnum 2)            ; how many bigits to we need to
                                        ;   hold the value of a fixnum?

; Procedures, assigned below

(define bignum-ref)                     ; get a bigit
(define bignum-set!)                    ; set a bigit
(define bignum-length)                  ; get the number of bigits
(define bignum-length-set!)             ; set the number of bigits
(define bignum-truncate-length!)        ; (complicated)

; The following procedures are defined in a LET and exported to allow
; the compiler to generate fast calls to internal definitions (or inline 
; them).

(let ()

  ; Bytevector-like-halfword-ref
  ;   bv is a bytevector
  ;   i is the _halfword_ index; i=1 means byte offset 2, and so on

  (define (bytevector-like-halfword-ref bv i)
    (let ((i (+ i i)))
      (fxlogior (fxlsh (bytevector-like-ref bv (+ i 1)) 8)
              (bytevector-like-ref bv i))))

  (define (bytevector-like-halfword-set! bv i v)
    (let ((hi (fxrsha v 8))
          (lo (fxlogand v 255))
          (i  (+ i i)))
      (bytevector-like-set! bv (+ i 1) hi)
      (bytevector-like-set! bv i lo)))

  (define (%bignum-ref a i)
    (bytevector-like-halfword-ref a (+ i 2)))

  (define (%bignum-set! a i v)
    (bytevector-like-halfword-set! a (+ i 2) v))

  ; Return the number of 16-bit bigits. We check if the high 16-bit 
  ; bigit of the high 32-bit bigit is 0 and return length-1 if so. 

  (define (%bignum-length b)
    (let* ((l0 (bytevector-like-halfword-ref b 0))
           (l2 (bytevector-like-ref b 2))
           (l0 (fxlogior l0 (fxlsh l2 16)))
           (l  (+ l0 l0)))
      (cond ((zero? l) l)
            ((zero? (bignum-ref b (- l 1))) (- l 1))
            (else l))))

  ; Set the number of 16-bit bigits. The number is converted to 32-bit bigits,
  ; which may involve adding a 0 bigit at the high end; see comments above.
  ;
  ; `l' is the number of 16-bit bigits. To get the number of 32-bit bigits,
  ; we must round up to an even number, then divide by 2. This is equivalent
  ; to adding 1 and dividing by 2.

  (define (%bignum-length-set! b l)
    (let ((l (fxrsha (+ l 1) 1)))
      (bytevector-like-halfword-set! b 0 (fxlogand l 65535))
      (bytevector-like-set! b 2 (fxrshl l 16))))

  ; This is like bignum-length-set!, except that it works also when the
  ; length is odd and the most significant half of the 32-bit bigit is not 
  ; zero, i.e., in that case, normalize would not work properly unless said
  ; half is zeroed out.  If this seems like a hack to you, you're right.
  ; I think the most reasonable thing to do would be to make this procedure
  ; the definition of bignum-length-set!.

  (define (%bignum-truncate-length! b ln)
    (let ((l (fxrsha (+ ln 1) 1)))
      (bytevector-like-halfword-set! b 0 (fxlogand l 65535))
      (bytevector-like-set! b 2 (fxrshl l 16))
      (if (not (= ln (+ l l)))
          (bignum-set! b ln 0))))

  (set! bignum-ref %bignum-ref)
  (set! bignum-set! %bignum-set!)
  (set! bignum-length %bignum-length)
  (set! bignum-length-set! %bignum-length-set!)
  (set! bignum-truncate-length! %bignum-truncate-length!)
  #t)

; Old big-fits-in-fix? test; rather slow.
;
;(define (big-fits-in-fix? b)
;  (and (bignum>? b largest-negative-bignum)
;       (bignum<? b smallest-positive-bignum)))

; A bignum fits in fixnum if it has zero 32-bit bigits or
; exactly 1 32-bit bigit and its magnitude is less than 2^29
; or exactly 1 32-bit bigit and its magnitude is exactly 2^29.

(define (big-fits-in-fix? b)
  (let ((b0 (bytevector-like-ref b 0)))
    (cond ((= 1 b0)
           (and (= 0 (bytevector-like-ref b 1))
                (= 0 (bytevector-like-ref b 2))
                (let* ((hi8 (bytevector-like-ref b 7))
                       (hi3 (fxlogand #xe0 hi8)))
                  (or (= 0 hi3)
                      (and (= negative-sign (bytevector-like-ref b 3))
                           (= #b00100000 hi8)                       ; FIXME
                           (= 0 (bytevector-like-ref b 6))
                           (= 0 (bytevector-like-ref b 5))
                           (= 0 (bytevector-like-ref b 4)))))))
          ((= 0 b0)
           (and (= 0 (bytevector-like-ref b 1))
                (= 0 (bytevector-like-ref b 2))))
          (else
           #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME:  This code doesn't depend on endianness, but it does
; depend on the representation of a bignum, especially on the
; fact that bignums current use 32-bit digits.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Normalize a bignum -- this involves removing leading zeroes, and, if the
; number is small enough to fit in a fixnum, converting it to a fixum.

(define (big-normalize! b)
  (let* ((n (bignum-length32 b)))
    (define (loop i)
      (cond ((= i 0)
             0)
            ((< 0 (bytevector-like-ref b i))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 1)))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 2)))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 3)))
             (finish i))
            (else
             (loop (- i 4)))))
    (define (finish i)
      (bignum-length32-set! b (fxrshl i 2))
      (if (and (= 4 i)
               (big-fits-in-fix? b))
          (bignum->fixnum b)
          b))
    (loop (fxlsh n 2))))

; Normalize, but do not convert.

(define (big-limited-normalize! b)
  (let* ((n (bignum-length32 b)))
    (define (loop i)
      (cond ((= i 0)
             (bignum-length32-set! b 0)
             (bignum-sign-set! b positive-sign)
             b)
            ((< 0 (bytevector-like-ref b i))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 1)))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 2)))
             (finish i))
            ((< 0 (bytevector-like-ref b (+ i 3)))
             (finish i))
            (else
             (loop (- i 4)))))
    (define (finish i)
      (bignum-length32-set! b (fxrshl i 2))
      b)
    (loop (fxlsh n 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Transitional code for 32-bit bigits.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns the length of a bignum in 32-bit bigits, not 16-bit.

(define (bignum-length32 b)
  (+ (bytevector-like-ref b 0)
     (fxlsh (bytevector-like-ref b 1) 8)
     (fxlsh (bytevector-like-ref b 2) 16)))

; Sets the length of a bignum (in units of 32-bit bigits, not 16-bit).

(define (bignum-length32-set! b l)
  (let* ((b0 (fxlogand l #xff))
         (t1 (fxrshl l 8))
         (b1 (fxlogand t1 #xff))
         (b2 (fxrshl t1 8)))
    (bytevector-like-set! b 0 b0)
    (bytevector-like-set! b 1 b1)
    (bytevector-like-set! b 2 b2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Endianness-dependent inner loops for
;     shift left
;     shift right
;     addition
;     subtraction
;     multiplication
;
; All of these inner loops operate on 32-bit bigits.
;
; FIXME:  All would be faster and simpler if certain
; bytevector operations were implemented as primitives.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Shift left.
; Stores b, left-shifted by k bits, into c.
; At the outset, c is assumed to be full of zero digits,
; or the same as b.
; The length of c is neither examined nor adjusted.
; No range checking is performed, so c must be large enough.

(define (bignum-shift-left! b c k)
  (let* ((n (bignum-length32 b))        ; 32-bit bigits
         (k8 (quotient k 8))
         (k32 (fxrshl k8 2))
         (k32bits (remainder k 32))
         (k8bits (fxlogand k32bits #x07)))

    ;; Given the bytevector index of the most significant byte
    ;; that has not yet been cleared, finishes the job.

    (define (zero-loop i)
      (cond ((< i 4)
             c)
            ((= 0 (remainder i 4))
             (bytevector-like-set! c i 0)
             (fast-zero-loop (- i 4)))
            (else
             (bytevector-like-set! c i 0)
             (zero-loop (- i 1)))))

    ;; Given the bytevector index of the most significant 32-bit
    ;; digit that has not yet been cleared, finishes the job.

    (define (fast-zero-loop i)
      (cond ((= i 0)
             c)
            (else
             (bytevector-like-set! c i 0)
             (bytevector-like-set! c (+ i 1) 0)
             (bytevector-like-set! c (+ i 2) 0)
             (bytevector-like-set! c (+ i 3) 0)
             (fast-zero-loop (- i 4)))))

    ;; Given the bytevector index of the most significant
    ;; 32-bit digit that has not yet been shifted, finishes the job.
    ;; The shift count is a multiple of 32 bits.

    (define (fast-loop i)
      (if (= i 0)
          (fast-zero-loop k8)
          (let ((b0 (bytevector-like-ref b i))
                (b1 (bytevector-like-ref b (+ i 1)))
                (b2 (bytevector-like-ref b (+ i 2)))
                (b3 (bytevector-like-ref b (+ i 3))))
            (bytevector-like-set! c (+ i k8) b0)
            (bytevector-like-set! c (+ i k8 1) b1)
            (bytevector-like-set! c (+ i k8 2) b2)
            (bytevector-like-set! c (+ i k8 3) b3)
            (fast-loop (- i 4)))))

    ;; Given the bytevector index of the most significant byte
    ;; that has not yet been shifted, finishes the job.
    ;; The shift count is a multiple of 8 bits.

    (define (fast-byte-loop i)
      (if (< i 4)
          (zero-loop (+ 3 k8))
          (let ((b0 (bytevector-like-ref b i)))
            (bytevector-like-set! c (+ i k8) b0)
            (fast-byte-loop (- i 1)))))

    ;; Given the bytevector index of the most significant byte
    ;; that has not yet been shifted, finishes the job.
    ;; The shift count is not a multiple of 8 bits.

    (define (slow-loop i)
      (if (< i 4)
          (zero-loop (+ 3 k8))
          (let ((b0 (bytevector-like-ref b i))
                (mask (fxlsh #xff k8bits)))
            (bytevector-like-set! c
                                  (+ i k8)
                                  (fxlogand #xff (fxlsh b0 k8bits)))
            (let ((extra-bits (fxrshl (fxlsh b0 k8bits) 8)))
              (if (> extra-bits 0)
                  (let ((c1 (bytevector-like-ref c (+ (+ i k8) 1))))
                    (bytevector-like-set! c
                                          (+ (+ i k8) 1)
                                          (fxlogior (fxlogand c1 mask)
                                                    extra-bits)))))
            (slow-loop (- i 1)))))

    (cond ((< k 0)
           (assertion-violation 'bignum-shift-left! (errmsg msg:notindex) k))
          ((= 0 k32bits)
           (fast-loop (fxlsh n 2)))
          ((= 0 k8bits)
           (fast-byte-loop (+ 3 (fxlsh n 2))))
          (else
           (slow-loop (+ 3 (fxlsh n 2)))))))

; Shift right.
; Stores b, right-shifted by k bits, into c.
; At the outset, c is assumed to be full of zero digits,
; or the same as b.
; The length of c is examined (for zero-fill) but not adjusted.
; No range checking is performed, so c must be large enough.

(define (bignum-shift-right! b c k)
  (let* ((n (bignum-length32 b))        ; 32-bit bigits
         (nbytes (+ 4 (* 4 n)))         ; limit of byte index for loops
         (nbytes-c (+ 4 (* 4 (bignum-length32 c))))
         (k8 (quotient k 8))
         (k32 (fxrshl k8 2))
         (k32bits (remainder k 32))
         (k8bits (fxlogand k32bits #x07)))

    ;; Given the bytevector index of the most significant byte
    ;; that has not yet been cleared, finishes the job.

    (define (zero-loop i)
      (cond ((>= i nbytes-c)
             c)
            ((= 0 (fxlogand i #x03))
             (fast-zero-loop i))
            (else
             (bytevector-like-set! c i 0)
             (zero-loop (+ i 1)))))

    ;; Given the bytevector index of the most significant 32-bit
    ;; digit that has not yet been cleared, finishes the job.

    (define (fast-zero-loop i)
      (cond ((>= i nbytes-c)
             c)
            (else
             (bytevector-like-set! c i 0)
             (bytevector-like-set! c (+ i 1) 0)
             (bytevector-like-set! c (+ i 2) 0)
             (bytevector-like-set! c (+ i 3) 0)
             (fast-zero-loop (+ i 4)))))

    ;; Given the bytevector index of the least significant
    ;; 32-bit digit that has not yet been shifted, finishes the job.
    ;; The shift count is a multiple of 32 bits.

    (define (fast-loop i)
      (if (>= i nbytes)
          (fast-zero-loop (max 4 (- i k8)))
          (let ((b0 (bytevector-like-ref b i))
                (b1 (bytevector-like-ref b (+ i 1)))
                (b2 (bytevector-like-ref b (+ i 2)))
                (b3 (bytevector-like-ref b (+ i 3))))
            (bytevector-like-set! c (- i k8) b0)
            (bytevector-like-set! c (+ (- i k8) 1) b1)
            (bytevector-like-set! c (+ (- i k8) 2) b2)
            (bytevector-like-set! c (+ (- i k8) 3) b3)
            (fast-loop (+ i 4)))))

    ;; Given the bytevector index of the least significant byte
    ;; that has not yet been shifted, finishes the job.
    ;; The shift count is a multiple of 8 bits.

    (define (fast-byte-loop i)
      (if (>= i nbytes)
          (zero-loop (max 4 (- i k8)))
          (let ((b0 (bytevector-like-ref b i)))
            (bytevector-like-set! c (- i k8) b0)
            (fast-byte-loop (+ i 1)))))

    ;; Given the bytevector index of the least significant byte
    ;; that has not yet been shifted, finishes the job.
    ;; The shift count is not a multiple of 8 bits.

    (define (slow-loop i)
      (if (>= i nbytes)
          (zero-loop (max 4 (- i k8)))
          (let ((b0 (bytevector-like-ref b i))
                (c1 (bytevector-like-ref c (- (- i k8) 1)))
                (mask (fxrshl #xff k8bits)))
            (bytevector-like-set! c (- i k8) (fxrshl b0 k8bits))
            (bytevector-like-set! c
                                  (- (- i k8) 1)
                                  (fxlogior (fxlogand c1 mask)
                                            (fxlsh b0 (- 8 k8bits))))
            (slow-loop (+ i 1)))))

    (cond ((< k 0)
           (assertion-violation 'bignum-shift-right! (errmsg msg:notindex) k))
          ((= 0 k32bits)
           (fast-loop (+ 4 (* 4 k32))))
          ((= 0 k8bits)
           (fast-byte-loop (+ 4 k8)))
          ((< (+ 4 k8) nbytes)
           (let ((b0 (bytevector-like-ref b (+ 4 k8))))
              (bytevector-like-set! c
                                    4
                                    (fxlogand #xff (fxrshl b0 k8bits))))
           (slow-loop (+ 5 k8)))
          (else
           (fast-zero-loop 4)))))

; Add the high-order bits of one bignum to another, destructively.
; This is the inner loop for classical addition.
;
; Given bignums b and c, indexes i and j (to be interpreted as
; indexes into 0-origin arrays of 32-bit bigits, not as indexes
; into bytevectors or arrays of 16-bit bigits), adds b[i...]
; to c[j...].
;
; Normally i=j, but letting j=i+k or j=i-k effectively shifts
; b left or right by k 32-bit digits before performing the
; addition.
;
; The signs of b and c are not examined.
; The length of b determines when the loop is terminated.
; The length of c is neither examined nor adjusted.
; No range checking is performed, so c must be large enough.

(define (bignum-add! b c i j)
  (let* ((n (bignum-length32 b)) ; n 16-bit bigits
         (nbytes (+ 4 (fxlsh n 2)))
         (debugging #f))

    ;; j is even, and carry is 0 or 1.

    (define (carry-loop16 c j carry)
      (cond ((= 0 carry)
             c)
            (else
             (let* ((cj0 (bytevector-like-ref c j))
                    (cj1 (bytevector-like-ref c (+ j 1)))

                    (t0  (+ cj0 carry))
                    (cj0 (fxlogand #xff t0))
                    (t1  (+ cj1 (fxrshl t0 8)))
                    (cj1 (fxlogand #xff t1)))

               (bytevector-like-set! c j cj0)
               (bytevector-like-set! c (+ j 1) cj1)
               (carry-loop16 c (+ j 2) (fxrshl t1 8))))))

    ;; Given the byte index of the next 32-bit word in b and c,
    ;; and a 1-bit carry, sets c[j] and the following bytes of c
    ;; to the sum of those bytes and b[i...] and the carry.

    (define (loop b c i j nbytes carry)
      (if debugging
          (begin (write b)
                 (newline)
                 (write c)
                 (newline)
                 (write (list 'khi= khi 'klo= klo))
                 (newline)
                 (write
                  (list 'i= i 'j= j 'carry-hi= carry-hi 'carry-lo= carry-lo))
                 (newline)))
      (cond ((>= i nbytes)
             (carry-loop16 c j carry))
            (else
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
               (loop b c (+ i 4) (+ j 4) nbytes carry)))))

    (loop b c (+ 4 (* 4 i)) (+ 4 (* 4 j)) nbytes 0)
    c))

; Subtract the high-order bits of one bignum from another, destructively.
; This is the inner loop for classical subtraction.
;
; Given bignums b and c (with the magnitude of b less than the
; magnitude of c), indexes i and j (to be interpreted as indexes
; into 0-origin arrays of 32-bit bigits, not as indexes into
;  bytevectors or arrays of 16-bit bigits), subtracts b[i...]
; from c[j...].
;
; Normally i=j, but letting j=i+k or j=i-k effectively shifts
; b left or right by k 32-bit digits before performing the
; subtraction.
;
; The signs of b and c are not examined.
; The length of b determines when the loop is terminated.
; The length of c is neither examined nor adjusted.
; No range checking is performed, so c must be large enough.

(define (bignum-subtract! b c i j)
  (let* ((n (bignum-length32 b)) ; n 16-bit bigits
         (nbytes (+ 4 (fxlsh n 2)))
         (debugging #f))

    ;; j is even, and borrow is 0 or 1.

    (define (borrow-loop16 c j borrow)
      (cond ((= 0 borrow)
             c)
            (else
             (let* ((cj0 (bytevector-like-ref c j))
                    (cj1 (bytevector-like-ref c (+ j 1)))

                    (t0  (- cj0 borrow))
                    (cj0 (fxlogand #xff t0))
                    (t1  (+ cj1 (fxrsha t0 8)))
                    (cj1 (fxlogand #xff t1)))

               (bytevector-like-set! c j cj0)
               (bytevector-like-set! c (+ j 1) cj1)
               (if (< t1 0)
                   (borrow-loop16 c (+ j 2) 1)
                   c)))))

    ;; Given the byte index of the next 32-bit word in b and c,
    ;; and a 1-bit borrow, sets c[j] and the following bytes of c
    ;; to the sum of those bytes and b[i...] and the borrow.

    (define (loop b c i j nbytes borrow)
      (if debugging
          (begin (write b)
                 (newline)
                 (write c)
                 (newline)
                 (write (list 'khi= khi 'klo= klo))
                 (newline)
                 (write
                  (list 'i= i 'j= j 'borrow-hi= borrow-hi 'borrow-lo= borrow-lo))
                 (newline)))
      (cond ((>= i nbytes)
             (borrow-loop16 c j borrow))
            (else
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
               (loop b c (+ i 4) (+ j 4) nbytes borrow)))))

    (loop b c (+ 4 (* 4 i)) (+ 4 (* 4 j)) nbytes 0)
    c))

; Multiply by scalar and add.
; This is the inner loop for classical multiplication.
;
; Given bignums b and c, indexes i and j (to be interpreted as
; indexes into 0-origin arrays of 32-bit bigits, not as indexes
; into bytevectors or arrays of 16-bit bigits), and the high
; and low 16 bits of a 32-bit scalar multiplier k, multiplies
; b[i...] by k and adds those products to c[j...].
;
; The signs of b and c are not examined.
; The length of b determines when the loop is terminated.
; The length of c is neither examined nor adjusted.
; No range checking is performed, so c must be large enough.
;
; Recognizes the special case where k is 0.

(define (bignum-multiply-by-scalar-and-add! b c i j khi klo)
  (let* ((n (bignum-length32 b)) ; n 32-bit digits
         (nbytes (+ n n n n 4))
         (debugging #f))

    (define (carry-loop c j carry-hi carry-lo)
      (cond ((= 0 carry-hi)
             (carry-loop16 c j carry-lo))
            (else
             (let* ((cj0 (bytevector-like-ref c j))
                    (cj1 (bytevector-like-ref c (+ j 1)))
                    (cj2 (bytevector-like-ref c (+ j 2)))
                    (cj3 (bytevector-like-ref c (+ j 3)))

                    (t0  (+ cj0 (fxlogand #xff carry-lo)))
                    (cj0 (fxlogand #xff t0))
                    (t1  (+ cj1 (fxrshl t0 8) (fxrshl carry-lo 8)))
                    (cj1 (fxlogand #xff t1))
                    (t2  (+ cj2 (fxrshl t1 8) (fxlogand #xff carry-hi)))
                    (cj2 (fxlogand #xff t2))
                    (t3  (+ cj3 (fxrshl t2 8) (fxrshl carry-hi 8)))
                    (cj3 (fxlogand #xff t3)))

               (bytevector-like-set! c j cj0)
               (bytevector-like-set! c (+ j 1) cj1)
               (bytevector-like-set! c (+ j 2) cj2)
               (bytevector-like-set! c (+ j 3) cj3)
               (carry-loop16 c (+ j 4) (fxrshl t3 8))))))

    (define (carry-loop16 c j carry)
      (cond ((= 0 carry)
             c)
            (else
             (let* ((cj0 (bytevector-like-ref c j))
                    (cj1 (bytevector-like-ref c (+ j 1)))

                    (t0  (+ cj0 (fxlogand #xff carry)))
                    (cj0 (fxlogand #xff t0))
                    (t1  (+ cj1 (fxrshl t0 8) (fxrshl carry 8)))
                    (cj1 (fxlogand #xff t1)))

               (bytevector-like-set! c j cj0)
               (bytevector-like-set! c (+ j 1) cj1)
               (carry-loop16 c (+ j 2) (fxrshl t1 8))))))

    ;; Given the byte index of the next 32-bit word in b and c,
    ;; and both the high-order and low-order 16 bits of a 32-bit
    ;; carry, sets c[j] and the following bytes of c to the sum of
    ;; the c[j] and the carry and the product of k with b[i] and
    ;; the following bytes of b.

    (define (loop b c i j nbytes khi klo carry-hi carry-lo)
      (if debugging
          (begin (write b)
                 (newline)
                 (write c)
                 (newline)
                 (write (list 'khi= khi 'klo= klo))
                 (newline)
                 (write
                  (list 'i= i 'j= j 'carry-hi= carry-hi 'carry-lo= carry-lo))
                 (newline)))
      (cond ((>= i nbytes)
             (carry-loop c j carry-hi carry-lo))
            (else
             (let* ((bi0 (bytevector-like-ref b i))
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

               (bytevector-like-set! c j cj0)
               (bytevector-like-set! c (+ j 1) cj1)
               (bytevector-like-set! c (+ j 2) cj2)
               (bytevector-like-set! c (+ j 3) cj3)
               (loop b c (+ i 4) (+ j 4) nbytes khi klo carry-hi carry-lo)))))

    (cond ((and (= 0 khi) (= 0 klo))
           c)
          (else
           (loop b c (+ 4 (* 4 i)) (+ 4 (* 4 j)) nbytes khi klo 0 0)))
    c))

; eof
