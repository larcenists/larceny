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
; The sign is kept in the upper two bytes and is 0 (positive) or 1 (negative).
; The bignum-length is kept in the lower two bytes and denotes the number
; of 32-bit "bigits" in the number.  
;
; If the bignum's value is zero, then the sign is ignored and the length
; field must be 0.  Zero-valued bignums should never be allowed to be
; visible to user code.
;
; The following code operates on 16-bit bigits since these fit 
; conveniently in a fixnum.  Thus a 32-bit bigit is split into two 
; 16-bit bigits.  This results in a little hair, as the length field 
; still denotes 32-bit bigits.
;
; FIXME (see similar comments in bignums-be.sch):
;  - Replace bignum-length-set! with the code from bignum-truncate-length!
;    and remove references to the latter.  Haven't done this because it
;    needs careful testing.
;  - Replace slow definition of big-fits-in-fix? with faster code.  Must
;    also be tested carefully.

($$trace "bignums-el")

(define (bignum-sign b)
  (bytevector-like-ref b 2))

(define (bignum-sign-set! b s)
  (bytevector-like-set! b 2 s))

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
      (bytevector-like-halfword-set! b 0 l)))

  ; This is like bignum-length-set!, except that it works also when the
  ; length is odd and the most significant half of the 32-bit bigit is not 
  ; zero, i.e., in that case, normalize would not work properly unless said
  ; half is zeroed out.  If this seems like a hack to you, you're right.
  ; I think the most reasonable thing to do would be to make this procedure
  ; the definition of bignum-length-set!.

  (define (%bignum-truncate-length! b ln)
    (let ((l (fxrsha (+ ln 1) 1)))
      (bytevector-like-halfword-set! b 0 l)
      (if (not (= ln (+ l l)))
          (bignum-set! b ln 0))))

  (set! bignum-ref %bignum-ref)
  (set! bignum-set! %bignum-set!)
  (set! bignum-length %bignum-length)
  (set! bignum-length-set! %bignum-length-set!)
  (set! bignum-truncate-length! %bignum-truncate-length!)
  #t)

; A bignum fits in fixnum if it has no more than two bigits, and if 
; it has exactly two bigits then the high three bits of the high bigit
; must be equal.
;
; FIXME: would be nice to weaken dependence on rather complex bignum-length.
; FIXME: bignum-ref can be replaced by bytevector-like-ref with known offset;
;        must then use a different shift count (5).

;(define (big-fits-in-fix? b)
;  (let ((s (bignum-length b)))
;    (cond ((< s 2)
;           #t)
;          ((= s 2)
;           (let* ((x (bignum-ref b 1))
;                  (y (fxrshl x 13)))
;             (cond ((= y 0) #t)
;                   ((= y 7) #t)
;                   (else    #f))))
;          (else
;           #f))))

; Old big-fits-in-fix? test; rather slow.

(define (big-fits-in-fix? b)
  (and (bignum>? b largest-negative-bignum)
       (bignum<? b smallest-positive-bignum)))

; eof
