; Asm/Sparc/asmutil32.sch
; Larceny assembler -- 32-bit endianness-independent utility procedures.
;
; $Id$
;
; 32-bit numbers are represented as 4-byte bytevectors where the
; exact layout depends on whether the little-endian or big-endian
; module has been loaded.  One of them must be loaded prior to loading
; this module.
;
; Logically, the 'big' end is on the left and the 'little' end
; is on the right, so a left shift shifts towards the big end.
;
; Generally, performance is not a major issue in this module.  The 
; assemblers should use more specialized code for truly good performance.
; These procedures are mainly suitable for one-time construction of 
; instruction templates, and during development.
;
; Endian-ness specific operations are in asmutil32be.sch and asmutil32le.sch:
;
;   (asm:bv n0 n1 n2 n3)    ; Construct bytevector
;   (asm:bv->int bv)        ; Convert bytevector to integer
;   (asm:lsh m k)           ; Shift left logical k bits
;   (asm:rshl m k)          ; Shift right logical k bits
;   (asm:rsha m k)          ; Shirt right arithmetic k bits


; Convert an integer to a length-4 bytevector using two's complement 
; representation for negative numbers.
; Returns length-4 bytevector.
;
; The procedure handles numbers in the range -2^31..2^32-1 [sic].
; It is an error for the number to be outside this range.
;
; FIXME: quotient/remainder may be slow; we could have special fixnum
;        case that uses shifts (that could be in-lined as macro).  It could
;        work for negative numbers too.
; FIXME: should probably check that the number is within range.

(define asm:int->bv
  (let ((two^32 (expt 2 32)))
    (lambda (m)
      (let* ((m  (if (< m 0) (+ two^32 m) m))
	     (b0 (remainder m 256))
	     (m  (quotient m 256))
	     (b1 (remainder m 256))
	     (m  (quotient m 256))
	     (b2 (remainder m 256))
	     (m  (quotient m 256))
	     (b3 (remainder m 256)))
	(asm:bv b3 b2 b1 b0)))))


; `Or' the bits of multiple operands together. 
; Each operand may be an exact integer or a length-4 bytevector.
; Returns a length-4 bytevector.

(define (asm:logior . ops)
  (let ((r (asm:bv 0 0 0 0)))
    (do ((ops ops (cdr ops)))
	((null? ops) r)
      (let* ((op (car ops))
	     (op (if (bytevector? op) op (asm:int->bv op))))
	(bytevector-set! r 0 (logior (bytevector-ref r 0)
				     (bytevector-ref op 0)))
	(bytevector-set! r 1 (logior (bytevector-ref r 1)
				     (bytevector-ref op 1)))
	(bytevector-set! r 2 (logior (bytevector-ref r 2)
				     (bytevector-ref op 2)))
	(bytevector-set! r 3 (logior (bytevector-ref r 3)
				     (bytevector-ref op 3)))))))


; `And' the bits of two operands together.
; Either may be an exact integer or length-4 bytevector.
; Returns length-4 bytevector.

(define (asm:logand op1 op2)
  (let ((op1 (if (bytevector? op1) op1 (asm:int->bv op1)))
	(op2 (if (bytevector? op2) op2 (asm:int->bv op2)))
	(bv  (make-bytevector 4)))
    (bytevector-set! bv 0 (logand (bytevector-ref op1 0)
				  (bytevector-ref op2 0)))
    (bytevector-set! bv 1 (logand (bytevector-ref op1 1)
				  (bytevector-ref op2 1)))
    (bytevector-set! bv 2 (logand (bytevector-ref op1 2)
				  (bytevector-ref op2 2)))
    (bytevector-set! bv 3 (logand (bytevector-ref op1 3)
				  (bytevector-ref op2 3)))
    bv))


; Extract the n low-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 32.
; Returns length-4 bytevector.
;
; Does not depend on endian-ness.

(define asm:lobits 
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (asm:int->bv (- (expt 2 i) 1))))
    (lambda (m n)
      (asm:logand m (vector-ref v (remainder n 33))))))

; Extract the n high-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 33.
; Returns length-4 bytevector with the high-order bits of m at low end.
;
; Does not depend on endian-ness.

(define (asm:hibits m n)
  (asm:rshl m (- 32 (remainder n 33))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; signed slot.
;
; Does not depend on endian-ness.

(define asm:fits?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= (- (vector-ref v (- n 1))) m (- (vector-ref v (- n 1)) 1)))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; unsigned slot.
;
; Does not depend on endian-ness.

(define asm:fits-unsigned?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= 0 m (- (vector-ref v n) 1)))))

; Add two operands (numbers or bytevectors).
;
; Does not depend on endian-ness.

(define (asm:add a b)
  (asm:int->bv (+ (if (bytevector? a) (asm:bv->int a) a)
		  (if (bytevector? b) (asm:bv->int b) b))))

; Given an unsigned 32-bit number, return it as a signed number
; as appropriate.
;
; Does not depend on endian-ness.

(define (asm:signed n)
  (if (< n 2147483647)
      n
      (- n 4294967296)))


(define (asm:print-bv bv)

  (define hex "0123456789abcdef")

  (define (pdig k)
    (display (string-ref hex (quotient k 16)))
    (display (string-ref hex (remainder k 16)))
    (display " "))
  
  (if (eq? asm:endianness 'little)
      (do ((i 3 (- i 1)))
	  ((< i 0))
	(pdig (bytevector-ref bv i)))
      (do ((i 0 (+ i 1)))
	  ((= i 4))
	(pdig (bytevector-ref bv i)))))


; eof
