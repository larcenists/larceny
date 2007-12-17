; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC code generation macros for primitives, part 3b:
;   helper procedures for data structures.


; SET-CAR!, SET-CDR!, CELL-SET!
;
; Input:  RS1: a hardware register; has pair pointer (tag check must be
;         performed by the caller).
;         RS2: any register; has value to store.
; Output: None.
;
; Having rs1 != RESULT is pretty silly with the current write barrier
; but will be less silly with the new barrier.

(define (emit-setcar/setcdr! as rs1 rs2 offs)
  (cond ((and (write-barrier) (hardware-mapped? rs2))
	 (sparc.sti as rs2 (- offs $tag.pair-tag) rs1)
         (if (not (= rs1 $r.result))
             (sparc.move as rs1 $r.result))
         (millicode-call/1arg as $m.addtrans rs2))
        ((write-barrier)
         (emit-move2hwreg! as rs2 $r.argreg2)
         (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1)
         (millicode-call/1arg-in-result as $m.addtrans rs1))
        (else
         (emit-setcar/setcdr-no-barrier! as rs1 rs2 offs))))

(define (emit-setcar/setcdr-no-barrier! as rs1 rs2 offs)
  (cond ((hardware-mapped? rs2)
         (sparc.sti as rs2 (- offs $tag.pair-tag) rs1))
        (else
         (emit-move2hwreg! as rs2 $r.argreg2)
         (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1))))


; Representation predicate.
;
; RESULT has an object.  If the tag of RESULT is 'tag1' and the 
; header byte of the object is 'tag2' then set RESULT to #t, else
; set it to #f.

(define (emit-double-tagcheck->bool! as tag1 tag2)
  (let ((L1 (new-label)))
    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.ldbi  as $r.result (+ (- tag1) 3) $r.tmp0)
    (sparc.set   as $imm.true $r.result)
    (sparc.cmpi  as $r.tmp0 tag2)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.label as L1)))


; Check structure tag.
;
; RS1 has an object.  If the tag of RS1 is not 'tag1', or if the tag is 
; 'tag1' but the header byte of the object header is not 'tag2', then an
; exception with code 'excode' is signalled.  The exception call is set
; up to return to the first instruction of the emitted code.
;
; If RS1 is not RESULT then it is moved to RESULT before the exception 
; is signalled.
;
; If RS2/IMM is not #f, then it is a register or immediate that is moved
; to ARGREG2 before the exception is signalled; it is an immediate iff 
; imm? = #t.  
;
; RS1 must be a hardware register.
; RS2/IMM is a general register, ARGREG2, an immediate, or #f.
; RS3 is a general register, ARGREG3, or #f.
;
; The procedure returns the label of the fault address.  If the execution
; falls off the end of the emitted instruction sequence, then the following
; are true:
;  - the tag of the object in RS1 was 'tag1' and its header byte was 'tag2'
;  - the object header word is in TMP0.

(define (double-tagcheck-assert as tag1 tag2 rs1 rs2/imm rs3 excode imm?)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as rs1 $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.be.a  as L1)
    (sparc.ldi   as rs1 (- tag1) $r.tmp0)
    (sparc.label as FAULT)
    (if (not (= rs1 $r.result))
        (sparc.move as rs1 $r.result))
    (if rs2/imm 
        (cond (imm?
               (sparc.set as (thefixnum rs2/imm) $r.argreg2))
              ((= rs2/imm $r.argreg2))
              (else
               (emit-move2hwreg! as rs2/imm $r.argreg2))))
    (if (and rs3 (not (= rs3 $r.argreg3)))
        (emit-move2hwreg! as rs3 $r.argreg3))
    (sparc.set   as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L0)
    (sparc.label as L1)
    (sparc.andi  as $r.tmp0 255 $r.tmp1)
    (sparc.cmpi  as $r.tmp1 tag2)
    (sparc.bne.a as FAULT)
    (sparc.slot  as)
    FAULT))

(define (emit-double-tagcheck-assert! as tag1 tag2 excode reg2)
  (double-tagcheck-assert as tag1 tag2 $r.result reg2 #f excode #f))

(define (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs1 rs2 excode)
  (double-tagcheck-assert as tag1 tag2 rs1 rs2 #f excode #f))
  
(define (emit-double-tagcheck-assert-reg/imm! as tag1 tag2 rs1 imm excode)
  (double-tagcheck-assert as tag1 tag2 rs1 imm #f excode #t))
  



; Get the length of a vector or bytevector structure, with tag checking
; included.
;
; Input: RS and RD are both hardware registers.

(define (emit-get-length! as tag1 tag2 excode rs rd)
  (if (not (unsafe-code))
      (if tag2
          (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs rd excode)
          (emit-single-tagcheck-assert-reg! as tag1 rs rd excode)))
  (emit-get-length-trusted! as tag1 rs rd))

; Get the length of a vector or bytevector structure, without tag checking.
;
; Input: RS and RD are both hardware registers.

(define (emit-get-length-trusted! as tag1 rs rd)
  (sparc.ldi  as rs (- tag1) $r.tmp0)
  (sparc.srli as $r.tmp0 8 rd)
  (if (= tag1 $tag.bytevector-tag)
      (sparc.slli as rd 2 rd)))


; Allocate a bytevector, leave untagged pointer in RESULT.
;
; The preserved-result is #f or a register that contains
; a copy of the fixnum byte count passed in RESULT.
;
; FIXME: The overflow checking code could be merged with tag
;        checking, moved into millicode, or moved into the RTS
;        to get it off the fast path?

(define (emit-allocate-bytevector as hdr preserved-result)

  ; Preserve the length field, then calculate the number of words
  ; to allocate.  The value `28' is an adjustment of 3 (for rounding 
  ; up) plus another 4 bytes for the header, all represented as a fixnum.
  ;
  ; The adjustment may overflow the size field.  In that case,
  ; we pass the number unadjusted and let the RTS clean it up -- 
  ; the size is in any case too large, and the system will panic.

  (let ((Lok (new-label)))
    (if (not preserved-result)
        (sparc.move as $r.result $r.argreg2))
    (sparc.addicc as $r.result 28 $r.tmp0)
    (sparc.bvc.a  as Lok)
    (sparc.andi   as $r.tmp0 (asm:signed #xFFFFFFF0) $r.result)
    (sparc.label  as Lok)

    ; Allocate space

    (sparc.jmpli as $r.millicode $m.alloc-bv $r.o7)
    (sparc.srai  as $r.result 2 $r.result)
  
    ; Setup the header.
    
    (if (not preserved-result)
        (sparc.slli as $r.argreg2 6 $r.tmp0)
        (sparc.slli as preserved-result 6 $r.tmp0))
    (sparc.addi as $r.tmp0 hdr $r.tmp0)
    (sparc.sti  as $r.tmp0 0 $r.result)))


; Given a nativeint count, a pointer to the first element of a 
; bytevector-like structure, and a byte value, fill the bytevector
; with the byte value.

(define (emit-bytevector-fill as r-bytecount r-pointer r-value)
  (let ((L2 (new-label))
        (L1 (new-label)))
    (sparc.label  as L2)
    (sparc.deccc  as r-bytecount)
    (sparc.bge.a  as L2)
    (sparc.stbr   as r-value r-bytecount r-pointer)
    (sparc.label  as L1)))


; Given a nativeint count, a pointer to the first element of a 
; bytevector-like structure, and a 4-byte value, fill the bytevector
; with the 4-byte value.
; (Used by make-ustring.)

(define (emit-bytevector-fill4 as r-bytecount r-pointer r-value)
  (let ((L2 (new-label))
        (L1 (new-label)))
    (sparc.label  as L2)
    (sparc.subicc as r-bytecount 4 r-bytecount)
    (sparc.bge.a  as L2)
    (sparc.str    as r-value r-bytecount r-pointer)
    (sparc.label  as L1)))


; BYTEVECTOR-REF, BYTEVECTOR-LIKE-REF, STRING-REF.
;
; The pointer in RS1 is known to be bytevector-like.  RS2 is the fixnum
; index into the structure.  Get the RS2'th element and place it in RD.
;
; RS1 and RD are hardware registers.
; RS2 is a general register or ARGREG2.
; 'fault' is defined iff (unsafe-code) = #f
; header is in TMP0 iff (unsafe-code) = #f and 'header-loaded?' = #t
; if 'charize?' is #t then store result as char, otherwise as fixnum.

(define (emit-bytevector-like-ref! as rs1 rs2 rd fault charize? header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (begin
          ; check that index is fixnum
          (sparc.btsti  as rs2 3)
          (sparc.bne    as fault)
          (if (not header-loaded?)
              (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
          ; check length
          (sparc.srai   as rs2 2 $r.tmp1)
          (sparc.srli   as $r.tmp0 8 $r.tmp0)
          (sparc.cmpr   as $r.tmp0 $r.tmp1)
          (sparc.bleu as fault)
          ; No NOP or SLOT -- the SUBI below goes into the slot.
          )
        (begin
          (sparc.srai   as rs2 2 $r.tmp1)))
    ; Pointer is in RS1.
    ; Shifted index is in TMP1.
    (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
    (sparc.ldbr as $r.tmp0 $r.tmp1 $r.tmp0)
    (if (not charize?)
        (sparc.slli as $r.tmp0 2 rd)
        (begin (sparc.slli as $r.tmp0 8 rd)
               (sparc.ori  as rd $imm.character rd)))))

; As above, but RS2 is replaced by an immediate, IMM.
;
; The immediate, represented as a fixnum, is guaranteed fit in the 
; instruction's immediate field.

(define (emit-bytevector-like-ref/imm! as rs1 imm rd fault charize?
                                       header-loaded?)
  (if (not (unsafe-code))
      (begin
        (if (not header-loaded?)
            (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
        ; Range check.
        (sparc.srli   as $r.tmp0 8 $r.tmp0)
        (sparc.cmpi   as $r.tmp0 imm)
        (sparc.bleu.a as fault)
        (sparc.slot   as)))

  ; Pointer is in RS1.

  (let ((adjusted-offset (+ (- 4 $tag.bytevector-tag) imm)))
    (if (immediate-literal? adjusted-offset)
        (begin
          (sparc.ldbi as rs1 adjusted-offset $r.tmp0))
        (begin
          (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
          (sparc.ldbr as $r.tmp0 imm $r.tmp0)))
    (if (not charize?)
        (sparc.slli as $r.tmp0 2 rd)
        (begin (sparc.slli as $r.tmp0 8 rd)
               (sparc.ori  as rd $imm.character rd)))))

(define (emit-bytevector-like-ref-trusted! as rs1 rs2 rd charize?)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (sparc.srai   as rs2 2 $r.tmp1)
    ; Pointer is in RS1.
    ; Shifted index is in TMP1.
    (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
    (sparc.ldbr as $r.tmp0 $r.tmp1 $r.tmp0)
    (if (not charize?)
        (sparc.slli as $r.tmp0 2 rd)
        (begin (sparc.slli as $r.tmp0 8 rd)
               (sparc.ori  as rd $imm.character rd)))))

; BYTEVECTOR-SET!, BYTEVECTOR-LIKE-SET!
;
; Input:  RESULT -- a pointer to a bytevector-like structure.
;         TMP0   -- the header iff (unsafe-code) = #f and header-loaded? = #t
;         IDX    -- a register that holds the second argument
;         BYTE   -- a register that holds the third argument
; Output: Nothing.
;
; 'Fault' is the address of the error code iff (unsafe-code) = #f
;
; FIXME: 
;   - There's no check that the value actually fits in a byte.
;   - Uses ARGREG3 and TMP2.

(define (emit-bytevector-like-set! as idx byte fault header-loaded?)
  (let ((r1 (force-hwreg! as idx $r.tmp1))
        (r2 (force-hwreg! as byte $r.argreg3)))
    (if (not (unsafe-code))
        (begin
          (if (not header-loaded?)
              (sparc.ldi     as $r.result (- $tag.bytevector-tag) $r.tmp0))
          ; Both index and byte must be fixnums.  
          ; Can't use tsubcc because the computation may really overflow.
          (sparc.orr     as r1 r2 $r.tmp2)
          (sparc.btsti   as $r.tmp2 3)
          (sparc.bnz     as fault)
          (if (not (eq? r2 $r.argreg3))
              (sparc.move as r2 $r.argreg3))
          ; No NOP -- next instruction is OK in slot.
          ; Index must be in range.
          (sparc.srli    as $r.tmp0 8 $r.tmp0)    ; limit - in slot
          (sparc.srai    as r1 2 $r.tmp1)         ; index
          (sparc.cmpr    as $r.tmp1 $r.tmp0)
          (sparc.bgeu    as fault)
          ; No NOP -- next instruction is OK in slot.
          )
        (begin
          (sparc.srai   as r1 2 $r.tmp1)))
    (sparc.srli as r2 2 $r.tmp0)
    ; Using ARGREG2 as the destination is OK because the resulting pointer
    ; value always looks like a fixnum.  By doing so, we avoid needing TMP2.
    (sparc.addi as $r.result (- 4 $tag.bytevector-tag) $r.argreg2)
    (sparc.stbr as $r.tmp0 $r.tmp1 $r.argreg2)))


; STRING-SET!

(define (emit-string-set! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
         (rs3 (force-hwreg! as rs3 $r.argreg3))
         (FAULT (if (not (unsafe-code))
                    (double-tagcheck-assert 
                     as 
                     $tag.bytevector-tag
                     (+ $imm.bytevector-header $tag.string-typetag)
                     rs1 rs2 rs3
                     $ex.sset
                     #f))))
    ; Header is in TMP0; TMP1 and TMP2 are free.
    (if (not (unsafe-code))
        (begin
          ; RS2 must be a fixnum.
          (sparc.btsti  as rs2 3)
          (sparc.bne    as FAULT)
          ; Index (in RS2) must be valid; header is in tmp0.
          (sparc.srli   as $r.tmp0 8 $r.tmp0) ; limit
          (sparc.srai   as rs2 2 $r.tmp1) ; index
          (sparc.cmpr   as $r.tmp1 $r.tmp0)
          (sparc.bgeu   as FAULT)
          ; RS3 must be a character.
          (sparc.andi   as rs3 #xFF $r.tmp0)
          (sparc.cmpi   as $r.tmp0 $imm.character)
          (sparc.bne    as FAULT)
          ; No NOP -- the SRLI below goes in the slot
          )
        (begin
          (sparc.srai as rs2 2 $r.tmp1)))
    ; tmp1 has nativeint index. 
    ; rs3/argreg3 has character.
    ; tmp0 is garbage.
    (sparc.subi as $r.tmp1 (- $tag.bytevector-tag 4) $r.tmp1)
    (sparc.srli as rs3 8 $r.tmp0)
    (sparc.stbr as $r.tmp0 rs1 $r.tmp1)))

(define (emit-string-set-trusted! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
         (rs3 (force-hwreg! as rs3 $r.argreg3)))
    ; Header is in TMP0; TMP1 and TMP2 are free.
    (sparc.srai as rs2 2 $r.tmp1)
    ; tmp1 has nativeint index. 
    ; rs3/argreg3 has character.
    ; tmp0 is garbage.
    (sparc.subi as $r.tmp1 (- $tag.bytevector-tag 4) $r.tmp1)
    (sparc.srli as rs3 8 $r.tmp0)
    (sparc.stbr as $r.tmp0 rs1 $r.tmp1)))

; USTRING-SET!

(define (emit-ustring-set! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
         (rs3 (force-hwreg! as rs3 $r.argreg3))
         (FAULT (if (not (unsafe-code))
                    (double-tagcheck-assert 
                     as 
                     $tag.bytevector-tag
                     (+ $imm.bytevector-header $tag.ustring-typetag)
                     rs1 rs2 rs3
                     $ex.sset
                     #f))))
    ; Header is in TMP0; TMP1 and TMP2 are free.
    (if (not (unsafe-code))
        (begin
          ; RS2 must be a fixnum.
          (sparc.btsti  as rs2 3)
          (sparc.bne    as FAULT)
          ; Index (in RS2) must be valid; header is in tmp0.
          (sparc.srli   as $r.tmp0 8 $r.tmp0) ; limit
          (sparc.cmpr   as rs2 $r.tmp0)
          (sparc.bgeu   as FAULT)
          ; RS3 must be a character.
          (sparc.andi   as rs3 #xFF $r.tmp0)
          (sparc.cmpi   as $r.tmp0 $imm.character)
          (sparc.bne    as FAULT)
          ; No NOP -- the SUBI below goes in the slot
          ))
    ; rs3/argreg3 has character.
    (emit-ustring-set-trusted! as rs1 rs2 rs3)))

(define (emit-ustring-set-trusted! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
         (rs3 (force-hwreg! as rs3 $r.argreg3)))
    ; rs3/argreg3 has character.
    ; tmp0 is garbage.
    (sparc.subi as rs2 (- $tag.bytevector-tag 4) $r.tmp0)
    (sparc.str  as rs3 rs1 $r.tmp0)))

; VECTORS and PROCEDURES

; Allocate short vectors of known length; faster than the general case.
;
; FIXME: can also allocate in-line.

(define (make-vector-n as length r)
  (sparc.jmpli as $r.millicode $m.alloc $r.o7)
  (sparc.set  as (thefixnum (+ length 1)) $r.result)
  (emit-immediate->register! as (+ (* 256 (thefixnum length))
                                   $imm.vector-header
                                   $tag.vector-typetag)
                             $r.tmp0)
  (sparc.sti  as $r.tmp0 0 $r.result)
  (let ((dest (force-hwreg! as r $r.argreg2)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (sparc.sti as dest (* (+ i 1) 4) $r.result)))
  (sparc.addi as $r.result $tag.vector-tag $r.result))


; emit-make-vector-like! assumes argreg3 is not destroyed by alloci.
;
; BUG 141: $ex.mkvl is not right if the operation is make-procedure
; or make-vector.
;
; FIXME: The overflow handling code could move into millicode or be
;        merged with the tag checking?

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((FAULT (emit-assert-positive-fixnum! as $r.result $ex.mkvl)))
    (sparc.move   as $r.result $r.argreg3)
    (sparc.addicc as $r.result 4 $r.result)
    (sparc.bvs.a  as (+ (here as) 8))         ; Overflowed
    (sparc.subi   as $r.result 4 $r.result)   ;   so undo
    (sparc.jmpli as $r.millicode $m.alloci $r.o7)
    (if (null? r)
        (sparc.set as $imm.null $r.argreg2)
        (emit-move2hwreg! as r $r.argreg2))
    (sparc.slli  as $r.argreg3 8 $r.tmp0)
    (sparc.addi  as $r.tmp0 hdr $r.tmp0)
    (sparc.sti   as $r.tmp0 0 $r.result)
    (sparc.addi  as $r.result ptrtag $r.result)))


; VECTOR-REF, VECTOR-LIKE-REF, PROCEDURE-REF
;
; FAULT is valid iff (unsafe-code) = #f
; Header is in TMP0 iff (unsafe-code) = #f and header-loaded? = #t.

(define (emit-vector-like-ref! as rs1 rs2 rd FAULT tag header-loaded?)
  (let ((index (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (begin
         (if (not header-loaded?)
             (sparc.ldi   as rs1 (- tag) $r.tmp0))
         ; Index must be fixnum.
         (sparc.btsti as index 3)
         (sparc.bne   as FAULT)
         ; Index must be within bounds.
         (sparc.srai  as $r.tmp0 8 $r.tmp0)
         (sparc.cmpr  as $r.tmp0 index)
         (sparc.bleu  as FAULT)
         ; No NOP; the following instruction is valid in the slot.
         ))
    (emit-vector-like-ref-trusted! as rs1 index rd tag)))

(define (emit-vector-like-ref-trusted! as rs1 rs2 rd tag)
  (let ((index (force-hwreg! as rs2 $r.argreg2)))
    (sparc.addi as rs1 (- 4 tag) $r.tmp0)
    (sparc.ldr  as $r.tmp0 index rd)))


; VECTOR-REF/IMM, VECTOR-LIKE-REF/IMM, PROCEDURE-REF/IMM
;
; 'rs1' is a hardware register containing a vectorish pointer (to a
;       vector-like or procedure).
; 'imm' is a fixnum s.t. (immediate-literal? imm) => #t.
; 'rd' is a hardware register.
; 'FAULT' is the label of the error code iff (unsafe-code) => #f
; 'tag' is the tag of the pointer in rs1.
; 'header-loaded?' is #t iff the structure header word is in $r.tmp0.

(define (emit-vector-like-ref/imm! as rs1 imm rd FAULT tag header-loaded?)
  (if (not (unsafe-code))
      (begin
        (if (not header-loaded?) (sparc.ldi as rs1 (- tag) $r.tmp0))
        ; Check bounds.
        (sparc.srai  as $r.tmp0 10 $r.tmp0)
        (sparc.cmpi  as $r.tmp0 imm)
        (sparc.bleu  as FAULT)
        (sparc.nop   as)))
  (emit-vector-like-ref/imm-trusted! as rs1 imm rd tag))

; 'rs1' is a hardware register containing a vectorish pointer (to a
;       vector-like or procedure).
; 'imm' is a fixnum s.t. (immediate-literal? imm) => #t.
; 'rd' is a hardware register.
; 'tag' is the tag of the pointer in rs1.

(define (emit-vector-like-ref/imm-trusted! as rs1 imm rd tag)
  (let* ((offset (* imm 4))                       ; words->bytes
         (adjusted-offset (+ (- 4 tag) offset)))
    (if (immediate-literal? adjusted-offset)
        (begin
          (sparc.ldi as rs1 adjusted-offset rd))
        (begin
          (sparc.addi as rs1 (- 4 tag) $r.tmp0)
          (sparc.ldi  as $r.tmp0 offset rd)))))



; VECTOR-SET!, VECTOR-LIKE-SET!, PROCEDURE-SET!
;
; It is assumed that the pointer in RESULT is valid. We must check the index
; in register x for validity and then perform the side effect (by calling
; millicode). The tag is the pointer tag to be adjusted for.
;
; The use of vector-set is ok even if it is a procedure.

; fault is valid iff (unsafe-code) = #f
; header is in tmp0 iff (unsafe-code) = #f and header-loaded? = #t

(define (emit-vector-like-set! as rs1 rs2 rs3 fault tag header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
        (rs3 (force-hwreg! as rs3 $r.argreg2)))
    (if (not (unsafe-code))
        (begin 
         (if (not header-loaded?)
             (sparc.ldi as $r.result (- tag) $r.tmp0))
         (sparc.btsti as rs2 3)
         (sparc.bne   as fault)
         (sparc.srai  as $r.tmp0 8 $r.tmp0)
         (sparc.cmpr  as $r.tmp0 rs2)
         (sparc.bleu  as fault)))
    (emit-vector-like-set-trusted! as rs1 rs2 rs3 tag)))

; rs1 must be a hardware register.
; tag is the pointer tag to be adjusted for.

(define (emit-vector-like-set-trusted! as rs1 rs2 rs3 tag)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
        (rs3 (force-hwreg! as rs3 $r.argreg2)))
    ;; The ADDR can go in the delay slot of a preceding BLEU.
    (sparc.addr as rs1 rs2 $r.tmp0)
    (cond ((not (write-barrier))
           (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
          ((= rs1 $r.result)
           (cond ((= rs3 $r.argreg2)
                  (sparc.jmpli as $r.millicode $m.addtrans $r.o7)
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
                 (else
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (millicode-call/1arg as $m.addtrans rs3))))
          (else
           (cond ((= rs3 $r.argreg2)
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (millicode-call/1arg-in-result as $m.addtrans rs1))
                 (else
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (sparc.move as rs1 $r.result)
                  (millicode-call/1arg as $m.addtrans rs3)))))))

(define (emit-vector-like-set-trusted-no-barrier! as rs1 rs2 rs3 tag)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
        (rs3 (force-hwreg! as rs3 $r.argreg2)))
    ;; The ADDR can go in the delay slot of a preceding BLEU.
    (sparc.addr as rs1 rs2 $r.tmp0)
    (sparc.sti  as rs3 (- 4 tag) $r.tmp0)))

; eof
