; Copyright 1998 Lars T Hansen and William D Clinger.
; 
; $Id: sparcprim-part2.sch,v 1.1.1.1 1998/11/19 21:52:00 lth Exp $
;
; Asm/Sparc/sparcprim-part2.sch -- SPARC primitives, part 2.
; Primitives introduced by peephole optimization.

(define-primop 'internal:car
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:car src1 dest)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 #f $ex.car))
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:cdr
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:cdr src1 dest)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 #f $ex.cdr))
    (sparc.ldi as src1 (- 4 $tag.pair-tag) dest)))

(define-primop 'internal:cell-ref
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:cell-ref src1 dest)
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:set-car!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:set-car! rs1 dest-ignored)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as $tag.pair-tag rs1 rs2 $ex.car))
    (emit-setcar/setcdr! as rs1 rs2 0)))

(define-primop 'internal:set-cdr!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:set-cdr! rs1 dest-ignored)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as $tag.pair-tag rs1 rs2 $ex.cdr))
    (emit-setcar/setcdr! as rs1 rs2 4)))

(define-primop 'internal:cell-set!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:cell-set! rs1 dest-ignored)
    (emit-setcar/setcdr! as rs1 rs2 0)))

; CONS
;
; One instruction reduced here translates into about 2.5KB reduction in the
; size of the basic heap image.  

; This is better code than the version below -- 8 instructions in the inline
; case rather than 13 -- but I need to implement $m.gc first!  
; (Code untested, looks alright.)

'(define-primop 'internal:cons
  (lambda (as rs1 rs2 rd)
    (if (inline-allocation)
	(let ((ENOUGH-MEMORY (new-label))
	      (START (new-label)))
	  (sparc.label   as START)
	  (sparc.addi    as $r.e-top 8 $r.e-top)
	  (sparc.cmpr    as $r.tmp0 $r.e-limit)
	  (sparc.ble.a   as ENOUGH-MEMORY)
	  (sparc.sti     as rs1 -8 $r.e-top)
	  (millicode-call/ret as $m.gc START)
	  (sparc.label   as ENOUGH-MEMORY)
	  (sparc.sti     as (force-hwreg! as rs2 $r.tmp0) rs2 -4 $r.e-top)
	  (sparc.subi    as $r.e-top (- 8 $tag.pair-tag) rd))
	(begin
	  (if (= rs1 $r.result)
	      (sparc.move as $r.result $r.argreg2))
	  (millicode-call/numarg-in-result as $m.alloc 8)
	  (if (= rs1 $r.result)
	      (sparc.sti as $r.argreg2 0 $r.result)
	      (sparc.sti as rs1 0 $r.result))
	  (sparc.sti as (force-hwreg! as rs2 $r.tmp1) 4 $r.result)
	  (sparc.addi as $r.result $tag.pair-tag rd)))))

(define-primop 'internal:cons
  (lambda (as src1 src2 dest)

    (define (emit-common-case cont)
      (let ((src1 (if (eqv? src1 $r.result)
		      (begin (sparc.move as $r.result $r.argreg2)
			     $r.argreg2)
		      src1)))
	(millicode-call/numarg-in-result as $m.alloc 8)
	(sparc.sti as src1 0 $r.result)
	(sparc.sti as (force-hwreg! as src2 $r.tmp1) 4 $r.result)
	(if cont (sparc.b as cont))
	(sparc.addi as $r.result $tag.pair-tag dest)))

    (internal-primop-invariant2 'internal:cons src1 dest)
    (if (inline-allocation)
	(let ((L1 (new-label))
	      (L2 (new-label)))
	  ; Fast path.
	  (sparc.addi  as $r.e-top 8 $r.e-top)
	  (sparc.cmpr  as $r.e-top $r.e-limit)
	  (sparc.ble.a as L1)
	  (sparc.sti   as src1 -8 $r.e-top)

	  ; Overflow: just do an 'alloc' to trigger the correct gc.
	  (sparc.subi as $r.e-top 8 $r.e-top)
	  (emit-common-case L2)

	  ; Fast path again.
	  (sparc.label as L1)
	  (let ((src2 (force-hwreg! as src2 $r.tmp1)))
	    (sparc.sti   as src2 -4 $r.e-top)
	    (sparc.subi  as $r.e-top 7 dest)     ; FIXME: evil tag spec
	    (sparc.label as L2)))
	(emit-common-case #f))))

(define-primop 'internal:vector-length
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:vector-length rs rd)
    (emit-get-length! as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      $ex.vlen
		      rs
		      rd)))

(define-primop 'internal:vector-ref
  (lambda (as rs1 rs2 rd)
    (internal-primop-invariant2 'internal:vector-ref rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1 
		      rs2
		      $ex.vref))))
      (emit-vector-like-ref! as rs1 rs2 rd fault $tag.vector-tag #t))))

(define-primop 'internal:vector-ref/imm
  (lambda (as rs1 imm rd)
    (internal-primop-invariant2 'internal:vector-ref/imm rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/imm!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1 
		      imm
		      $ex.vref))))
      (emit-vector-like-ref/imm! as rs1 imm rd fault $tag.vector-tag #t))))

(define-primop 'internal:vector-set!
  (lambda (as rs1 rs2 rs3)
    (internal-primop-invariant1 'internal:vector-set! rs1)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1
		      rs2
		      $ex.vset))))
      (emit-vector-like-set! as rs1 rs2 rs3 fault $tag.vector-tag #t))))

(define-primop 'internal:string-length
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:string-length rs rd)
    (emit-get-length! as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      $ex.slen
		      rs
		      rd)))

(define-primop 'internal:string-ref
  (lambda (as rs1 rs2 rd)
    (internal-primop-invariant2 'internal:string-ref rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      rs1 
		      rs2
		      $ex.sref))))
      (emit-bytevector-like-ref! as rs1 rs2 rd fault #t #t))))

(define-primop 'internal:string-ref/imm
  (lambda (as rs1 imm rd)
    (internal-primop-invariant2 'internal:string-ref/imm rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/imm!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      rs1 
		      imm
		      $ex.sref))))
      (emit-bytevector-like-ref/imm! as rs1 imm rd fault #t #t))))

(define-primop 'internal:string-set!
  (lambda (as rs1 rs2 rs3)
    (internal-primop-invariant1 'internal:string-set! rs1)
      (emit-string-set! as rs1 rs2 rs3)))

(define-primop 'internal:+
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:+ src1 dest)
    (emit-arith-primop! as sparc.taddrcc sparc.subr $m.add src1 src2 dest #t)))

(define-primop 'internal:+/imm
  (lambda (as src1 imm dest)
    (internal-primop-invariant2 'internal:+/imm src1 dest)
    (emit-arith-primop! as sparc.taddicc sparc.subi $m.add src1 imm dest #f)))

(define-primop 'internal:-
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:- src1 dest)
    (emit-arith-primop! as sparc.tsubrcc sparc.addr $m.subtract 
			src1 src2 dest #t)))

(define-primop 'internal:-/imm
  (lambda (as src1 imm dest)
    (internal-primop-invariant2 'internal:-/imm src1 dest)
    (emit-arith-primop! as sparc.tsubicc sparc.addi $m.subtract
			src1 imm dest #f)))

(define-primop 'internal:--
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:-- rs rd)
    (emit-negate as rs rd)))

(define-primop 'internal:branchf-null?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:branchf-null? reg)
    (sparc.cmpi  as reg $imm.null)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-pair?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:branchf-pair? reg)
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $tag.pair-tag)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-zero?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:brancf-zero? reg)
    (emit-bcmp-primop! as sparc.bne.a reg $r.g0 label $m.zerop #t)))

(define-primop 'internal:branchf-eof-object?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-eof-object? rs)
    (sparc.cmpi  as rs $imm.eof)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-fixnum?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-fixnum? rs)
    (sparc.btsti as rs 3)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-char?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-char? rs)
    (sparc.andi  as rs 255 $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $imm.character)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-= src1)
    (emit-bcmp-primop! as sparc.bne.a src1 src2 label $m.numeq #t)))

(define-primop 'internal:branchf-<
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-< src1)
    (emit-bcmp-primop! as sparc.bge.a src1 src2 label $m.numlt #t)))

(define-primop 'internal:branchf-<=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-<= src1)
    (emit-bcmp-primop! as sparc.bg.a src1 src2 label $m.numle #t)))

(define-primop 'internal:branchf->
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-> src1)
    (emit-bcmp-primop! as sparc.ble.a src1 src2 label $m.numgt #t)))

(define-primop 'internal:branchf->=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf->= src1)
    (emit-bcmp-primop! as sparc.bl.a src1 src2 label $m.numge #t)))

(define-primop 'internal:branchf-=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-=/imm src1)
    (emit-bcmp-primop! as sparc.bne.a src1 imm label $m.numeq #f)))

(define-primop 'internal:branchf-</imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-</imm src1)
    (emit-bcmp-primop! as sparc.bge.a src1 imm label $m.numlt #f)))

(define-primop 'internal:branchf-<=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-<=/imm src1)
    (emit-bcmp-primop! as sparc.bg.a src1 imm label $m.numle #f)))

(define-primop 'internal:branchf->/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf->/imm src1)
    (emit-bcmp-primop! as sparc.ble.a src1 imm label $m.numgt #f)))

(define-primop 'internal:branchf->=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf->=/imm src1)
    (emit-bcmp-primop! as sparc.bl.a src1 imm label $m.numge #f)))

(define-primop 'internal:branchf-char=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char=? src1)
    (emit-char-bcmp-primop! as sparc.bne.a src1 src2 label $ex.char=?)))

(define-primop 'internal:branchf-char<=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char<=? src1)
    (emit-char-bcmp-primop! as sparc.bg.a src1 src2 label $ex.char<=?)))

(define-primop 'internal:branchf-char<?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char<? src1)
    (emit-char-bcmp-primop! as sparc.bge.a src1 src2 label $ex.char<?)))

(define-primop 'internal:branchf-char>=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char>=? src1)
    (emit-char-bcmp-primop! as sparc.bl.a src1 src2 label $ex.char>=?)))

(define-primop 'internal:branchf-char>?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char>=? src1)
    (emit-char-bcmp-primop! as sparc.ble.a src1 src2 label $ex.char>?)))

(define-primop 'internal:branchf-char=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char=?/imm src)
    (emit-char-bcmp-primop! as sparc.bne.a src imm label $ex.char=?)))

(define-primop 'internal:branchf-char>=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char>=?/imm src)
    (emit-char-bcmp-primop! as sparc.bl.a src imm label $ex.char>=?)))

(define-primop 'internal:branchf-char>?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char>?/imm src)
    (emit-char-bcmp-primop! as sparc.ble.a src imm label $ex.char>?)))

(define-primop 'internal:branchf-char<=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char<=?/imm src)
    (emit-char-bcmp-primop! as sparc.bg.a src imm label $ex.char<=?)))

(define-primop 'internal:branchf-char<?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char<?/imm src)
    (emit-char-bcmp-primop! as sparc.bge.a src imm label $ex.char<?)))

(define-primop 'internal:eq?
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:eq? src1 dest)
    (let ((tmp (force-hwreg! as src2 $r.tmp0)))
      (sparc.cmpr as src1 tmp)
      (emit-set-boolean-reg! as dest))))

(define-primop 'internal:eq?/imm
  (lambda (as rs imm rd)
    (internal-primop-invariant2 'internal:eq?/imm rs rd)
    (cond ((fixnum? imm) (sparc.cmpi as rs (thefixnum imm)))
	  ((eq? imm #t)  (sparc.cmpi as rs $imm.true))
	  ((eq? imm #f)  (sparc.cmpi as rs $imm.false))
	  ((null? imm)   (sparc.cmpi as rs $imm.null))
	  (else ???))
    (emit-set-boolean-reg! as rd)))

(define-primop 'internal:branchf-eq?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-eq? src1)
    (let ((src2 (force-hwreg! as src2 $r.tmp0)))
      (sparc.cmpr  as src1 src2)
      (sparc.bne.a as label)
      (sparc.slot  as))))

(define-primop 'internal:branchf-eq?/imm
  (lambda (as rs imm label)
    (internal-primop-invariant1 'internal:branchf-eq?/imm rs)
    (cond ((fixnum? imm) (sparc.cmpi as rs (thefixnum imm)))
	  ((eq? imm #t)  (sparc.cmpi as rs $imm.true))
	  ((eq? imm #f)  (sparc.cmpi as rs $imm.false))
	  ((null? imm)   (sparc.cmpi as rs $imm.null))
	  (else ???))
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define (internal-primop-invariant2 name a b)
    (if (not (and (hardware-mapped? a) (hardware-mapped? b)))
	(asm-error "SPARC assembler internal invariant violated by " name
		   " on operands " a " and " b)))

(define (internal-primop-invariant1 name a)
    (if (not (hardware-mapped? a))
	(asm-error "SPARC assembler internal invariant violated by " name
		   " on operand " a)))

; eof
