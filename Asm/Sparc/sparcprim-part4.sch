; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; SPARC code generation macros for primitives, part 3:
;   fixnum-specific operations.
;
; Constraints for all the primops.
;
; RS1 is a general hardware register or RESULT.
; RS2 is a general register or ARGREG2.
; IMM is an exact integer in the range -1024 .. 1023.
; RD is a general hardware register or RESULT.

; FIXME
;   Missing fxquotient, fxremainder
;   When new pass1 in place:
;     Must add code to pass1 to allow n-ary calls to be rewritten as binary
;     Must add compiler macro for fxabs.


; most-negative-fixnum, most-positive-fixnum.

(define-primop 'most-negative-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x80000000) $r.result)))

(define-primop 'most-positive-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x7FFFFFFC) $r.result)))


; fx+, fx- w/o immediates

(define-primop 'fx+
  (lambda (as rs2)
    (emit-fixnum-arithmetic as sparc.taddrcc sparc.addr $r.result rs2 $r.result
			    $ex.fx+)))

(define-primop 'internal:fx+
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-arithmetic as sparc.taddrcc sparc.addr rs1 rs2 rd $ex.fx+)))

(define-primop 'fx-
  (lambda (as rs2)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr $r.result rs2 $r.result
			    $ex.fx-)))

(define-primop 'internal:fx-
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr rs1 rs2 rd $ex.fx-)))

(define-primop 'fx--
  (lambda (as)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr
			    $r.g0 $r.result $r.result $ex.fx--)))

(define-primop 'internal:fx--
  (lambda (as rs rd)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr $r.g0 rs rd $ex.fx--)))

(define (emit-fixnum-arithmetic as op-check op-nocheck rs1 rs2 rd exn)
  (if (unsafe-code)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(op-nocheck as rs1 rs2 rd))
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2))
	    (L0  (new-label))
	    (L1  (new-label)))
	(sparc.label  as L0)
	(op-check as rs1 rs2 $r.tmp0)
	(sparc.bvc.a  as L1)
	(sparc.move   as $r.tmp0 rd)
	(if (not (= rs1 $r.result)) (sparc.move as rs1 $r.result))
	(if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
	(sparc.set    as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label  as L1))))

; fx* w/o immediate

(define-primop 'fx*
  (lambda (as rs2)
    (emit-multiply-code as rs2 #t)))

; fx+, fx- w/immediates

(define-primop 'internal:fx+/imm
  (lambda (as rs imm rd)
    (emit-fixnum-arithmetic/imm as sparc.taddicc sparc.addi
				rs imm rd $ex.fx+)))

(define-primop 'internal:fx-/imm
  (lambda (as rs imm rd)
    (emit-fixnum-arithmetic/imm as sparc.tsubicc sparc.subi
				rs imm rd $ex.fx-)))

(define (emit-fixnum-arithmetic/imm as op-check op-nocheck rs imm rd exn)
  (if (unsafe-code)
      (op-nocheck as rs (thefixnum imm) rd)
      (let ((L0  (new-label))
	    (L1  (new-label)))
	(sparc.label  as L0)
	(op-check     as rs (thefixnum imm) $r.tmp0)
	(sparc.bvc.a  as L1)
	(sparc.move   as $r.tmp0 rd)
	(if (not (= rs $r.result)) (sparc.move as rs $r.result))
	(sparc.set    as (thefixnum imm) $r.argreg2)
	(sparc.set    as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label  as L1))))


; fx=, fx<, fx<=, fx>, fx>=, fxpositive?, fxnegative?, fxzero? w/o immediates

(define-primop 'fx=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bne.a $r.result rs2 $r.result $ex.fx= #f)))

(define-primop 'fx<
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bge.a $r.result rs2 $r.result $ex.fx< #f)))

(define-primop 'fx<=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bg.a $r.result rs2 $r.result $ex.fx<= #f)))

(define-primop 'fx>
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.ble.a $r.result rs2 $r.result $ex.fx> #f)))

(define-primop 'fx>=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bl.a $r.result rs2 $r.result $ex.fx>= #f)))

(define-primop 'internal:fx=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bne.a rs1 rs2 rd $ex.fx= #f)))

(define-primop 'internal:fx<
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bge.a rs1 rs2 rd $ex.fx< #f)))

(define-primop 'internal:fx<=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bg.a rs1 rs2 rd $ex.fx<= #f)))

(define-primop 'internal:fx>
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.ble.a rs1 rs2 rd $ex.fx> #f)))

(define-primop 'internal:fx>=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bl.a rs1 rs2 rd $ex.fx>= #f)))


; Use '/imm' code for these because the generated code is better.

(define-primop 'fxpositive?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.ble.a $r.result 0 $r.result
			     $ex.fxpositive? #f)))

(define-primop 'fxnegative?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.bge.a $r.result 0 $r.result
				$ex.fxnegative? #f)))

(define-primop 'fxzero?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.bne.a $r.result 0 $r.result
				$ex.fxzero? #f)))

(define-primop 'internal:fxpositive?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.ble.a rs 0 rd $ex.fxpositive? #f)))

(define-primop 'internal:fxnegative?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.bge.a rs 0 rd $ex.fxnegative? #f)))

(define-primop 'internal:fxzero?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.bne.a rs 0 rd $ex.fxzero? #f)))


; fx=, fx<, fx<=, fx>, fx>=  w/immediates

(define-primop 'internal:fx=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bne.a rs imm rd $ex.fx= #f)))

(define-primop 'internal:fx</imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bge.a rs imm rd $ex.fx< #f)))

(define-primop 'internal:fx<=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bg.a rs imm rd $ex.fx<= #f)))

(define-primop 'internal:fx>/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.ble.a rs imm rd $ex.fx> #f)))

(define-primop 'internal:fx>=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bl.a rs imm rd $ex.fx>= #f)))


; fx=, fx<, fx<=, fx>, fx>=, fxpositive?, fxnegative?, fxzero? w/o immediates
; for control.

(define-primop 'internal:branchf-fx=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bne.a rs1 rs2 #f $ex.fx= L)))

(define-primop 'internal:branchf-fx<
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bge.a rs1 rs2 #f $ex.fx< L)))

(define-primop 'internal:branchf-fx<=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bg.a rs1 rs2 #f $ex.fx<= L)))

(define-primop 'internal:branchf-fx>
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.ble.a rs1 rs2 #f $ex.fx> L)))

(define-primop 'internal:branchf-fx>=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bl.a rs1 rs2 #f $ex.fx>= L)))

(define-primop 'internal:branchf-fxpositive?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.ble.a rs1 0 #f $ex.fxpositive? L)))

(define-primop 'internal:branchf-fxnegative?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.bge.a rs1 0 #f $ex.fxnegative? L)))

(define-primop 'internal:branchf-fxzero?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.bne.a rs1 0 #f $ex.fxzero? L)))


; fx=, fx<, fx<=, fx>, fx>=  w/immediates for control.

(define-primop 'internal:branchf-fx=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bne.a rs imm #f $ex.fx= L)))

(define-primop 'internal:branchf-fx</imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bge.a rs imm #f $ex.fx< L)))

(define-primop 'internal:branchf-fx<=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bg.a rs imm #f $ex.fx<= L)))

(define-primop 'internal:branchf-fx>/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.ble.a rs imm #f $ex.fx> L)))

(define-primop 'internal:branchf-fx>=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bl.a rs imm #f $ex.fx>= L)))


; Below, 'target' is a label or #f.  If #f, RD must be a general hardware
; register or RESULT, and a boolean result is generated in RD.

(define (emit-fixnum-compare as branchf.a rs1 rs2 rd exn target)
  (if (unsafe-code)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(sparc.cmpr  as rs1 rs2))
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2))
	    (L0 (new-label))
	    (L1 (new-label)))
	(sparc.label as L0)
	(sparc.orr   as rs1 rs2 $r.tmp0)
	(sparc.btsti as $r.tmp0 3)
	(sparc.be.a  as L1)
	(sparc.cmpr  as rs1 rs2)
	(if (not (= rs1 $r.result)) (sparc.move as rs1 $r.result))
	(if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
	(sparc.set   as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label as L1)))
  (if (not target)
      (let ((target (new-label)))
	(branchf.a   as target)
	(sparc.set   as $imm.false rd)
	(sparc.set   as $imm.true rd)
	(sparc.label as target))
      (begin
	(branchf.a   as target)
	(sparc.slot  as))))

(define (emit-fixnum-compare/imm as branchf.a rs imm rd exn target)
  (if (unsafe-code)
      (begin
	(sparc.cmpi  as rs (thefixnum imm)))
      (let ((L0 (new-label))
	    (L1 (new-label)))
	(sparc.label as L0)
	(sparc.btsti as rs 3)
	(sparc.be.a  as L1)
	(sparc.cmpi  as rs (thefixnum imm))
	(if (not (= rs $r.result)) (sparc.move as rs $r.result))
	(sparc.set   as (thefixnum imm) $r.argreg2)
	(sparc.set   as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label as L1)))
  (if (not target)
      (let ((target (new-label)))
	(branchf.a   as target)
	(sparc.set   as $imm.false rd)
	(sparc.set   as $imm.true rd)
	(sparc.label as target))
      (begin
	(branchf.a   as target)
	(sparc.slot  as))))

; eof
