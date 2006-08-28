; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; 25 September 2000 / wdc
;
; SPARC code generation macros for primitives, part 4:
;   index- and fixnum-specific operations.
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
;     Must add compiler macro for fxabs.


; most-negative-fixnum, most-positive-fixnum.

(define-primop 'most-negative-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x80000000) $r.result)))

(define-primop 'most-positive-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x7FFFFFFC) $r.result)))

; index-specific operations: no checks at all

(define-primop '+:idx:idx
  (lambda (as rs2)
    (emit-index-arithmetic as sparc.addr $r.result rs2 $r.result)))

(define-primop 'internal:+:idx:idx
  (lambda (as rs1 rs2 rd)
    (emit-index-arithmetic as sparc.addr rs1 rs2 rd)))

(define-primop '-:idx:idx
  (lambda (as rs2)
    (emit-index-arithmetic as sparc.subr $r.result rs2 $r.result)))

(define-primop 'internal:-:idx:idx
  (lambda (as rs1 rs2 rd)
    (emit-index-arithmetic as sparc.subr rs1 rs2 rd)))

; index-specific with immediate operand.

(define-primop 'internal:+:idx:idx/imm
  (lambda (as rs imm rd)
    (emit-index-arithmetic/imm as sparc.addi rs imm rd)))

(define-primop 'internal:-:idx:idx/imm
  (lambda (as rs imm rd)
    (emit-index-arithmetic/imm as sparc.subi rs imm rd)))

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


; Trusted fixnum comparisons.

(define-primop '=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bne.a $r.result rs2 $r.result #f)))

(define-primop '<:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bge.a $r.result rs2 $r.result #f)))

(define-primop '<=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bg.a $r.result rs2 $r.result #f)))

(define-primop '>:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.ble.a $r.result rs2 $r.result #f)))

(define-primop '>=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bl.a $r.result rs2 $r.result #f)))

(define-primop 'internal:=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bne.a rs1 rs2 rd #f)))

(define-primop 'internal:<:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bge.a rs1 rs2 rd #f)))

(define-primop 'internal:<=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bg.a rs1 rs2 rd #f)))

(define-primop 'internal:>:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.ble.a rs1 rs2 rd #f)))

(define-primop 'internal:>=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bl.a rs1 rs2 rd #f)))

; With immediates.

(define-primop 'internal:=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bne.a rs imm rd #f)))

(define-primop 'internal:<:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bge.a rs imm rd #f)))

(define-primop 'internal:<=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bg.a rs imm rd #f)))

(define-primop 'internal:>:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.ble.a rs imm rd #f)))

(define-primop 'internal:>=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bl.a rs imm rd #f)))

; Without immediates, for control.

(define-primop 'internal:branchf-=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bne.a rs1 rs2 #f L)))

(define-primop 'internal:branchf-<:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bge.a rs1 rs2 #f L)))

(define-primop 'internal:branchf-<=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bg.a rs1 rs2 #f L)))

(define-primop 'internal:branchf->:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.ble.a rs1 rs2 #f L)))

(define-primop 'internal:branchf->=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bl.a rs1 rs2 #f L)))

; With immediates, for control.

(define-primop 'internal:branchf-=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bne.a rs imm #f L)))

(define-primop 'internal:branchf-<:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bge.a rs imm #f L)))

(define-primop 'internal:branchf-<=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bg.a rs imm #f L)))

(define-primop 'internal:branchf->:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.ble.a rs imm #f L)))

(define-primop 'internal:branchf->=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bl.a rs imm #f L)))

; Range check:  0 <= src1 < src2

(define-primop 'internal:check-range
  (lambda (as src1 src2 L1 livregs)
    (let ((src2 (force-hwreg! as src2 $r.argreg2)))
      (emit-fixnum-compare-check
       as src2 src1 sparc.bleu L1 livregs))))

; Trusted fixnum comparisons followed by a check.

(define-primop 'internal:check-=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bne L1 liveregs)))

(define-primop 'internal:check-<:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bge L1 liveregs)))

(define-primop 'internal:check-<=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bg L1 liveregs)))

(define-primop 'internal:check->:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.ble L1 liveregs)))

(define-primop 'internal:check->=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bl L1 liveregs)))

(define-primop 'internal:check-=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bne L1 liveregs)))

(define-primop 'internal:check-<:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bge L1 liveregs)))

(define-primop 'internal:check-<=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bg L1 liveregs)))

(define-primop 'internal:check->:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.ble L1 liveregs)))

(define-primop 'internal:check->=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bl L1 liveregs)))

; Emitters.

(define (emit-index-arithmetic as op rs1 rs2 rd)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (op as rs1 rs2 rd)))

(define (emit-fixnum-arithmetic as op-check op-nocheck rs1 rs2 rd exn)
  (if (unsafe-code)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(op-nocheck as rs1 rs2 rd))
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2))
	    (L0  (new-label))
	    (L1  (new-label)))
	(sparc.label  as L0)
	(op-check     as rs1 rs2 $r.tmp0)
	(sparc.bvc.a  as L1)
	(sparc.move   as $r.tmp0 rd)
        (if (not (= exn $ex.fx--))
            (begin
              (if (not (= rs1 $r.result)) (sparc.move as rs1 $r.result))
              (if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2)))
            (begin
              (if (not (= rs2 $r.result)) (sparc.move as rs2 $r.result))))
	(sparc.set    as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label  as L1))))

(define (emit-index-arithmetic/imm as op rs1 imm rd)
  (op as rs1 (thefixnum imm) rd))

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

; Below, 'target' is a label or #f.  If #f, RD must be a general hardware
; register or RESULT, and a boolean result is generated in RD.

(define (emit-fixnum-compare as branchf.a rs1 rs2 rd exn target)
  (if (unsafe-code)
      (emit-fixnum-compare-trusted as branchf.a rs1 rs2 rd target)
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
        (sparc.label as L1)
        (emit-evaluate-cc! as branchf.a rd target))))

; Below, 'target' is a label or #f.  If #f, RD must be a general hardware
; register or RESULT, and a boolean result is generated in RD.

(define (emit-fixnum-compare-trusted as branchf.a rs1 rs2 rd target)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (sparc.cmpr  as rs1 rs2)
    (emit-evaluate-cc! as branchf.a rd target)))

; rs must be a hardware register.

(define (emit-fixnum-compare/imm as branchf.a rs imm rd exn target)
  (if (unsafe-code)
      (emit-fixnum-compare/imm-trusted as branchf.a rs imm rd target)
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
  (emit-evaluate-cc! as branchf.a rd target))

; rs must be a hardware register.

(define (emit-fixnum-compare/imm-trusted as branchf.a rs imm rd target)
  (sparc.cmpi  as rs (thefixnum imm))
  (emit-evaluate-cc! as branchf.a rd target))

; Range checks.

(define (emit-fixnum-compare-check
         as src1 src2 branch-bad L1 liveregs)
  (internal-primop-invariant1 'emit-fixnum-compare-check src1)
  (let ((src2 (force-hwreg! as src2 $r.argreg2)))
    (sparc.cmpr    as src1 src2)
    (emit-checkcc! as branch-bad L1 liveregs)))

(define (emit-fixnum-compare/imm-check
         as src1 imm branch-bad L1 liveregs)
  (internal-primop-invariant1 'emit-fixnum-compare/imm-check src1)
  (sparc.cmpi    as src1 imm)
  (emit-checkcc! as branch-bad L1 liveregs))

; eof
