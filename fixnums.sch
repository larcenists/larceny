(define-primop 'most-negative-fixnum
  (lambda (as)
    (emit-most-negative-fixnum as $r.result)))

(define-primop 'most-positive-fixnum
  (lambda (as)
    (emit-most-positive-fixnum as $r.result)))

(define-primop 'fx+
  (lambda (as rs2)
    (emit-fx+ as $r.result rs2 $r.result)))

(define-primop 'fx+
  (lambda (as rs2)
    (emit-fx- as $r.result rs2 $r.result)))


(define (emit-most-positive-fixnum as rd)
  (emit-constant->register as (asm:signed #x7FFFFFFC) rd))

(define (emit-most-negative-fixnum as rd)
  (emit-constant->register as (asm:signed #xFFFFFFFC) rd))

; General constraints
;
; RS1 is a general hardware register or RESULT
; RS2 is a general register or ARGREG2
; RD is a general hardware register or RESULT

(define (emit-fx+ as rs1 rs2 rd)
  (emit-fixnum-arithmetic as sparc.add sparc.taddcc rs1 rs2 rd $ex.fx+))

(define (emit-fx- as rs1 rs2 rd)
  (emit-fixnum-arithmetic as sparc.sub sparc.tsubcc rs1 rs2 rd $ex.fx-))

(define (emit-fixnum-arithmetic as op-nocheck op-check rs1 rs2 rd exn)
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


