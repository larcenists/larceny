; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Asm/Sparc/peepopt.sch -- MAL peephole optimizer, for the SPARC assembler.
;
; The procedure `peep' is called on the as structure before every
; instruction is assembled.  It may replace the prefix of the instruction
; stream by some other instruction sequence.
;
; Invariant: if the peephole optimizer doesn't change anything, then 
;
;  (let ((x (as-source as)))
;    (peep as)
;    (eq? x (as-source as)))     => #t
;
; Note this still isn't right -- it should be integrated with pass5p2 --
; but it's a step in the right direction.

(define *peephole-table* (make-vector *number-of-mnemonics* #f))

(define (define-peephole n p)
  (vector-set! *peephole-table* n p)
  (unspecified))

(define (peep as)
  (let ((t0 (as-source as)))
    (if (not (null? t0))
	(let ((i1 (car t0)))
	  (let ((p (vector-ref *peephole-table* (car i1))))
	    (if p
		(let* ((t1 (if (null? t0) t0 (cdr t0)))
		       (i2 (if (null? t1) '(-1 0 0 0) (car t1)))
		       (t2 (if (null? t1) t1 (cdr t1)))
		       (i3 (if (null? t2) '(-1 0 0 0) (car t2)))
		       (t3 (if (null? t2) t2 (cdr t2))))
		  (p as i1 i2 i3 t1 t2 t3))))))))

(define-peephole $reg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $return)
	   (reg-return as i1 i2 t2))
	  ((= (car i2) $setglbl)
	   (reg-setglbl as i1 i2 t2))
	  ((= (car i2) $op1)
	   (cond ((= (car i3) $setreg)
		  (reg-op1-setreg as i1 i2 i3 t2 t3))
		 ((= (car i3) $branchf)
		  (reg-op1-branchf as i1 i2 i3 t3))
		 (else
		  (reg-op1 as i1 i2 t2))))
	  ((= (car i2) $op2)
	   (cond ((= (car i3) $setreg)
		  (reg-op2-setreg as i1 i2 i3 t2 t3))
		 ((= (car i3) $branchf)
		  (reg-op2-branchf as i1 i2 i3 t3))
		 (else
		  (reg-op2 as i1 i2 t2))))
	  ((= (car i2) $op2imm)
	   (cond ((= (car i3) $setreg)
		  (reg-op2imm-setreg as i1 i2 i3 t2 t3))
		 ((= (car i3) $branchf)
		  (reg-op2imm-branchf as i1 i2 i3 t3))
		 (else
		  (reg-op2imm as i1 i2 t2))))
	  ((= (car i2) $op3)
	   (reg-op3 as i1 i2 t2))
	  ((= (car i2) $setreg)
	   (reg-setreg as i1 i2 t2))
	  ((= (car i2) $branchf)
	   (reg-branchf as i1 i2 t2)))))

(define-peephole $op1
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
	   (op1-branchf as i1 i2 t2))
	  ((= (car i2) $setreg)
	   (op1-setreg as i1 i2 t2)))))

(define-peephole $op2
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
	   (op2-branchf as i1 i2 t2))
	  ((= (car i2) $setreg)
	   (op2-setreg as i1 i2 t2)))))

(define-peephole $op2imm
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
	   (op2imm-branchf as i1 i2 t2))
	  ((= (car i2) $setreg)
	   (op2imm-setreg as i1 i2 t2)))))

(define-peephole $const
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
	   (const-setreg as i1 i2 t2))
	  ((= (car i2) $op2)
	   (const-op2 as i1 i2 t2))
	  ((= (car i2) $return)
	   (const-return as i1 i2 t2)))))

(define-peephole $setrtn
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branch)
	   (cond ((= (car i3) $.align)
		  (if (not (null? t3))
		      (let ((i4 (car t3))
			    (t4 (cdr t3)))
			(cond ((= (car i4) $.label)
			       (setrtn-branch as i1 i2 i3 i4 t4))))))))
	  ((= (car i2) $invoke)
	   (cond ((= (car i3) $.align)
		  (if (not (null? t3))
		      (let ((i4 (car t3))
			    (t4 (cdr t3)))
			(cond ((= (car i4) $.label)
			       (setrtn-invoke as i1 i2 i3 i4 t4)))))))))))

(define-peephole $branch
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $.align)
	   (cond ((= (car i3) $.label)
		  (branch-and-label as i1 i2 i3 t3)))))))

(define-peephole $global
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
	   (global-setreg as i1 i2 t2))
	  ((= (car i2) $invoke)
	   (global-invoke as i1 i2 t2))
	  ((= (car i2) $setrtn)
	   (cond ((= (car i3) $invoke)
		  (global-setrtn-invoke as i1 i2 i3 t3)))))))

; Worker procedures.

(define (reg-return as i:reg i:return tail)
  (let ((rs (operand1 i:reg)))
    (if (hwreg? rs)
	(as-source! as (cons (list $reg/return rs) tail)))))

(define (reg-op1-setreg as i:reg i:op1 i:setreg tail-1 tail)
  (let ((rs (operand1 i:reg))
	(rd (operand1 i:setreg))
	(op (operand1 i:op1)))
    (if (hwreg? rs)
	(if (hwreg? rd)
	    (peep-reg/op1/setreg as op rs rd tail)
	    (peep-reg/op1/setreg as op rs 'RESULT tail-1)))))

(define (reg-op1 as i:reg i:op1 tail)
  (let ((rs (operand1 i:reg))
	(op (operand1 i:op1)))
    (if (hwreg? rs)
	(peep-reg/op1/setreg as op rs 'RESULT tail))))

(define (op1-setreg as i:op1 i:setreg tail)
  (let ((op (operand1 i:op1))
	(rd (operand1 i:setreg)))
    (if (hwreg? rd)
	(peep-reg/op1/setreg as op 'RESULT rd tail))))

(define (peep-reg/op1/setreg as op rs rd tail)
  (let ((op (case op
	      ((car)           'internal:car)
	      ((cdr)           'internal:cdr)
	      ((cell-ref)      'internal:cell-ref)
	      ((vector-length) 'internal:vector-length)
	      ((string-length) 'internal:string-length)
	      ((--)            'internal:--)
	      ((fx--)          'internal:fx--)
	      ((fxpositive?)   'internal:fxpositive?)
	      ((fxnegative?)   'internal:fxnegative?)
	      ((fxzero?)       'internal:fxzero?)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op1/setreg op rs rd) tail)))))

(define (reg-op2-setreg as i:reg i:op2 i:setreg tail-1 tail)
  (let ((rs1 (operand1 i:reg))
	(rs2 (operand2 i:op2))
	(op  (operand1 i:op2))
	(rd  (operand1 i:setreg)))
    (if (hwreg? rs1)
	(if (hwreg? rd)
	    (peep-reg/op2/setreg as op rs1 rs2 rd tail)
	    (peep-reg/op2/setreg as op rs1 rs2 'RESULT tail-1)))))

(define (reg-op2 as i:reg i:op2 tail)
  (let ((rs1 (operand1 i:reg))
	(rs2 (operand2 i:op2))
	(op  (operand1 i:op2)))
    (if (hwreg? rs1)
	(peep-reg/op2/setreg as op rs1 rs2 'RESULT tail))))

(define (op2-setreg as i:op2 i:setreg tail)
  (let ((op  (operand1 i:op2))
	(rs2 (operand2 i:op2))
	(rd  (operand1 i:setreg)))
    (if (hwreg? rd)
	(peep-reg/op2/setreg as op 'RESULT rs2 rd tail))))

(define (peep-reg/op2/setreg as op rs1 rs2 rd tail)
  (let ((op (case op
	      ((+)          'internal:+)
	      ((-)          'internal:-)
	      ((fx+)        'internal:fx+)
	      ((fx-)        'internal:fx-)
	      ((fx=)        'internal:fx=)
	      ((fx>)        'internal:fx>)
	      ((fx>=)       'internal:fx>=)
	      ((fx<)        'internal:fx<)
	      ((fx<=)       'internal:fx<=)
	      ((eq?)        'internal:eq?)
	      ((cons)       'internal:cons)
	      ((vector-ref) 'internal:vector-ref)
	      ((string-ref) 'internal:string-ref)
	      ((set-car!)   'internal:set-car!)
	      ((set-cdr!)   'internal:set-cdr!)
	      ((cell-set!)  'internal:cell-set!)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op2/setreg op rs1 rs2 rd) tail)))))

(define (reg-op2imm-setreg as i:reg i:op2imm i:setreg tail-1 tail)
  (let ((rs  (operand1 i:reg))
	(imm (operand2 i:op2imm))
	(op  (operand1 i:op2imm))
	(rd  (operand1 i:setreg)))
    (if (hwreg? rs)
	(if (hwreg? rd)
	    (peep-reg/op2imm/setreg as op rs imm rd tail)
	    (peep-reg/op2imm/setreg as op rs imm 'RESULT tail-1)))))

(define (reg-op2imm as i:reg i:op2imm tail)
  (let ((rs  (operand1 i:reg))
	(imm (operand2 i:op2imm))
	(op  (operand1 i:op2imm)))
    (if (hwreg? rs)
	(peep-reg/op2imm/setreg as op rs imm 'RESULT tail))))

(define (op2imm-setreg as i:op2imm i:setreg tail)
  (let ((op  (operand1 i:op2imm))
	(imm (operand2 i:op2imm))
	(rd  (operand1 i:setreg)))
    (if (hwreg? rd)
	(peep-reg/op2imm/setreg as op 'RESULT imm rd tail))))

(define (peep-reg/op2imm/setreg as op rs imm rd tail)
  (let ((op (case op
	      ((+)          'internal:+/imm)
	      ((-)          'internal:-/imm)
	      ((fx+)        'internal:fx+/imm)
	      ((fx-)        'internal:fx-/imm)
	      ((fx=)        'internal:fx=/imm)
	      ((fx<)        'internal:fx</imm)
	      ((fx<=)       'internal:fx<=/imm)
	      ((fx>)        'internal:fx>/imm)
	      ((fx>=)       'internal:fx>=/imm)
	      ((eq?)        'internal:eq?/imm)
	      ((vector-ref) 'internal:vector-ref/imm)
	      ((string-ref) 'internal:string-ref/imm)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op2imm/setreg op rs imm rd) tail)))))

(define (reg-op1-branchf as i:reg i:op1 i:branchf tail)
  (let ((rs (operand1 i:reg))
	(op (operand1 i:op1))
	(L  (operand1 i:branchf)))
    (if (hwreg? rs)
	(peep-reg/op1/branchf as op rs L tail))))

(define (op1-branchf as i:op1 i:branchf tail)
  (let ((op (operand1 i:op1))
	(L  (operand1 i:branchf)))
    (peep-reg/op1/branchf as op 'RESULT L tail)))

(define (peep-reg/op1/branchf as op rs L tail)
  (let ((op (case op
	      ((null?)       'internal:branchf-null?)
	      ((pair?)       'internal:branchf-pair?)
	      ((zero?)       'internal:branchf-zero?)
	      ((eof-object?) 'internal:branchf-eof-object?)
	      ((fixnum?)     'internal:branchf-fixnum?)
	      ((char?)       'internal:branchf-char?)
	      ((fxzero?)     'internal:branchf-fxzero?)
	      ((fxnegative?) 'internal:branchf-fxnegative?)
	      ((fxpositive?) 'internal:branchf-fxpositive?)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op1/branchf op rs L) tail)))))

(define (reg-op2-branchf as i:reg i:op2 i:branchf tail)
  (let ((rs1 (operand1 i:reg))
	(rs2 (operand2 i:op2))
	(op  (operand1 i:op2))
	(L   (operand1 i:branchf)))
    (if (hwreg? rs1)
	(peep-reg/op2/branchf as op rs1 rs2 L tail))))

(define (op2-branchf as i:op2 i:branchf tail)
  (let ((op  (operand1 i:op2))
	(rs2 (operand2 i:op2))
	(L   (operand1 i:branchf)))
    (peep-reg/op2/branchf as op 'RESULT rs2 L tail)))

(define (peep-reg/op2/branchf as op rs1 rs2 L tail)
  (let ((op (case op
	      ((<)       'internal:branchf-<)
	      ((>)       'internal:branchf->)
	      ((>=)      'internal:branchf->=)
	      ((<=)      'internal:branchf-<=)
	      ((=)       'internal:branchf-=)
	      ((eq?)     'internal:branchf-eq?)
	      ((char=?)  'internal:branchf-char=?)
	      ((char>=?) 'internal:branchf-char>=?)
	      ((char>?)  'internal:branchf-char>?)
	      ((char<=?) 'internal:branchf-char<=?)
	      ((char<?)  'internal:branchf-char<?)
	      ((fx=)     'internal:branchf-fx=)
	      ((fx>)     'internal:branchf-fx>)
	      ((fx>=)    'internal:branchf-fx>=)
	      ((fx<)     'internal:branchf-fx<)
	      ((fx<=)    'internal:branchf-fx<=)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op2/branchf op rs1 rs2 L) tail)))))

(define (reg-op2imm-branchf as i:reg i:op2imm i:branchf tail)
  (let ((rs  (operand1 i:reg))
	(imm (operand2 i:op2imm))
	(op  (operand1 i:op2imm))
	(L   (operand1 i:branchf)))
    (if (hwreg? rs)
	(peep-reg/op2imm/branchf as op rs imm L tail))))

(define (op2imm-branchf as i:op2imm i:branchf tail)
  (let ((op  (operand1 i:op2imm))
	(imm (operand2 i:op2imm))
	(L   (operand1 i:branchf)))
    (peep-reg/op2imm/branchf as op 'RESULT imm L tail)))

(define (peep-reg/op2imm/branchf as op rs imm L tail)
  (let ((op (case op
	      ((<)       'internal:branchf-</imm)
	      ((>)       'internal:branchf->/imm)
	      ((>=)      'internal:branchf->=/imm)
	      ((<=)      'internal:branchf-<=/imm)
	      ((=)       'internal:branchf-=/imm)
	      ((eq?)     'internal:branchf-eq?/imm)
	      ((char=?)  'internal:branchf-char=?/imm)
	      ((char>=?) 'internal:branchf-char>=?/imm)
	      ((char>?)  'internal:branchf-char>?/imm)
	      ((char<=?) 'internal:branchf-char<=?/imm)
	      ((char<?)  'internal:branchf-char<?/imm)
	      ((fx=)     'internal:branchf-fx=/imm)
	      ((fx>)     'internal:branchf-fx>/imm)
	      ((fx>=)    'internal:branchf-fx>=/imm)
	      ((fx<)     'internal:branchf-fx</imm)
	      ((fx<=)    'internal:branchf-fx<=/imm)
	      (else #f))))
    (if op
	(as-source! as (cons (list $reg/op2imm/branchf op rs imm L) tail)))))

(define (reg-op3 as i:reg i:op3 tail)
  (let ((rs1 (operand1 i:reg))
	(rs2 (operand2 i:op3))
	(rs3 (operand3 i:op3))
	(op  (operand1 i:op3)))
    (if (hwreg? rs1)
	(let ((op (case op
		    ((vector-set!) 'internal:vector-set!)
		    ((string-set!) 'internal:string-set!)
		    (else #f))))
	  (if op
	      (as-source! as (cons (list $reg/op3 op rs1 rs2 rs3) tail)))))))

; Reg-setreg is not restricted to hardware registers, as $movereg is 
; a standard instruction.

(define (reg-setreg as i:reg i:setreg tail)
  (let ((rs (operand1 i:reg))
	(rd (operand1 i:setreg)))
    (if (= rs rd)
	(as-source! as tail)
	(as-source! as (cons (list $movereg rs rd) tail)))))

(define (reg-branchf as i:reg i:branchf tail)
  (let ((rs (operand1 i:reg))
	(L  (operand1 i:branchf)))
    (if (hwreg? rs)
	(as-source! as (cons (list $reg/branchf rs L) tail)))))

(define (const-setreg as i:const i:setreg tail)
  (let ((c  (operand1 i:const))
	(rd (operand1 i:setreg)))
    (if (hwreg? rd)
	(as-source! as (cons (list $const/setreg c rd) tail)))))

; Make-vector on vectors of known short length.

(define (const-op2 as i:const i:op2 tail)
  (let ((vn '#(make-vector:0 make-vector:1 make-vector:2 make-vector:3
	       make-vector:4 make-vector:5 make-vector:6 make-vector:7
	       make-vector:8 make-vector:9))
	(c  (operand1 i:const))
	(op (operand1 i:op2))
	(r  (operand2 i:op2)))
    (if (and (eq? op 'make-vector)
	     (fixnum? c)
	     (<= 0 c 9))
	(as-source! as (cons (list $op2 (vector-ref vn c) r) tail)))))

; Constants that can be synthesized in a single instruction can be
; moved into RESULT in the delay slot of the return instruction.

(define (const-return as i:const i:return tail)
  (let ((c (operand1 i:const)))
    (if (or (and (number? c) (immediate-int? c))
	    (null? c)
	    (boolean? c))
	(as-source! as (cons (list $const/return c) tail)))))

; This allows the use of hardware 'call' instructions.
;    (setrtn Lx)
;    (branch Ly k)
;    (.align k)            Ignored on SPARC
;    (.label Lx)
; => (setrtn/branch Ly k)
;    (.label Lx)

(define (setrtn-branch as i:setrtn i:branch i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
	(branch-ops   (cdr i:branch))
	(label        (operand1 i:label)))
    (if (= return-label label)
	(as-source! as (cons (cons $setrtn/branch branch-ops)
			     (cons i:label
				   tail))))))

; Ditto for 'invoke'.
;
; Disabled because it does _not_ pay off on the SPARC currently -- 
; probably, the dependency created between 'jmpl' and 'st' is not 
; handled well on the test machine (an Ultrasparc).  Might work 
; better if the return address were to be kept in a register always.

(define (setrtn-invoke as i:setrtn i:invoke i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
	(invoke-ops   (operand1 i:invoke))
	(label        (operand1 i:label)))
    (if (and #f				; DISABLED
	     (= return-label label))
	(as-source! as (cons (cons $setrtn/invoke invoke-ops)
			     (cons i:label
				   tail))))))

; Gets rid of spurious branch-to-next-instruction
;    (branch Lx k)
;    (.align y)
;    (.label Lx)
; => (.align y)
;    (.label Lx)

(define (branch-and-label as i:branch i:align i:label tail)
  (let ((branch-label (operand1 i:branch))
	(label        (operand1 i:label)))
    (if (= branch-label label)
	(as-source! as (cons i:align (cons i:label tail))))))

(define (global-setreg as i:global i:setreg tail)
  (let ((global (operand1 i:global))
	(rd     (operand1 i:setreg)))
    (if (hwreg? rd)
	(as-source! as (cons (list $global/setreg global rd) tail)))))

; Obscure guard: unsafe-code = #t implies that global/invoke will not
; check the value of the global variable, yet unsafe-code and
; catch-undefined-globals are supposed to be independent.

(define (global-invoke as i:global i:invoke tail)
  (let ((global (operand1 i:global))
	(argc   (operand1 i:invoke)))
    (if (not (and (unsafe-code) (catch-undefined-globals)))
	(as-source! as (cons (list $global/invoke global argc) tail)))))

; Obscure guard: see comment for previous procedure.
; FIXME!  This implementation is temporary until setrtn-invoke is enabled.

(define (global-setrtn-invoke as i:global i:setrtn i:invoke tail)
  (let ((global (operand1 i:global))
	(argc   (operand1 i:invoke)))
    (if (not (and (unsafe-code) (catch-undefined-globals)))
	(as-source! as (cons i:setrtn 
			     (cons (list $global/invoke global argc)
				   tail))))))
  
(define (reg-setglbl as i:reg i:setglbl tail)
  (let ((rs     (operand1 i:reg))
	(global (operand1 i:setglbl)))
    (if (hwreg? rs)
	(as-source! as (cons (list $reg/setglbl rs global) tail)))))



; Test code

(define (peeptest istream)
  (let ((as (make-assembly-structure istream)))
    (let loop ((l '()))
      (if (null? (as-source as))
	  (reverse l)
	  (begin (peep as)
		 (let ((a (car (as-source as))))
		   (as-source! as (cdr (as-source as)))
		   (loop (cons a l))))))))


; eof
