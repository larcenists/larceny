; Asm/Sparc/pass5p2.sch
; Larceny -- Sparc machine assembler, top level
;
; $Id: pass5p2.sch,v 1.1.1.1 1998/11/19 21:51:58 lth Exp $
;
; Based on MacScheme machine assembler:
;    Copyright 1991 Lightship Software, Incorporated.
;
; The assembly instruction generators are themselves in the files
; Asm/Sparc/gen-msi.sch and Asm/Sparc/gen-prim.sch.

; Overrides the procedure of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $sparc-assembly-table$)

; Controls listing of instructions during assembly.

(define listify? #f)

; Table of assembler procedures.

(define $sparc-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $sparc-assembly-table$ i proc)
  #t)

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 12 (string-length name)) 1)
                                   #\space))
             (if (not (null? (cdr instruction)))
                 (begin (write (cadr instruction))
                        (do ((operands (cddr instruction)
                                       (cdr operands)))
                            ((null? operands))
                            (write-char #\,)
                            (write (car operands)))))
             (newline)
	     (flush-output-port))))

(define (list-label instruction)
  (if listify?
      (begin (display list-indentation)
             (write-char #\L)
             (write (cadr instruction))
             (newline))))

(define (list-lambda-start instruction)
  (list-instruction "lambda" (list $lambda '* (operand2 instruction)))
  (set! list-indentation (string-append list-indentation "|   ")))

(define (list-lambda-end)
  (set! list-indentation
        (substring list-indentation
                   0
                   (- (string-length list-indentation) 4))))

(define list-indentation "")

; Utilities

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (sparc.label as (make-asm-label as (operand1 instruction)))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)
    #t))

(define-instruction $.proc-doc
  (lambda (instruction as)
    (list-instruction ".proc-doc" instruction)
    (add-documentation as (operand1 instruction))
    #t))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)
    #t))

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    #t))

(define-instruction $.end
  (lambda (instruction as)
    #t))

(define-instruction $.singlestep
  (lambda (instruction as)
    (let ((instr (car (as-source as))))

      (define (special?)
	(let ((op (operand0 instr)))
	  (or (= op $.label)
	      (= op $.proc)
	      (= op $.cont)
	      (= op $.align)
	      (and (= op $load) (= 0 (operand1 instr))))))

      (define (readify-instr)
	(if (= (operand0 instr) $lambda)
	    (list 'lambda '(...) (caddr instr) (cadddr instr))
	    (car (readify-lap (list instr)))))

      (if (not (special?))
	  (let ((repr   (format-object (readify-instr)))
		(funky? (= (operand0 instr) $restore)))
	    (let ((o (emit-datum as repr)))
	      (emit-singlestep-instr! as funky? 0 o)))))))


; Instructions.

; A hack to deal with the MacScheme macro expander's treatment of 1+ and 1-.

(define-instruction $op1
  (lambda (instruction as)
    (cond ((eq? (operand1 instruction) (string->symbol "1+"))
	   (push-instruction as (list $op2imm '+ 1)))
	  ((eq? (operand1 instruction) (string->symbol "1-"))
	   (push-instruction as (list $op2imm '- 1)))
	  (else
	   (list-instruction "op1" instruction)
	   (emit-primop.1arg! as (operand1 instruction))))))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-primop.2arg! as
		       (operand1 instruction)
		       (regname (operand2 instruction)))))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-primop.3arg! as
		       (operand1 instruction)
		       (regname (operand2 instruction))
		       (regname (operand3 instruction)))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (let ((op (case (operand1 instruction)
		((+) 'internal:+/imm)
		((-) 'internal:-/imm)
		(else #f))))
      (if op
	  (emit-primop.4arg! as op $r.result (operand2 instruction) $r.result)
	  (begin
	    (emit-constant->register as (operand2 instruction) $r.argreg2)
	    (emit-primop.2arg! as
			       (operand1 instruction)
			       $r.argreg2))))))

(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (emit-constant->register as (operand1 instruction) $r.result)))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-global->register! as
			    (emit-global as (operand1 instruction))
			    $r.result)))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-register->global! as
			    $r.result
			    (emit-global as (operand1 instruction)))))

; FIXME: A problem is that the listing is messed up because of the delayed
; assembly; somehow we should fix this by putting an identifying label
; in the listing and emitting this label later, with the code.

(define-instruction $lambda
  (lambda (instruction as)
    (let ((code-offset  #f)
	  (const-offset #f))
      (list-lambda-start instruction)
      (assemble-nested-lambda as
			      (operand1 instruction)
			      (operand3 instruction)   ; documentation
			      (lambda (nested-as segment)
				(set-constant! as code-offset (car segment))
				(set-constant! as const-offset (cdr segment))))
      (list-lambda-end)
      (set! code-offset  (emit-codevector as 0))
      (set! const-offset (emit-constantvector as 0))
      (emit-lambda! as
		    code-offset
		    const-offset
		    (operand2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (emit-lexes! as (operand1 instruction))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (emit-args=! as (operand1 instruction))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (emit-args>=! as (operand1 instruction))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-invoke as (operand1 instruction) #f $m.invoke-ex)))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "restore" instruction)
         (emit-restore! as (operand1 instruction))))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "pop" instruction)
         (let ((next (next-instruction as)))
           (if (and (peephole-optimization)
		    (eqv? $return (operand0 next)))
               (begin (list-instruction "return" next)
                      (consume-next-instruction! as)
                      (emit-pop! as (operand1 instruction) #t))
               (emit-pop! as (operand1 instruction) #f)))))))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-load! as (operand1 instruction) $r.result)))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-store! as $r.result (operand1 instruction))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-load! as (operand2 instruction) (regname (operand1 instruction)))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-store! as (regname (operand1 instruction)) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (emit-lexical! as (operand1 instruction) (operand2 instruction))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-setlex! as (operand1 instruction) (operand2 instruction))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-register->register! as (regname (operand1 instruction)) $r.result)))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-register->register! as $r.result (regname (operand1 instruction)))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-register->register! as 
			      (regname (operand1 instruction))
			      (regname (operand2 instruction)))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-return! as)))

(define-instruction $reg/return
  (lambda (instruction as)
    (list-instruction "reg/return" instruction)
    (emit-return-reg! as (regname (operand1 instruction)))))

(define-instruction $const/return
  (lambda (instruction as)
    (list-instruction "const/return" instruction)
    (emit-return-const! as (operand1 instruction))))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)
    (sparc.nop as)))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "save" instruction)
         (let* ((n (operand1 instruction))
                (v (make-vector (+ n 1) #t)))
           (emit-save0! as n)
           (if (peephole-optimization)
               (let loop ((instruction (next-instruction as)))
                 (if (eqv? $store (operand0 instruction))
                     (begin (list-instruction "store" instruction)
                            (emit-store! as
                                         (regname (operand1 instruction))
                                         (operand2 instruction))
                            (consume-next-instruction! as)
                            (vector-set! v (operand2 instruction) #f)
                            (loop (next-instruction as))))))
           (emit-save1! as v))))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-setrtn! as (make-asm-label as (operand1 instruction)))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-apply! as
		 (regname (operand1 instruction))
		 (regname (operand2 instruction)))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-jump! as
		(operand1 instruction)
		(make-asm-label as (operand2 instruction)))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-branch! as #f (make-asm-label as (operand1 instruction)))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-branch! as #t (make-asm-label as (operand1 instruction)))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-branchf! as (make-asm-label as (operand1 instruction)))))


; Operations introduced by the peephole optimizer.

(define (peep-regname r)
  (if (eq? r 'RESULT) $r.result (regname r)))

(define-instruction $reg/op1/branchf
  (lambda (instruction as)
    (list-instruction "reg/op1/branchf" instruction)
    (emit-primop.3arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (make-asm-label as (operand3 instruction)))))

(define-instruction $reg/op2/branchf
  (lambda (instruction as)
    (list-instruction "reg/op2/branchf" instruction)
    (emit-primop.4arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (peep-regname (operand3 instruction))
		       (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/op2imm/branchf
  (lambda (instruction as)
    (list-instruction "reg/op2imm/branchf" instruction)
    (emit-primop.4arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (operand3 instruction)
		       (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/op1/setreg
  (lambda (instruction as)
    (list-instruction "reg/op1/setreg" instruction)
    (emit-primop.3arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (peep-regname (operand3 instruction)))))

(define-instruction $reg/op2/setreg
  (lambda (instruction as)
    (list-instruction "reg/op2/setreg" instruction)
    (emit-primop.4arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (peep-regname (operand3 instruction))
		       (peep-regname (operand4 instruction)))))

(define-instruction $reg/op2imm/setreg
  (lambda (instruction as)
    (list-instruction "reg/op2imm/setreg" instruction)
    (emit-primop.4arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (operand3 instruction)
		       (peep-regname (operand4 instruction)))))

(define-instruction $reg/op3 
  (lambda (instruction as)
    (list-instruction "reg/op3" instruction)
    (emit-primop.4arg! as
		       (operand1 instruction)
		       (peep-regname (operand2 instruction))
		       (peep-regname (operand3 instruction))
		       (peep-regname (operand4 instruction)))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (emit-constant->register as
			     (operand1 instruction)
			     (regname (operand2 instruction)))))

(define-instruction $reg/branchf
  (lambda (instruction as)
    (list-instruction "reg/branchf" instruction)
    (emit-branchfreg! as 
		      (regname (operand1 instruction))
		      (make-asm-label as (operand2 instruction)))))

(define-instruction $setrtn/branch
  (lambda (instruction as)
    (list-instruction "setrtn/branch" instruction)
    (emit-branch-with-setrtn! as (make-asm-label as (operand1 instruction)))))

(define-instruction $setrtn/invoke
  (lambda (instruction as)
    (list-instruction "setrtn/invoke" instruction)
    (emit-invoke as (operand1 instruction) #t $m.invoke-ex)))

(define-instruction $global/setreg
  (lambda (instruction as)
    (list-instruction "global/setreg" instruction)
    (emit-global->register! as
			    (emit-global as (operand1 instruction))
			    (regname (operand2 instruction)))))

(define-instruction $global/invoke
  (lambda (instruction as)
    (list-instruction "global/invoke" instruction)
    (emit-load-global as
		      (emit-global as (operand1 instruction))
		      $r.result
		      #f)
    (emit-invoke as (operand2 instruction) #f $m.global-invoke-ex)))

(define-instruction $reg/setglbl
  (lambda (instruction as)
    (list-instruction "reg/setglbl" instruction)
    (emit-register->global! as
			    (regname (operand1 instruction))
			    (emit-global as (operand2 instruction)))))

; eof
