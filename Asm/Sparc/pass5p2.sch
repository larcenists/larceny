; Asm/Sparc/pass5p2.sch
; Larceny -- Sparc machine assembler, top level
;
; $Id$
;
; Based on MacScheme machine assembler:
;    Copyright 1991 Lightship Software, Incorporated.
;
; The assembly instruction generators are themselves in the files
; Asm/Sparc/gen-msi.sch and Asm/Sparc/gen-prim.sch.

; Controls listing of instructions during assembly.

(define listify? #f)

; Table of assembler procedures.

(define $sparc-assembly-table$
  (make-vector
   64
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

; Given a numeric (MAL) label, prepend a L and make it a symbol,
; so that the SPARC assembler can interpret it correctly.

(define (make-asm-label q)
  (string->symbol (string-append
		   "Q"
		   (number->string q))))

; Create an internal label; prepend with Q to distinguish from MAL labels.

(define new-label
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "Q" (number->string n))))))

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (sparc.label as (make-asm-label (operand1 instruction)))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)
    #t))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)
    #t))

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    #t))

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

; Questionable use of argreg2 here?
;
; FIXME: This is sub-optimal in that it generates the constant into a 
; register before operating on it; however, peephole optimization rewrites 
; critical code to use other operations, so the problem is minor. 

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "opx" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop.2arg! as
		       (operand1 instruction)
		       $r.argreg2)))

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
    (emit-result-register->global! as
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
			      (lambda (segment)
				(set-constant! as code-offset (car segment))
				(set-constant! as const-offset (cdr segment))))
      (list-lambda-end)
      (set! code-offset  (emit-codevector as 0))
      (set! const-offset (emit-constantvector as 0))
      ; (FIXME: is this true?)
      ; Documentation is ignored.  It could be assigned to the constant
      ; vector offset that holds the documentation.
      (emit-lambda! as
		    code-offset
		    const-offset
		    (operand2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    ; (FIXME: is this true?)
    ; Documentation is ignored for now.
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
    (emit-invoke! as (operand1 instruction))))

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
    (emit-setrtn! as (operand1 instruction))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-apply! as
		 (regname (operand1 instruction))
		 (regname (operand2 instruction)))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-jump! as (operand1 instruction) (operand2 instruction))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-branch! as #f (operand1 instruction))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-branch! as #t (operand1 instruction))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-branchf! as (operand1 instruction))))


; Operations introduced by the peephole optimizer.
;
; General mnemonic key:
;
;   optbregN     operation test-and-branch with op1 in explicit reg
;   optbregNimm  ditto with op2 an immediate (rather than explicit reg)
;   dresopN      operation with explicit source and target registers
;   dresopNimm   ditto with op2 an immediate (rather than explicit reg)
;
; A register can always be the symbol RESULT. (eh?)


; ($optbreg1 test-op src1 label)
; Test register and branch if false to label.

(define-instruction $optbreg1
  (let ((names '((null? . internal:bfnull?)
		 (zero? . internal:bfzero?)
		 (pair? . internal:bfpair?))))
    (lambda (instruction as)
      (list-instruction "optbreg1" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to optbreg1: " instruction)
	    (emit-primop.3arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)))))))

; ($optbreg2 test-op src1 src2 label)
; Test two registers and branch if false to label.

(define-instruction $optbreg2
  (let ((names '((<       . internal:bf<)
		 (>       . internal:bf>)
		 (<=      . internal:bf<=)
		 (>=      . internal:bf>=)
		 (=       . internal:bf=)
		 (char=?  . internal:bfchar=?)
		 (char<=? . internal:bfchar<=?)
		 (char<?  . internal:bfchar<?)
		 (char>=? . internal:bfchar>=?)
		 (char>?  . internal:bfchar>?)
		 (eq?     . internal:bfeq?))))
    (lambda (instruction as)
      (list-instruction "optb2reg" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to optbreg2: " instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))
			       (operand4 instruction)))))))

; ($optbreg2imm test-op src1 imm label)
; Test register and immediate and branch if false to label.
;
; Cheats: loads constant into register, then does register-register case.
; Dubious use of argreg2?
 
(define-instruction $optbreg2imm
  (let ((names '((<       . internal:bf<imm)
		 (>       . internal:bf>imm)
		 (<=      . internal:bf<=imm)
		 (>=      . internal:bf>=imm)
		 (=       . internal:bf=imm)
		 (char=?  . internal:bfchar=?imm)
		 (char>=? . internal:bfchar>=?imm)
		 (char>?  . internal:bfchar>?imm)
		 (char<=? . internal:bfchar<=?imm)
		 (char<?  . internal:bfchar<?imm)
		 (eq? . internal:bfeq?imm))))
    (lambda (instruction as)
      (list-instruction "optbreg2imm" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to optbreg2imm: " instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)
			       (operand4 instruction)))))))

; ($dresop1 op src1 dest)
; Operate on register and put result in register.

(define-instruction $dresop1
  (let ((names `((car            . internal:car2reg)
		 (cdr            . internal:cdr2reg)
		 (,name:CELL-REF . internal:cellref2reg))))
    (lambda (instruction as)
      (list-instruction "dresop1" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to dresop1: " instruction)
	    (emit-primop.3arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))))))))

; ($dresop2 op src1 src2 dest)
; Operate on two registers and put result in register.

(define-instruction $dresop2
  (let ((names '((+    . internal:+2reg)
		 (-    . internal:-2reg)
		 (eq?  . internal:eq2reg)
		 (cons . internal:cons2reg))))
    (lambda (instruction as)
      (list-instruction "dresop2" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to dresop2: " instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))
			       (if (eq? (operand4 instruction) 'RESULT)
				   $r.result
				   (regname (operand4 instruction)))))))))

; ($dresop2imm op src1 imm dest)
; Operate on register and immediate and put result in register.

(define-instruction $dresop2imm
  (let ((names '((+ . internal:+imm2reg)
		 (- . internal:-imm2reg))))
    (lambda (instruction as)
      (list-instruction "dresop2imm" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (asm-error "Invalid op to dresop2imm: " instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)
			       (if (eq? (operand4 instruction) 'RESULT)
				   $r.result
				   (regname (operand4 instruction)))))))))

; ($constreg const dest)
; Move constant directly to register.

(define-instruction $constreg
  (lambda (instruction as)
    (list-instruction "constreg" instruction)
    (emit-constant->register as
			     (operand1 instruction)
			     (regname (operand2 instruction)))))

; ($branchfreg reg label)
; branch to label if reg is false.

(define-instruction $branchfreg
  (lambda (instruction as)
    (list-instruction "branchfreg" instruction)
    (emit-branchfreg! as 
		      (regname (operand1 instruction))
		      (operand2 instruction))))

; We emit a singlestep breakpoint for all MacScheme instructions except
; the pseudo-operations and a load into R0.

(define (emit-singlestep! as instr)

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
	  (emit-singlestep-instr! as funky? 0 o)))))

; eof
