; Copyright 1991 Lightship Software, Incorporated.
;
; $Id: pass5p2-sassy.sch 2543 2005-07-20 21:54:03Z pnkfelix $
;
; Intel x86 machine assembler, building on Sassy.
; Felix S Klock.
;
; Code generation strategy:
;   Each MAL basic block is compiled to a separate ASM procedure, with
;   the name compiled_block_x_y where x is a procedure label (unique
;   in a file) and y is the label at the block's entry point.
;
;   Procedure entry points do not have labels, and the entry points
;   are named compiled_start_x_y, where y is some unique identifier.
;
;   A list of procedure names is maintained (with a flag for whether they
;   are conditional or not); they can be used to create forward declarations
;   when assembly is done.
;
; Overrides the procedures of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $x86-sassy-assembly-table$)

(define (assembly-start as)
  (let ((u (as-user as)))
    (user-data.proc-counter! u 0)
    (user-data.toplevel-counter! u (+ 1 (user-data.toplevel-counter u))))
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as)))))

(define (assembly-end as segment)
  (list (car segment) (cdr segment) (lookup-functions as)))

(define (assembly-user-data)
  (make-user-data))

(define (assembly-declarations user-data)
  (append (if (not (runtime-safety-checking))
	      '("%define UNSAFE_CODE")
	      '())
	  (if (not (catch-undefined-globals))
	      '("%define UNSAFE_GLOBALS")
	      '())
	  (if (inline-allocation)
	      '("%define INLINE_ALLOCATION")
	      '())
	  (if (inline-assignment)
	      '("%define INLINE_ASSIGNMENT")
	      '())))

; User-data structure has three fields:
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for labels
;  seen-labels          A list of labels at lower addresses

(define (make-user-data) (list 0 0 '()))

(define (user-data.toplevel-counter u) (car u))
(define (user-data.proc-counter u) (cadr u))
(define (user-data.labels u) (caddr u))

(define (user-data.toplevel-counter! u x) (set-car! u x))
(define (user-data.proc-counter! u x) (set-car! (cdr u) x))
(define (user-data.labels! u x) (set-car! (cddr u) x))


; Assembly listing.

(define listify? #f)

(define $x86-sassy-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $x86-sassy-assembly-table$ i proc)
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


; Auxiliary assembler interface.

(define emit-text-noindent
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

(define emit-text
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (error 'emit-text "calls to emit-text should be replaced in Sassy")
      (emit-string! as code-indentation)
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

(define (begin-compiled-scheme-function as label entrypoint? start?)
  (let ((name (compiled-procedure as label)))
    (emit-text as "begin_codevector ~a" name)
    (add-function as name #t entrypoint?)
    (set! code-indentation (string #\tab))
    (set! code-name name)))

(define (add-compiled-scheme-function as label entrypoint? start?)
  (let ((name (compiled-procedure as label)))
    (if (not (assoc name (lookup-functions as)))
        (add-function as name #t entrypoint?))))

(define (end-compiled-scheme-function as)
  (set! code-indentation "")
  (emit-text as "end_codevector ~a" code-name)
  (emit-text as ""))

(define code-indentation "")
(define code-name "")

(define (lookup-functions as)
  (or (assembler-value as 'functions) '()))

(define (add-function as name definite? entrypoint?)
  (assembler-value! as 'functions (cons (list name definite? entrypoint?)
					(lookup-functions as)))
  name)
    
; Pseudo-instructions.

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    (emit-sassy as
     `(align ,(operand1 instruction)))
    ))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)))

(define-instruction $.end
  (lambda (instruction as)
    (list-instruction ".end" instruction)
    (end-compiled-scheme-function as)))

(define-instruction $.entry
  (lambda (instruction as)
    (list-instruction ".entry" instruction)
    (begin-compiled-scheme-function as (operand1 instruction)
				    (operand2 instruction)
				    #t)))

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (add-compiled-scheme-function as (operand1 instruction) #f #f)
    (let ((u (as-user as)))
      (user-data.labels! u (cons (operand1 instruction) (user-data.labels u))))
    (emit-sassy as 
	`(label ,(compiled-procedure as (operand1 instruction))))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)))

(define-instruction $.proc-doc
  (lambda (instruction as)
    (list-instruction ".proc-doc" instruction)
    (add-documentation as (operand1 instruction))))

(define-instruction $.singlestep
  (lambda (instruction as)
    ; Use GDB.
    #t))
    

; Instructions.

; A hack to deal with the MacScheme macro expander's treatment of 1+ and 1-.

(define-instruction $op1
  (lambda (instruction as)
    (list-instruction "op1" instruction)
    (emit-primop.1arg! as (operand1 instruction))))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-primop.2arg! as
		       (operand1 instruction)
		       (regname (operand2 instruction)))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop.2arg! as (operand1 instruction) $r.argreg2)))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-primop.3arg! as
		       (operand1 instruction)
		       (regname (operand2 instruction))
		       (regname (operand3 instruction)))))

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

(define-instruction $lambda
  (lambda (instruction as)
    (let* ((const-offset #f)
	   (code-offset  #f)
	   (entry        (new-proc-id as)))
      (list-lambda-start instruction)
      (assemble-nested-lambda
       as
       (cons (list $.entry entry #f)
	     (operand1 instruction))
       (operand3 instruction)
       (lambda (nested-as segment)
	 (assembler-value! as 'functions
			   (append (lookup-functions as)
				   (lookup-functions nested-as)))
	 (set-constant! as code-offset (car segment))
	 (set-constant! as const-offset (cdr segment)))
       (as-user as))
      (list-lambda-end)
      (set! code-offset (emit-codevector as 0))
      (set! const-offset (emit-constantvector as 0))
      (emit-lambda! as 
		    code-offset
		    const-offset
		    (operan2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (let ((n (operand1 instruction)))
      (emit-sassy 
       as
       `((const2regf $r.result (fixnum (+ PROC_HEADER_WORDS 
					  PROC_OVERHEAD_WORDS
					  ,n
					  1)))
	 (alloc)
	 (mov (dword $r.result) (| (<< (words2bytes 
					(+ PROC_OVERHEAD_WORDS ,n 1))
					 8)
				     PROC_HDR))
	 (loadr TEMP 0)
	 (mov TEMP (+ (- TEMP PROC_TAG) PROC_CODEVECTOR_NATIVE))
	 (mov (dword (+ RESULT PROC_CODEVECTOR_NATIVE)) TEMP)
	 (loadr TEMP 0)
	 (mov TEMP (+ (- TEMP PROC_TAG) PROC_CONSTVECTOR))
	 (mov (dword (+ RESULT PROC_CONSTVECTOR)) TEMP)
	 (init_closure ,n))))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (if (not (unsafe-code))
	(let ((n (operand1 instruction)))
	  (emit-sassy as
	   `((locals (L1 L0)
	       (label L0
		 (cmp RESULT (fixnum ,n))
		 (je short L1)
		 (mcall M_ARGC_EX)
		 (jmp L0))
	       (label L1))))))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (let ((n (operand1 instruction)))
      (emit-sassy as
	`((const2regf SECOND (fixnum ,n))
	  ,@(if (and (not (unsafe-code))
		    (> n 0))
	       (list `(locals (L0 L1)
			(label L0
			  (cmp RESULT SECOND)
			  (jge short L1)
			  (mcall M_ARGC_EX)
			  (jmp L0))
			(label L1)))
	       (list))
	  (mcall M_VARARGS))))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (let ((tgt (operand1 instruction)))
      (emit-sassy as
       (if (unsafe-code)
	   `((timer-check)
	     (storer 0 RESULT)
	     (mov TEMP RESULT)
	     (const2regf RESULT (fixnum ,tgt)) ; bogus
	     (jmp (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
	   `(locals (L0 L1)
	      (dec dword (+ GLOBALS G_TIMER))
	      (jnz short L1)
	      (label L0
		(mcall M_INVOKE_EX))
	      (label L1
		(lea TEMP (+ RESULT (- 8 PROC_TAG)))
		(test TEMP_LOW tag_mask)
		(jnz short L0)
		;; Observe TEMP points to proc+8 here and that if we were
		;; really perverse we would lay the procedure out so that
		;; the codevector is stored at offset 4, reducing the size
		;; of the JMP instruction below.
		(storer	0 RESULT)
		(const2regf RESULT (fixnum ,tgt))
		(jmp	(+ TEMP -8 PROC_CODEVECTOR_NATIVE)))))))))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "restore" instruction)
	  (let ((n (min (operand1 instruction) (- *nregs* 1))))
	    (let rep ((slotno 0))
	      (cond ((<= slotno n)
		     (emit-sassy as
		      (if (is_hwreg slotno)
			  `((mov (+ REG ,slotno) (dword (stkslot ,slotno))))
			  `((mov TEMP (dword (stkslot ,slotno)))
			    (mov (& (+ GLOBALS (G_REG ,slotno))) TEMP))))
		     (rep (+ slotno 1))))))))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "pop" instruction)
	  (emit-sassy as `(add CONT (framesize ,(operand1 instruction))))))))

(define-instruction $popstk
  (lambda (instruction as)
    (error "POPSTK is not yet implemented by the x86-SASSY assembler.")))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-sassy as `(mov RESULT (stkslot ,(operand1 instruction))))))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-sassy as `(mov (& (stkslot ,(operand1 instruction))) RESULT))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-sassy as
     (if (is_hwreg (operand1 instruction))
	 `(mov (reg ,(operand1 instruction)) (& (stkslot ,(operand2 instruction))))
	 `(begin
	    (mov TEMP (& (stkslot ,(operand2 instruction))))
	    (storer ,(operand1 instruction) TEMP))))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-store as (operand1 instruction) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (let rep ((ribno 0))
      (cond ((< ribno (operand1 instruction))
	     (emit-sassy as
	      `(mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0))))
	     (rep (+ ribno 1)))))
    (emit-sassy as
     `(mov RESULT (& (+ TEMP (- PROC_TAG) PROC_REG0 (words2bytes ,(operand2 instruction))))))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-sassy as
     `(loadr TEMP 0))
    (let rep ((ribno 0))
      (cond ((< ribno (operand1 instruction))
	     (emit-sassy as
	      `(mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0))))
	     (rep (+ ribno 1)))))
    (emit-sassy as
     `(mov (& (+ TEMP (- PROC_TAG) PROC_REG0 (words2bytes ,(operand2 instruction)))) RESULT))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-sassy as `(loadr RESULT ,(operand1 instruction)))))

;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-sassy as `(storer ,(operand1 instruction) RESULT))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-sassy as
     (cond ((is_hwreg (operand1 instruction))
	    `(storer ,(operand2 instruction) (reg ,(operand1 instruction))))
	   ((is_hwreg (operand2 instruction))
	    `(loadr  ,(reg ,(operand2 instruction)) (operand1 instruction)))
	   (else
	    `(begin 
	       (loadr TEMP ,(operand1 instruction))
	       (storer ,(operand2 instruction) TEMP)))))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-sassy as '(jmp (& (+ CONT STK_RETADDR))))))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)))


; (define-instruction $save
;   (lambda (instruction as)
;     (if (not (negative? (operand1 instruction)))
;         (begin
;          (list-instruction "save" instruction)
; 	 (emit-text as "T_SAVE ~a" (operand1 instruction))))))

(define (emit-save0 as n)
  (emit-sassy 
   `(locals (L0 L1)
      (label L0
	(sub CONT (framesize ,n))
	(cmp CONT (& (+ GLOBALS G_ETOP)))
	(jge short L1)
	(add CONT (framesize ,n))
	(mcall M_STKOFLOW)
	(jmp L0))
      (label L1
	(mov (dword (& CONT)) (recordedsize ,n))
	;; Not necessary to store reg0 here, this is handled
	;; explicitly by the generated code.
	(xor	RESULT RESULT)
	(mov	(dword (& (+ CONT STK_RETADDR))) RESULT)
	,@(if (= (- (framesize n) (recordedsize n)) 8)
	      ;; We have a pad word at the end -- clear it
	      (list `(mov (dword (& (stkslot (+ n 1)))) RESULT))
	      (list))))))


;;; Initialize the numbered slot to the value of RESULT.
;;; Using RESULT is probably OK because it is almost certainly 0
;;; after executing T_SAVE0 and only T_STORE instructions
;;; after that.
(define (emit-save1 as i)
  (emit-sassy as `(mov (dword (& (stkslot ,i))) RESULT)))
(define (emit-store src tgt)
  (emit-sassy as
   (if (is_hwreg ,src)
       `(mov (& (stkslot ,tgt)) (reg ,src))
       `(begin 
	  (loadr TEMP ,src)
	  (mov (& (stkslot ,tgt)) TEMP)))))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
	  (list-instruction "save" instruction)
	  (let* ((n (operand1 instruction))
		 (v (make-vector (+ n 1) #t)))
	    (emit-save0 as n)
	    (if (peephole-optimization)
		(let loop ((instruction (next-instruction as)))
		  (if (eqv? $store (operand0 instruction))
		      (begin (list-instruction "store" instruction)
			     (emit-store as 
					 (operand1 instruction)
					 (operand2 instruction))
			     (consume-next-instruction! as)
			     (vector-set! v (operand2 instruction) #f)
			     (loop (next-instruction as))))))
	    (do ((i 0 (+ i 1)))
		((= i (vector-length v)))
	      (if (vector-ref v i)
		  (emit-save1 as i))))))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-sassy as `(mov (dword (& (+ CONT STK_RETADDR))) 
			 (t_label ,(compiled-procedure (operand1 instruction)))))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-sassy as
     `(begin
	(timer-check)
	(loadr TEMP ,(operand2 instruction))
	(mov (& (+ GLOBALS G_THIRD)) TEMP)
	(loadr SECOND ,(operand1 instruction))
	(mcall M_APPLY)
	(loadr TEMP 0)
	(mov TEMP (& (+ TEMP (- PROC_TAG) PROC_CODEVECTOR_NATIVE)))
	(jmp TEMP)))))

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; in this implementation)

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (let ((levels (operand1 instruction))
	  (label  (operand2 instruction))
	  (name   (compiled-procedure (operand2 instruction))))
      (emit-sassy
       `(begin 
	  (timer-check)
	  ,@(if (> levels 0)
		(list `((loadr TEMP 0)
			,@(let rep ((ribno 0))
			    (cond ((< ribno levels)
				   (cons 
				    '(mov TEMP (& (+ TEMP (- PROC_TAG) PROC_REG0)))
				    (rep (+ ribno 1))))
				  (else 
				   (list))))
			(storer 0 TEMP)))
		(list))
	  (jmp ,name))))))

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

(define (emit-skip as label)
  (emit-sassy as `(jmp (t_label ,label))))
(define (emit-skipf as label)
  (emit-sassy as `(begin (cmp RESULT_LOW FALSE_CONST)
			 (je (t_label ,label)))))
(define (emit-branch as label)
  (emit-sassy as `(begin (dec (dword (& (+ GLOBALS G_TIMER))))
			 (jnz (t_label ,label))
			 (mcall M_TIMER_EXCEPTION)
			 (jmp (t_label ,label)))))
(define (emit-branchf as label)
  (emit-sassy as `(begin (timer-check)
			 (cmp RESULT_LOW FALSE_CONST)
			 (je (t_label ,label)))))
(define (emit-check as x1 x2 x3 x4)
  (emit-sassy as `(begin (cmp RESULT_LOW FALSE_CONST)
			 (je (t_label ,x4)))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-skip as (compiled-procedure as (operand1 instruction)))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-text as
	       (if (memq (operand1 instruction) 
			 (user-data.labels (as-user as)))
		   "T_BRANCH ~a"
		   "T_SKIP ~a")
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-text as 
	       (if (memq (operand1 instruction)
			 (user-data.labels (as-user as)))
		   "T_BRANCHF ~a" 
		   "T_SKIPF ~a")
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction "check" instruction)
    (emit-text as "T_CHECK ~a, ~a, ~a, ~a"
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (compiled-procedure as (operand4 instruction)))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction "trap" instruction)
    (emit-text as "T_TRAP ~a, ~a, ~a, ~a"
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (operand4 instruction))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (if (immediate-constant? (operand1 instruction))
	(emit-text as "T_CONST_SETREG_IMM  ~a, ~a"
		   (constant-value (operand1 instruction))
		   (operand2 instruction))
	(emit-text as "T_CONST_SETREG_CONSTVECTOR ~a, ~a"
		   (emit-datum as (operand1 instruction))
                   (operand2 instruction)))))

(define-instruction $op1/branchf
  (lambda (instruction as)
    (list-instruction "op1/branchf" instruction)
    (emit-text as "T_OP1_BRANCHF_~a ~a  ; ~a"
	       (op-primcode (operand1 instruction))
	       (compiled-procedure as (operand2 instruction))
	       (operand1 instruction))))

(define-instruction $op2/branchf
  (lambda (instruction as)
    (list-instruction "op2/branchf" instruction)
    (emit-text as "T_OP2_BRANCHF_~a ~a, ~a ; ~a"
	       (op-primcode (operand1 instruction))
	       (operand2 instruction)
	       (compiled-procedure as (operand3 instruction))
	       (operand1 instruction))))

(define-instruction $op2imm/branchf
  (lambda (instruction as)
    (list-instruction "op2imm/branchf" instruction)
    (emit-text as "T_OP2IMM_BRANCHF_~a  ~a, ~a  ; ~a"
	       (op-primcode (operand1 instruction))            ; Note, not op2imm-primcode
	       (constant-value (operand2 instruction))
	       (compiled-procedure as (operand3 instruction))
	       (operand1 instruction))))

(define-instruction $global/invoke
  (lambda (instruction as)
    (list-instruction "global/invoke" instruction)
    (emit-text as "T_GLOBAL_INVOKE  ~a, ~a  ; ~a" 
	       (emit-global as (operand1 instruction))
	       (operand2 instruction)
	       (operand1 instruction))))

;    Note, for the _check_ optimizations there is a hack in place.  Rather
;    than using register numbers in the instructions the assembler emits
;    reg(k) expressions when appropriate.  The reason it does this is so
;    that it can also emit RESULT when it needs to, since RESULT can
;    appear as a register name in these instructions.
;
;    This is a hack, but it beats having two versions of every macro.

; FIXME: not right -- REGn only works for HW registers!

'(define-instruction $reg/op1/check
  (lambda (instruction as)
    (list-instruction "reg/op1/check" instruction)
    (let ((rn (if (eq? (operand2 instruction) 'RESULT)
		  "RESULT"
		  (twobit-format #f "REG~a" (operand2 instruction)))))
      (emit-text as "T_REG_OP1_CHECK_~a ~a,~a   ; ~a with ~a"
		 (op-primcode (operand1 instruction))
		 rn
		 (operand3 instruction)
		 (operand1 instruction)
		 (operand4 instruction)))))

'(define-instruction $reg/op2/check
  (lambda (instruction as)
    (list-instruction "reg/op2/check" instruction)
    (let ((rn (if (eq? (operand2 instruction) 'RESULT)
		  "RESULT"
		  (twobit-format #f "REG~a" (operand2 instruction)))))
      (emit-text as "twobit_reg_op2_check_~a(~a,reg(~a),~a,~a); /* ~a with ~a */"
		 (op2-primcode (operand1 instruction))
		 rn
		 (operand3 instruction)
		 (operand4 instruction)
		 (compiled-procedure as (operand4 instruction))
		 (operand1 instruction)
		 (operand5 instruction)))))

'(define-instruction $reg/op2imm/check
  (lambda (instruction as)
    (list-instruction "reg/op2imm/check" instruction)
    (let ((rn (if (eq? (operand2 instruction) 'RESULT)
		  "RESULT"
		  (twobit-format #f "reg(~a)" (operand2 instruction)))))
      (emit-text as "twobit_reg_op2imm_check_~a(~a,~a,~a,~a); /* ~a with ~a */"
		 (op2-primcode (operand1 instruction)) ; Note, not op2imm-primcode
		 rn
		 (constant-value (operand3 instruction))
		 (operand4 instruction)
		 (compiled-procedure as (operand4 instruction))
		 (operand1 instruction)
		 (operand5 instruction)))))

; Helper procedures.

(define (compiled-procedure as label)
  (twobit-format #f "compiled_start_~a_~a" 
		 (user-data.toplevel-counter (as-user as))
		 label))

(define (immediate-constant? x)
  (or (fixnum? x)
      (null? x)
      (boolean? x)
      (char? x)
      (eof-object? x)
      (equal? x (unspecified))
      (equal? x (undefined))))

(define (constant-value x)

  (define (exact-int->fixnum x)
    (* x 4))

  (define (char->immediate c)
    (+ (* (char->integer c) 65536) $imm.character))

  (cond ((fixnum? x)              (twobit-format #f "fixnum(~a)" x))
        ((eq? x #t)               "TRUE_CONST")
        ((eq? x #f)               "FALSE_CONST")
	((equal? x (eof-object))  "EOF_CONST")
	((equal? x (unspecified)) "UNSPECIFIED_CONST")
	((equal? x (undefined))   "UNDEFINED_CONST")
        ((null? x)                "NIL_CONST")
        ((char? x)                (if (and (char>? x #\space)
					   (char<=? x #\~)
					   (not (char=? x #\\))
					   (not (char=? x #\')))
				      (twobit-format #f "char('~a')" x)
				      (twobit-format #f "char(~a)" 
						 (char->integer x))))
	(else ???)))

(define (new-proc-id as)
  (let* ((u (as-user as))
	 (x (user-data.proc-counter u)))
    (user-data.proc-counter! u (+ 1 x))
    x))

(define (op-primcode name)
  (prim-primcode (prim-entry-by-opcodename name)))

; eof
