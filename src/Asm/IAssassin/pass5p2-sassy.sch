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
  (reset-symbolic-label-cache!)
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as))))
  (current-sassy-assembly-structure as))

(define (check-for-free-ids code)
  (define (keyword? x)
    (case x
      ((eax ebx ecx edx edi esi esp ebp
         al  bl  cl  dl
         & align reloc abs
         mov push ret label short
         lea add sub cmp test neg 
         imul 
         and or xor shl shr sar not
         stosb rep cld
         call 
         jmp jz je jnz jne jg jng jge jl jnl jle jno jbe
         bl 
         pop
         inc dec 
         dword byte dwords) #t)
      (else #f)))
         
  (let ((need-labels  '())
        (found-labels '()))
    (define (sym x)
      (cond ((and (not (keyword? x))
                  (not (memq x found-labels))
                  (not (memq x need-labels)))
             (set! need-labels (cons x need-labels)))))

    (define (found-label! l)
      (if (memq l found-labels)
          (error 'check-for-free-labels " duplicate label " l))
      (set! found-labels 
            (cons l found-labels))
      (set! need-labels 
            (filter (lambda (x) (not (eq? x l))) need-labels)))
    
    (let rec ((x code))
      (cond
       ((number? x) 'ignore)
       ((null? x) 'ignore)
       ((pair? x) 
        (cond ((eq? (car x) 'label)
               (found-label! (cadr x))))
        (rec (car x)) 
        (rec (cdr x)))
       ((symbol? x)
        (sym x))
       (else (error 'check-for-free-ids " what is: " x))))
    
    (cond ((not (null? need-labels))
           (pretty-print code)
           (error 'check-for-free-ids " unbound: " need-labels)))))
       

    

(define (sassy-assemble as code)
  ;(begin (display code) (newline))
  (check-for-free-ids code)
  (sassy `(,@sassy-machine-directives 
           ,@sassy-instr-directives 
           ,@(map (lambda (l) `(export ,(string->symbol (compiled-procedure as l))))
                  (user-data.labels (as-user as)))
           (text ,@code))
         'dont-expand))

(define (assembly-end as segment)
  segment)

(define (assembly-user-data)
  (make-user-data))

(define (assembly-user-local)
  (make-user-local))

;; make-sassy-postpass : (AsmStruct AsmSegment SassyOutput -> X) -> AsmStruct AsmSegment -> X
(define (make-sassy-postpass k)
  (lambda (as seg)
    (let ((code (sassy-assemble as (car seg))))
      (user-local.sassy-input!  (as-user-local as) (car seg))
      (user-local.sassy-output! (as-user-local as) code)
      (for-each (lambda (entry) 
                  (let* ((sym-table (sassy-symbol-table code))
                         (sassy-sym (hash-table-ref 
                                     sym-table 
                                     (t_label (compiled-procedure as (car entry)))))
                         (offset (sassy-symbol-offset sassy-sym)))
                    (set-cdr! entry offset)))
                (as-labels as))
      (k as seg code))))

(set! assembly-postpass-segment
      (make-sassy-postpass 
       (lambda (as seg sassy-output)
         (cons (sassy-text-bytevector sassy-output)
               (cdr seg)))))

(define (assemble-read source . rest)
  (let* ((old-postpass assembly-postpass-segment)
         (new-postpass (make-sassy-postpass 
                        (lambda (as seg sassy-output)
                          ;; still need old-postpass, to store the
                          ;; generated sassy-output for offsets of
                          ;; labels for inner lambdas.  Discard
                          ;; result; output is a human readable Sexp.
                          (old-postpass as seg)
                          (cons (car seg) (cdr seg))))))
    (dynamic-wind 
        (lambda () (set! assembly-postpass-segment new-postpass))
        (lambda () (apply assemble source rest))
        (lambda () (set! assembly-postpass-segment old-postpass)))))
      
; User-data structure has three fields:
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for labels
;  seen-labels          A list of labels at lower addresses
;  local-counter        A serial number for (local) labels

(define (make-user-data) (list 0 0 '() 0 #f #f #f))

(define (user-data.toplevel-counter u) (car u))
(define (user-data.proc-counter u) (cadr u))
(define (user-data.labels u) (caddr u))
(define (user-data.local-counter u) (cadddr u))

(define (user-data.toplevel-counter! u x) (set-car! u x))
(define (user-data.proc-counter! u x) (set-car! (cdr u) x))
(define (user-data.labels! u x) (set-car! (cddr u) x))
(define (user-data.local-counter! u x) (set-car! (cdddr u) x))

;  sassy-output         <sassy-output> for code, or #f if none yet
;  sassy-input          The sexp that was passed to sassy-assemble, or #f
(define (make-user-local) (list #f #f))

(define (user-local.sassy-output u) (car u))
(define (user-local.sassy-input u) (car (cdr u)))

(define (user-local.sassy-output! u x) (set-car! u x))
(define (user-local.sassy-input! u x) (set-car! (cdr u) x))

(define (fresh-label)
  (let* ((local (user-data.local-counter 
                (as-user (current-sassy-assembly-structure))))
         (new-local (- local 1)))
    (user-data.local-counter! 
     (as-user (current-sassy-assembly-structure))
     new-local)
    (string->symbol
     (string-append ".L" (number->string new-local)))))

; Assembly listing.

(define listify? #f)

(define $x86-sassy-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $x86-sassy-assembly-table$ i 
               (lambda (i as)
                 (parameterize ((current-sassy-assembly-structure as))
                   (proc i as))))
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

(define emit-sassy 
  (lambda (as . x)
    (define (handle-added-code! as added-code)
      (cond ((null? (as-code as))
             (as-code! as (cons added-code (last-pair added-code))))
            (else
             (let* ((beg*end (as-code as))
                    (beg (car beg*end))
                    (end (cdr beg*end))
                    (new-end (last-pair added-code)))
               (set-cdr! end added-code)
               (as-code! as (cons beg new-end))))))
    (cond 
     ((procedure? (car x))
      (parameterize ((current-sassy-assembly-structure as))
        (let* ((added-code (apply do-sassy-instr (car x) (cdr x))))
          (handle-added-code! as added-code))))
     (else
      (handle-added-code! as (list x))))
    (as-lc! as (+ (as-lc as) 1)))) ; FSK: perhaps incrementing by 1 won't work, but what the hell.

(define emit-text-noindent
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (error 'emit-text-noindent "calls to emit-text should be replaced in Sassy")
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

(define emit-text
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (error 'emit-text "calls to emit-text should be replaced in Sassy")
      (emit-string! as code-indentation)
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

(define *did-emit-setrtn-invoke* '())
(define *did-emit-setrtn-branch* '())
(define *did-emit-setrtn-jump* #f)

(define (begin-compiled-scheme-function as label entrypoint? start?)
  (let ((name (compiled-procedure as label)))
    ;(emit-text as "begin_codevector ~a" name)
    (emit-sassy as 'align code_align)
    (set! *did-emit-setrtn-invoke* '())
    (set! *did-emit-setrtn-branch* '())
    (set! *did-emit-setrtn-jump* #f)
    (set! code-indentation (string #\tab))
    (set! code-name name)))

(define (end-compiled-scheme-function as)
  (set! code-indentation "")
  (for-each (lambda (n) (emit-setrtn-invoke-patch-code as n))
            *did-emit-setrtn-invoke*)
  (for-each (lambda (n) (emit-setrtn-branch-patch-code as n))
            *did-emit-setrtn-branch*)
  (if *did-emit-setrtn-jump*
      (emit-setrtn-jump-patch-code as))
  ;(emit-text as "end_codevector ~a" code-name)
  ;(emit-text as "")
  )

(define code-indentation "")
(define code-name "")

; Pseudo-instructions.

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    (emit-sassy as ia86.T_ALIGN
                (operand1 instruction))
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
    (let ((u (as-user as)))
      (user-data.labels! u (cons (operand1 instruction) (user-data.labels u))))
    (make-asm-label as (operand1 instruction))
    (emit-sassy as ia86.T_LABEL
                (compiled-procedure as (operand1 instruction)))))

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
    (emit-sassy as ia86.T_OP1
	       (operand1 instruction))))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-sassy as ia86.T_OP2
                (operand1 instruction)
                (operand2 instruction))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (emit-sassy as ia86.T_OP2IMM
                (operand1 instruction)
                (constant-value (operand2 instruction)))))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-sassy as ia86.T_OP3
                (operand1 instruction)
                (operand2 instruction)
                (operand3 instruction))))


(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (if (immediate-constant? (operand1 instruction))
	(emit-sassy as ia86.T_CONST_IMM
                    (constant-value (operand1 instruction)))
	(emit-sassy as ia86.T_CONST_CONSTVECTOR
                    (emit-datum as (operand1 instruction))))))


(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-sassy as ia86.T_GLOBAL
                (emit-global as (operand1 instruction)))))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-sassy as ia86.T_SETGLBL
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
	 (set-constant! as code-offset (car segment))
	 (set-constant! as const-offset (cdr segment)))
       (as-user as))
      (list-lambda-end)
      (set! code-offset (emit-codevector as 0))
      (set! const-offset (emit-constantvector as 0))
      (emit-sassy as ia86.T_LAMBDA
                 code-offset 
		 const-offset
		 (operand2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (emit-sassy as ia86.T_LEXES (operand1 instruction))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (emit-sassy as ia86.T_ARGSEQ (operand1 instruction))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (emit-sassy as ia86.T_ARGSGE (operand1 instruction))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-sassy as ia86.T_INVOKE (operand1 instruction))))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "restore" instruction)
	  (emit-sassy as ia86.T_RESTORE
		     (min (operand1 instruction) (- *nregs* 1)))))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "pop" instruction)
	  (emit-sassy as ia86.T_POP (operand1 instruction))))))

(define-instruction $popstk
  (lambda (instruction as)
    (error "POPSTK is not yet implemented by the x86-NASM assembler.")))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-sassy as ia86.T_STACK (operand1 instruction))))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-sassy as ia86.T_SETSTK (operand1 instruction))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-sassy as ia86.T_LOAD
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-sassy as ia86.T_STORE
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (emit-sassy as ia86.T_LEXICAL
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-sassy as ia86.T_SETLEX
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-sassy as ia86.T_REG (operand1 instruction))))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-sassy as ia86.T_SETREG (operand1 instruction))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-sassy as ia86.T_MOVEREG
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-sassy as ia86.T_RETURN)))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)
    (emit-sassy as ia86.T_NOP)))

; (define-instruction $save
;   (lambda (instruction as)
;     (if (not (negative? (operand1 instruction)))
;         (begin
;          (list-instruction "save" instruction)
; 	 (emit-sassy as 'T_SAVE (operand1 instruction))))))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
	  (list-instruction "save" instruction)
	  (let* ((n (operand1 instruction))
		 (v (make-vector (+ n 1) #t)))
	    (emit-sassy as ia86.T_SAVE0 n)
            ;; FIXME: shouldn't this xform be in peepopt.sch?
	    (if (peephole-optimization)
		(let loop ((instruction (next-instruction as)))
		  (if (eqv? $store (operand0 instruction))
		      (begin (list-instruction "store" instruction)
			     (emit-sassy as ia86.T_STORE 
					(operand1 instruction)
					(operand2 instruction))
			     (consume-next-instruction! as)
			     (vector-set! v (operand2 instruction) #f)
			     (loop (next-instruction as))))))
	    (do ((i 0 (+ i 1)))
		((= i (vector-length v)))
	      (if (vector-ref v i)
		  (emit-sassy as ia86.T_SAVE1 i))))))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-sassy as ia86.T_SETRTN 
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-sassy as ia86.T_APPLY
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-sassy as ia86.T_JUMP
               (operand1 instruction)
               (operand2 instruction)
	       (compiled-procedure as (operand2 instruction)))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-sassy as
                ia86.T_SKIP
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-sassy as
	       (if (assq (operand1 instruction) 
			 (as-labels as))
		   ia86.T_BRANCH
		   ia86.T_SKIP)
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-sassy as 
	       (if (assq (operand1 instruction)
			 (as-labels as))
		   ia86.T_BRANCHF 
		   ia86.T_SKIPF)
	       (compiled-procedure as (operand1 instruction)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction "check" instruction)
    (emit-sassy as ia86.T_CHECK
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (compiled-procedure as (operand4 instruction)))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction "trap" instruction)
    (emit-sassy as ia86.T_TRAP
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (operand4 instruction))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (if (immediate-constant? (operand1 instruction))
	(emit-sassy as ia86.T_CONST_SETREG_IMM 
		   (constant-value (operand1 instruction))
		   (operand2 instruction))
	(emit-sassy as ia86.T_CONST_SETREG_CONSTVECTOR
		   (emit-datum as (operand1 instruction))
                   (operand2 instruction)))))

(define-instruction $global/invoke
  (lambda (instruction as)
    (list-instruction "global/invoke" instruction)
    (emit-sassy as ia86.T_GLOBAL_INVOKE
                (emit-global as (operand1 instruction))
                (operand2 instruction))))

(define-instruction $global/setreg
  (lambda (instruction as)
    (list-instruction "global/setreg" instruction)
    (emit-sassy as ia86.T_GLOBAL_SETREG
                (emit-global as (operand1 instruction))
                (operand2 instruction))))

(define-instruction $setrtn/invoke
  (lambda (instruction as)
    (list-instruction "setrtn/invoke" instruction)
    (emit-sassy as ia86.T_SETRTN_INVOKE (operand1 instruction))))

(define-instruction $setrtn/jump
  (lambda (instruction as)
    (list-instruction "setrtn/jump" instruction)
    (emit-sassy as ia86.T_SETRTN_JUMP 
                (operand1 instruction) 
                (operand2 instruction))))

(define-instruction $setrtn/branch
  (lambda (instruction as)
    (list-instruction "setrtn/branch" instruction)
    (emit-sassy as 	      
                (if (assq (operand1 instruction) 
                          (as-labels as))
                    ia86.T_SETRTN_BRANCH
                    ia86.T_SETRTN_SKIP)
                (compiled-procedure as (operand1 instruction)))))

(define-instruction $reg/setglbl
  (lambda (instruction as)
    (list-instruction "reg/setglbl" instruction)
    (emit-sassy as ia86.T_REG_SETGLBL 
                (operand1 instruction) 
                (emit-global as (operand2 instruction)))))

(define-instruction $reg/op1/branchf
  (lambda (instruction as)
    (list-instruction "reg/op1/branchf" instruction)
    (emit-sassy as ia86.T_REG_OP1_BRANCHF
                (operand1 instruction)
                (operand2 instruction)
                (compiled-procedure as (operand3 instruction)))))

(define-instruction $reg/op1/setreg
  (lambda (instruction as)
    (list-instruction "reg/op1/setreg" instruction)
    (emit-sassy as ia86.T_OP1* 
                (operand1 instruction)
                (operand2 instruction)
                (operand3 instruction))))

(define-instruction $reg/op2/setreg
  (lambda (instruction as)
    (list-instruction "reg/op2/setreg" instruction)
    (emit-sassy as ia86.T_OP2* 
                (operand1 instruction)
                (operand2 instruction)
                (operand3 instruction)
                (operand4 instruction))))

; Helper procedures.

(define symbolic-label-cache #f)
(define (reset-symbolic-label-cache!)
  (set! symbolic-label-cache 
        (make-vector 2048 #f)))
(define (grow-symbolic-label-cache label)
  (let ((target-length (do ((n (* 2 (vector-length symbolic-label-cache))
                               (* 2 n)))
                           ((> n label) n))))
    (let ((new-cache (make-vector target-length #f)))
      (do ((i 0 (+ i 1)))
          ((>= i (vector-length symbolic-label-cache)))
        (vector-set! new-cache i
                     (vector-ref symbolic-label-cache i)))
      (set! symbolic-label-cache new-cache))))
(define (symbolic-label-cache-get label fail-thunk)
  (cond ((>= label (vector-length symbolic-label-cache))
         (grow-symbolic-label-cache label)))
  (cond ((vector-ref symbolic-label-cache label))
        (else
         (let ((val (fail-thunk)))
           (vector-set! symbolic-label-cache label val)
           val))))

(define (compiled-procedure as label)
  (symbolic-label-cache-get 
   label
   (lambda ()
     '(begin (display `(compiled-procedure as ,label))
             (newline))
     (twobit-format #f "compiled_start_~a_~a" 
                    (user-data.toplevel-counter (as-user as))
                    label))))

(define (immediate-constant? x)
  (or (fixnum? x)
      (null? x)
      (boolean? x)
      (char? x)
      (eof-object? x)
      (equal? x (unspecified))
      (equal? x (undefined))))

(define (constant-value x)
  (define (char n)
    (fxlogior (fxlsh (char->integer n) char_shift) $imm.character))

  (define (exact-int->fixnum x)
    (* x 4))

  (define (char->immediate c)
    (+ (* (char->integer c) 65536) $imm.character))

  (cond ((fixnum? x)              (fixnum x))
        ((eq? x #t)               $imm.true)
        ((eq? x #f)               $imm.false)
        ((equal? x (eof-object))  $imm.eof)
        ((equal? x (unspecified)) $imm.unspecified)
        ((equal? x (undefined))   $imm.undefined)
        ((null? x)                $imm.null)
        ((char? x)                (char x))
	(else ???)))

(define (new-proc-id as)
  (let* ((u (as-user as))
	 (x (user-data.proc-counter u)))
    (user-data.proc-counter! u (+ 1 x))
    x))

; eof
