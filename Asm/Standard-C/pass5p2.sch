; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Standard-C machine assembler
;
; Code generation strategy:
;   Each MAL basic block is compiled to a separate C procedure, with
;   the name compiled_block_x_y where x is a procedure label (unique
;   in a file) and y is the label at the block's entry point.  
;
;   There are implicit basic block breaks in some primitives, and code 
;   for these primitives is emitted with an extra macro argument that is
;   the name of that continuation point, e.g., 
;      twobit_op2imm_130( 3, compiled_temp_7_1234 )
;   Depending on a #define value during C compilation, the macro is
;   expanded either to ignore the extra entry point or to end the current
;   procedure and start a new procedure with the given name.
;
;   Procedure entry points do not have labels, and the entry points
;   are named compiled_start_x_y, where y is some unique identifier.
;
;   A list of procedure names is maintained (with a flag for whether they
;   are conditional or not); they can be used to create forward declarations
;   when assembly is done.
;
; Overrides the procedures of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $standard-C-assembly-table$)

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


; User-data structure has two fields:
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for labels

(define (make-user-data) (list 0 0))

(define (user-data.toplevel-counter u) (car u))
(define (user-data.proc-counter u) (cadr u))

(define (user-data.toplevel-counter! u x) (set-car! u x))
(define (user-data.proc-counter! u x) (set-car! (cdr u) x))


; Assembly listing.

(define listify? #f)

(define $standard-C-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $standard-C-assembly-table$ i proc)
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

(define emit-text
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (emit-string! as code-indentation)
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

(define (begin-compiled-scheme-function as label entrypoint? start?)
  (let ((name (compiled-procedure as label start?)))
    (emit-text as "static RTYPE ~a( CONT_PARAMS ) {~%  twobit_prologue(); " 
	       name)
    (add-function as name #t entrypoint?)
    (set! code-indentation "  ")))

(define (add-compiled-scheme-function as label entrypoint? start?)
  (let ((name (compiled-procedure as label start?)))
    (if (not (assoc name (lookup-functions as)))
        (add-function as name #t entrypoint?))))

(define (end-compiled-scheme-function as)
  (emit-text as "twobit_epilogue();")
  (set! code-indentation "")
  (emit-text as "}")
  (emit-text as ""))

(define code-indentation "")

(define (lookup-functions as)
  (or (assembler-value as 'functions) '()))

(define (add-function as name definite? entrypoint?)
  (assembler-value! as 'functions (cons (list name definite? entrypoint?)
					(lookup-functions as)))
  name)
    
; Pseudo-instructions.

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)))

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
    (emit-text as "twobit_label( ~a, ~a );"
                  (operand1 instruction)
                  (compiled-procedure as (operand1 instruction) #f))))

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
    (if (op1-implicit-continuation? (operand1 instruction))
        (call-with-values
          (lambda () (implicit-procedure as))
          (lambda (numeric symbolic)
            (add-function as symbolic #f #f)
            (emit-text as "twobit_op1_~a( ~a, ~a ); /* ~a */"
                          (op1-primcode (operand1 instruction))
                          numeric
                          symbolic
                          (operand1 instruction))))
	(emit-text as "twobit_op1_~a(); /* ~a */"
		   (op1-primcode (operand1 instruction))
		   (operand1 instruction)))))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (if (op2-implicit-continuation? (operand1 instruction))
        (call-with-values
          (lambda () (implicit-procedure as))
          (lambda (numeric symbolic)
            (add-function as symbolic #f #f)
            (emit-text as "twobit_op2_~a( ~a, ~a, ~a ); /* ~a */"
                          (op2-primcode (operand1 instruction))
                          (operand2 instruction)
                          numeric
                          symbolic
                          (operand1 instruction))))
	(emit-text as "twobit_op2_~a( ~a ); /* ~a */"
		   (op2-primcode (operand1 instruction))
		   (operand2 instruction)
		   (operand1 instruction)))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (if (op2imm-implicit-continuation? (operand1 instruction))
        (call-with-values
          (lambda () (implicit-procedure as))
          (lambda (numeric symbolic)
            (add-function as symbolic #f #f)
            (emit-text as "twobit_op2imm_~a( ~a, ~a, ~a ); /* ~a */"
                          (op2imm-primcode (operand1 instruction))
                          (constant-value (operand2 instruction))
                          numeric
                          symbolic
                          (operand1 instruction))))
	(emit-text as "twobit_op2imm_~a( ~a ); /* ~a */"
		   (op2imm-primcode (operand1 instruction))
		   (constant-value (operand2 instruction))
		   (operand1 instruction)))))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (if (op3-implicit-continuation? (operand1 instruction))
        (call-with-values
          (lambda () (implicit-procedure as))
          (lambda (numeric symbolic)
            (add-function as symbolic #f #f)
            (emit-text as "twobit_op3_~a( ~a, ~a, ~a, ~a ); /* ~a */"
                          (op3-primcode (operand1 instruction))
                          (operand2 instruction)
                          (operand3 instruction)
                          numeric
                          symbolic
                          (operand1 instruction))))
	(emit-text as "twobit_op3_~a( ~a, ~a ); /* ~a */"
		   (op3-primcode (operand1 instruction))
		   (operand2 instruction)
		   (operand3 instruction)
		   (operand1 instruction)))))

(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (if (immediate-constant? (operand1 instruction))
	(emit-text as "twobit_imm_const( ~a ); /* ~a */"
		   (constant-value (operand1 instruction))
                   (operand1 instruction))
	(emit-text as "twobit_const( ~a );"
		   (emit-datum as (operand1 instruction))))))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-text as "twobit_global( ~a ); /* ~a */"
	       (emit-global as (operand1 instruction))
	       (operand1 instruction))))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-text as "twobit_setglbl( ~a ); /* ~a */"
	       (emit-global as (operand1 instruction))
	       (operand1 instruction))))

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
      (emit-text as "twobit_lambda( ~a, ~a, ~a );"
		 (compiled-procedure as entry #t)
		 const-offset
		 (operand2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (emit-text as "twobit_lexes( ~a );" 
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (if (not (unsafe-code))
	(emit-text as "twobit_argseq( ~a );" (operand1 instruction)))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (if (not (unsafe-code))
	(emit-text as "twobit_argsge( ~a );" (operand1 instruction)))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-text as "twobit_invoke( ~a );" (operand1 instruction))))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "restore" instruction)
	  (emit-text as "twobit_restore( ~a );"
		     (min (operand1 instruction) (- *nregs* 1)))))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
	(begin
	  (list-instruction "pop" instruction)
	  (emit-text as "twobit_pop( ~a );" (operand1 instruction))))))

(define-instruction $popstk
  (lambda (instruction as)
    (error "POPSTK is not yet implemented by the standard-C assembler.")))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-text as "twobit_stack( ~a );" (operand1 instruction))))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-text as "twobit_setstk( ~a );" (operand1 instruction))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-text as "twobit_load( ~a, ~a );"
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-text as "twobit_store( ~a, ~a );"
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (emit-text as "twobit_lexical( ~a, ~a );"
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-text as "twobit_setlex( ~a, ~a );"
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-text as "twobit_reg( ~a );" (operand1 instruction))))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-text as "twobit_setreg( ~a );" (operand1 instruction))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-text as "twobit_movereg( ~a, ~a );"
	       (operand1 instruction) (operand2 instruction))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-text as "twobit_return();")))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)
    (emit-text as "twobit_nop();")))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "save" instruction)
	 (emit-text as "twobit_save( ~a );" (operand1 instruction))))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-text as "twobit_setrtn( ~a, ~a );"
                  (operand1 instruction)
	       (compiled-procedure as (operand1 instruction) #f))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-text as "twobit_apply( ~a, ~a );"
	       (operand1 instruction)
	       (operand2 instruction))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-text as "twobit_jump( ~a, ~a, ~a );"
               (operand1 instruction)
               (operand2 instruction)
	       (compiled-procedure as (operand2 instruction) #f))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-text as "twobit_skip( ~a, ~a );"
               (operand1 instruction)
	       (compiled-procedure as (operand1 instruction) #f))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-text as "twobit_branch( ~a, ~a );"
               (operand1 instruction)
	       (compiled-procedure as (operand1 instruction) #f))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-text as "twobit_branchf( ~a, ~a );"
               (operand1 instruction)
	       (compiled-procedure as (operand1 instruction) #f))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction "check" instruction)
    (emit-text as "twobit_check( ~a, ~a, ~a, ~a, ~a );"
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (operand4 instruction)
               (compiled-procedure as (operand4 instruction) #f))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction "trap" instruction)
    (emit-text as "twobit_trap( ~a, ~a, ~a, ~a );"
               (operand1 instruction)
               (operand2 instruction)
               (operand3 instruction)
               (operand4 instruction))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (if (immediate-constant? (operand1 instruction))
	(emit-text as "twobit_imm_const_setreg( ~a, ~a ); /* ~a */"
		   (constant-value (operand1 instruction))
		   (operand2 instruction)
                   (operand1 instruction))
	(emit-text as "twobit_const_setreg( ~a, ~a );"
		   (emit-datum as (operand1 instruction))
                   (operand2 instruction)))))

; Helper procedures.

(define (compiled-procedure as label start?)
  (if start?
      (twobit-format #f "compiled_start_~a_~a" 
		     (user-data.toplevel-counter (as-user as))
		     label)
      (twobit-format #f "compiled_block_~a_~a" 
		     (user-data.toplevel-counter (as-user as))
		     label)))

(define (implicit-procedure as)
  (let ((id (new-proc-id as)))
    (values
     id
     (twobit-format #f "compiled_temp_~a_~a" 
                    (user-data.toplevel-counter (as-user as))
                    id))))

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

  (cond ((fixnum? x)              (exact-int->fixnum x))
        ((eq? x #t)               "TRUE_CONST")
        ((eq? x #f)               "FALSE_CONST")
	((equal? x (eof-object))  "EOF_CONST")
	((equal? x (unspecified)) "UNSPECIFIED_CONST")
	((equal? x (undefined))   "UNDEFINED_CONST")
        ((null? x)                "NIL_CONST")
        ((char? x)                (char->immediate x))
	(else ???)))

(define (new-proc-id as)
  (let* ((u (as-user as))
	 (x (user-data.proc-counter u)))
    (user-data.proc-counter! u (+ 1 x))
    x))


; eof
