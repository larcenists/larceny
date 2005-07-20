;; -----------------
;; Instructions
;; -----------------

;; Many instructions are implemented by the runtime. instr-runtime-method 
;; creates IL that calls these implementations in the case where the runtime
;; method takes int arguments directly from the instruction.
;; Aided by the instruction to methodname mapping below.

(define operand-selector-list
  (list operand1 operand2 operand3 operand4))

;; instr-runtime-method/full : symbol number boolean ->
;;         (instruction assembler -> void)
(define (instr-runtime-method/full instr argc ret?)
  (lambda (instruction as)
    (list-instruction/line (symbol->string instr) instruction as)
    (let loop ((argc argc) (selectors operand-selector-list))
      (cond ((zero? argc)
             #f)
            ((null? selectors)
             (error "more than 4 arguments to runtime method call"))
            (else (emit as (il 'ldc.i4 ((car selectors) instruction)))
                  (loop (- argc 1) (cdr selectors)))))
    (emit as 
          (il:flush-result-cache)
          (il:instr-method-call instr)
          (il:recache-result))
    (if ret? (assembler-value! as 'basic-block-closed #t))))

(define (instr-runtime-method instr argc)
  (instr-runtime-method/full instr argc #f))
(define (instr-runtime-method/ret instr argc)
  (instr-runtime-method/full instr argc #t))

; Mnemonic to IL methodname mapping
(define instr-methodname-table (make-hash-table))
(define (define-instr-methodname instr methodname)
  (hash-table-put! instr-methodname-table instr methodname))

(define (il:instr-method-call instr)
  (hash-table-get
   instr-methodname-table instr
   (lambda () (asm-error "Mnemonic has no IL method: " instr))))

(define-syntax instr-method/full
  (syntax-rules ()
   ((_ opts return-type class name (argtype ...))
     (define-instr-methodname 'name
       (il:call 'opts
                return-type
                class
                (symbol->string 'name)
                (list argtype ...))))))

(define-syntax instr-method
  (syntax-rules ()
    ((_ name (argtype ...))
     (instr-method/full () iltype-void il-instructions name (argtype ...)))))
(define-syntax instr-method/ret
  (syntax-rules ()
    ((_ name (argtype ...))
     (instr-method/full (tail) iltype-code-address il-instructions name (argtype ...)))))

;; Default implementations: These are used if inlining is not
;; specified for the instruction (see config.sch). Some instructions
;; have no runtime implementation (like branchf).


(instr-method argseq (iltype-int32))
(instr-method argsge (iltype-int32))
(instr-method constant (iltype-int32))
(instr-method global (iltype-int32 iltype-string))
(instr-method imm_constant ())
(instr-method lambda (iltype-codevector iltype-int32 iltype-int32))
(instr-method lexes (iltype-int32))  ;; not used
(instr-method lexical (iltype-int32 iltype-int32))
(instr-method load (iltype-int32 iltype-int32))
(instr-method movereg (iltype-int32 iltype-int32))
(instr-method nop ())  ;; not used, I hope
(instr-method pop (iltype-int32))
(instr-method reg (iltype-int32))
(instr-method restore (iltype-int32)) ;; not used
(instr-method save (iltype-int32))
(instr-method setglbl (iltype-int32))
(instr-method setlex (iltype-int32 iltype-int32))
(instr-method setreg (iltype-int32))
(instr-method setrtn (iltype-codevector iltype-int32))
(instr-method setstk (iltype-int32))
(instr-method stack (iltype-int32))
(instr-method store (iltype-int32 iltype-int32))

(instr-method/ret apply (iltype-int32 iltype-int32))
(instr-method/ret invoke (iltype-int32))
(instr-method/ret rtn ())
(instr-method/ret trap (iltype-int32 iltype-int32 iltype-int32 iltype-int32))

;; -----------------
;; Instructions
;; -----------------

;;(define-instruction $mnemonic
;;  (lambda (instruction as)
;;    [emit inlined implementation in IL (optional)])
;;  (lambda (instruction as)
;;    emit IL))

;; PSEUDO INSTRUCTIONS

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction/line ".align" instruction as)))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction/line ".cont" instruction as)))

(define-instruction $.end
  (lambda (instruction as)
    (list-instruction/line ".end" instruction as)
    (emit as (il:comment "End"))
    (end-codevector-class as)))

(define-instruction $.entry
  (lambda (instruction as)
    ;; Entry point... marks beginning of codevector
    (let ((local-variables-closed 
           (if (pair? (cdddr instruction)) (operand3 instruction) #f)))
      (begin-codevector-class as 
                              (operand1 instruction)
                              (operand2 instruction))
      (list-entry/line ".entry" instruction as))))

(define-instruction $.label
  (lambda (instruction as)
    ;; Marks beginning of basic block
    (list-label/line instruction as)
    (begin-basic-block as (operand1 instruction))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction/line ".proc" instruction as)))

(define-instruction $.proc-doc
  (lambda (instruction as)
    (list-instruction/line ".proc-doc" instruction as)
    (add-documentation as (operand1 instruction))))

(define-instruction $.singlestep
  (lambda (instruction as)
    ;; We don't handle
    #t))

;; INSTRUCTION IMPLEMENTATIONS

;; Operation instructions (op1, op2, op2imm, op3)
;; are defined in pass5p2-ops.sch

;; Globals and Constants (Constant Vector)
(define-instruction $const
  (lambda (instruction as)
    (list-instruction/line "const" instruction as)
    (emit as
          (il:set-register 
           'result
           (cond ((immediate-constant? (operand1 instruction))
                  (il:load-constant (operand1 instruction)))
                 (else
                  (il:load-constant/vector
                   (emit-datum as (operand1 instruction))))))))
  (lambda (instruction as)
    (list-instruction/line "const" instruction as)
    (if (immediate-constant? (operand1 instruction))
        (emit as (il:set-register 'result (il:load-constant (operand1 instruction))))
        (let ((index (emit-datum as (operand1 instruction))))

          (define (default)
            (emit as
                  (il 'ldc.i4 index)
                  (il:instr-method-call 'constant)))

          (if (and (codegen-option 'special-const-instructions)
                   (< index SPECIAL-INSTRUCTION-LIMIT))
              (emit as (il:call '() iltype-void il-instructions
                                (string-append "constant" (number->string index))
                                '()))
              (default))))))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction/line "global" instruction as)
    (let ((defined-label (allocate-label as))
          (global-index (emit-global as (operand1 instruction))))
      (emit as
            (il:load-global-cell global-index)
            ;; (rep:pair-car)
            (il:call '(instance virtual) iltype-schemeobject il-schemeobject "op_cell_ref" '())
            (il 'dup)
            (il:load-constant (undefined))
            (il:branch-s 'bne.un defined-label)
            (il 'pop)
            (il:fault/undef-global global-index)
            (il:label defined-label)
            (il:set-register/pop 'result))))
  (lambda (instruction as)
    (list-instruction/line "global" instruction as)
    (emit as 
          (il 'ldc.i4 (emit-global as (operand1 instruction)))
          (il:ldstr (symbol->string (operand1 instruction)))
          (il:instr-method-call 'global))))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction/line "setglbl" instruction as)
    (emit as
          (il:load-global-cell (emit-global as (operand1 instruction)))
          (il 'castclass iltype-schemepair)
          (il:load-register 'result)
          (rep:set-pair-car!)))
  (lambda (instruction as)
    (list-instruction/line "setglbl" instruction as)
    (emit as 
          (il 'ldc.i4 (emit-global as (operand1 instruction)))
          (il:instr-method-call 'setglbl))))

(define-instruction $lambda
  (let ()
    (define *nested-id-counter* 0)
    (define (nested-id)
      (set! *nested-id-counter* (+ 1 *nested-id-counter*))
      *nested-id-counter*)
    (lambda (instruction as)
      (let* ((const-offset #f)
             (code-offset  #f)
             (entry        (new-proc-id as))
             (lambda-constant-vector #f))
        (list-instruction/line "lambda" instruction as)
        (assemble-nested-lambda
         as
         (cons (list $.entry entry #f (operand2 instruction))
               (operand1 instruction))
         (operand3 instruction)
         (lambda (nested-as segment)
           (assembler-value! 
            as 'functions
            (append (lookup-functions as)
                    (lookup-functions nested-as)))
           (set-constant! as code-offset (car segment))
           (set-constant! as const-offset (cdr segment)))
         (as-user as))
        (set! code-offset (emit-codevector as (nested-id)))
        (set! const-offset (emit-constantvector as (nested-id)))
        
        (emit as
              (il:load-codevector
               (codevector-name (codevector-id as entry))
               (as-il-namespace as))
              (il 'ldc.i4 const-offset)
              (il 'ldc.i4 (operand2 instruction))
              (il:instr-method-call 'lambda)
              (il:recache-result))))))

;; not used
(define-instruction $lexes
  (instr-runtime-method 'lexes 1))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction/line "args=" instruction as)
    (let ((okay-label (allocate-label as))
          (required (operand1 instruction)))
      (cond ((< required NAMED-FIXNUM-LIMIT)
             (emit as
                   (il:load-register 'result)
                   (il:load-constant required)
                   (il:branch-s 'beq okay-label)
                   (il:fault/argc required)
                   (il:label okay-label)))
            (else
             (emit as
                   (il:load-register 'result)
                   ;; unchecked: we trust call convention
                   (il 'castclass iltype-fixnum)
                   (rep:fixnum-value)
                   (il 'ldc.i4 (operand1 instruction))
                   (il:branch-s 'beq okay-label)
                   (il:fault/argc (operand1 instruction))
                   (il:label okay-label))))))
  (instr-runtime-method 'argseq 1))

(define-instruction $args>=
  (instr-runtime-method 'argsge 1))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction/line "invoke" instruction as)
    (with-il-locals as (list iltype-procedure)
     (lambda (procedure-local)
       (emit as
             (il:load-register 'result)
             (il:check-type
              iltype-procedure
              (il:fault/invoke-nonproc (operand1 instruction)))
             (il 'stloc procedure-local)
             ;; Stack is empty
             (il:set-register 'second
                              (il:load-register ENV-REGISTER))
             (il:set-register 'result 
                              (il:load-constant (operand1 instruction)))
             (il:set-register ENV-REGISTER
                              (il 'ldloc procedure-local))
             
             (il:use-fuel/call)
             (il 'ldloc procedure-local)
             (rep:procedure-entrypoint)
             (il 'ldc.i4 FIRST-JUMP-INDEX)
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (lambda (instruction as)
    (define (default)
      ((instr-runtime-method/ret 'invoke 1) instruction as))

    (let ((count (operand1 instruction)))
      (if (and (codegen-option 'special-invoke-instructions)
               (< count SPECIAL-INSTRUCTION-LIMIT))
          (begin
            (emit as
                  (il:call '(tail) iltype-code-address il-instructions
                           (string-append "invoke" (number->string count))
                           '()))
            (assembler-value! as 'basic-block-closed #t))
          (default)))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction/line "apply" instruction as)
    (with-il-locals as (list iltype-procedure)
     (lambda (procedure-local)
       (emit as
             (il:load-register 'result)
             (il:check-type 
              iltype-procedure
              (il:fault/apply-nonproc
               (operand1 instruction)
               (operand2 instruction)))
             (il 'stloc procedure-local)
             
             (il:set-register 'second
                              (il:load-register ENV-REGISTER))
             (il:set-register ENV-REGISTER
                              (il 'ldloc procedure-local))

             ;; Destroys registers 1 - N
             (il 'ldc.i4 (operand1 instruction))
             (il 'ldc.i4 (operand2 instruction))
             (il:call '() iltype-int32 il-call "applySetup" 
                      (list iltype-int32 iltype-int32))
             ;; records N in RESULT
             (rep:make-fixnum)
             (il:set-register/pop 'result)
             
             (il:use-fuel/call)
             (il 'ldloc procedure-local)
             (rep:procedure-entrypoint)
             (il 'ldc.i4 FIRST-JUMP-INDEX)
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (instr-runtime-method/ret 'apply 2))

;; Stack
(define-instruction $save
  (lambda (instruction as)
    (define (default)
      ((instr-runtime-method 'save 1) instruction as))

    (let ((n (operand1 instruction)))
      (if (and (codegen-option 'special-save-instructions)
               (< n SPECIAL-INSTRUCTION-LIMIT))
          (emit as
                (il:call '() iltype-void il-instructions
                         (string-append "save" (number->string n))
                         '()))
          (default)))))

;; Not used
(define-instruction $restore
  (instr-runtime-method 'restore 1))

(define-instruction $pop
  (lambda (instruction as)
    (define (default)
      ((instr-runtime-method 'pop 1) instruction as))

    (let ((n (operand1 instruction)))
      (if (and (codegen-option 'special-pop-instructions)
               (< n SPECIAL-INSTRUCTION-LIMIT))
          (emit as
                (il:call '() iltype-void il-instructions
                         (string-append "pop" (number->string n))
                         '()))
          (default)))))

(define-instruction $popstk
  (lambda (instruction as)
    (error "POPSTK is not implemented by this assembler.")))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction/line "stack" instruction as)
    (emit as
          (il:set-register 'result
                           (rep:load-current-frame-slot 
                            (operand1 instruction)))))
  (lambda (instruction as)
    (define (default)
      ((instr-runtime-method 'stack 1) instruction as))

    (let ((slot (operand1 instruction)))

      (if (and (codegen-option 'special-stack-instructions)
               (< slot CONTINUATION-FRAME-SLOTS))
          (emit as
                (il:call '() iltype-void il-instructions
                         (string-append "stack" (number->string slot))
                         '()))
          (default)))))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction/line "setstk" instruction as)
    (emit as
          (il:set-current-frame-slot (operand1 instruction)
                                     (il:load-register 'result))))
  (instr-runtime-method 'setstk 1))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction/line "load" instruction as)
    (emit as
          (il:set-register 
           (operand1 instruction)
           (rep:load-current-frame-slot (operand2 instruction)))))
  (lambda (instruction as)
    (let ((reg (operand1 instruction))
          (slot (operand2 instruction)))
      (if (< slot CONTINUATION-FRAME-SLOTS)
          (emit as
                (il:call '() iltype-void il-instructions
                         (string-append "load_"
                                        (number->string (operand1 instruction))
                                        "_"
                                        (number->string (operand2 instruction)))
                         '()))
          ((instr-runtime-method 'load 2) instruction as)))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction/line "store" instruction as)
    (emit as
          (rep:set-current-frame-slot 
           (operand2 instruction) 
           (il:load-register (operand1 instruction)))))
  (lambda (instruction as)
    (let ((reg  (operand1 instruction))
          (slot (operand2 instruction)))
      (cond ((and (zero? reg) (zero? slot))
             (list-instruction/line "store" instruction as)
             ;; This is only emitted after a `save' instruction
             ;; and the save instruction does this anyway.
             ;; (emit as (il:instr-method-call 'store_0_0))
             )
            ((< slot CONTINUATION-FRAME-SLOTS)
             (emit as
                   (il:call '() iltype-void il-instructions
                            (string-append "store_"
                                           (number->string reg)
                                           "_"
                                           (number->string slot))
                            '())))
            (else
             ((instr-runtime-method 'store 2) instruction as))))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction/line "return" instruction as)
    (with-il-locals as (list iltype-continuation-frame)
     (lambda (contframe)
       (emit as
             (rep:current-frame)
             (il 'stloc contframe)
             (il 'ldloc contframe)
             (rep:load-frame-slot 0)
             (il 'dup)
             (il:set-register/pop ENV-REGISTER)
             ;(il 'castclass iltype-procedure)
             (rep:procedure-entrypoint)
             (il 'ldloc contframe)
             (rep:frame-return-index)
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (instr-runtime-method/ret 'rtn 0))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction/line "setrtn" instruction as)
    (emit as
          (rep:current-frame)
          (il 'ldc.i4 (intern-label as (operand1 instruction)))
          (rep:set-frame-return-index!)))
  (lambda (instruction as)
    (list-instruction/line "setrtn" instruction as)
    (emit as
          (il 'ldarg THIS-ARG)
          (il 'ldc.i4 (intern-label as (operand1 instruction)))
          (il:instr-method-call 'setrtn))))

;; Lexical
(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction/line "lexical" instruction as)
    (emit as
          (il:set-register 'result
                           (list
                            (rep:load-rib (operand1 instruction))
                            (il 'ldc.i4 (operand2 instruction))
                            (il 'ldelem.ref)))))
  (instr-runtime-method 'lexical 2))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction/line "setlex" instruction as)
    (emit as
          (rep:load-rib (operand1 instruction))
          (il 'ldc.i4 (operand2 instruction))
          (il:load-register 'result)
          (il 'stelem.ref)))
  (instr-runtime-method 'setlex 2))

;; Registers
(define-instruction $reg
  (lambda (instruction as)
    (list-instruction/line "reg" instruction as)
    (emit as
          (il:set-register
           'result
           (il:load-register (operand1 instruction)))))
  (lambda (instruction as)
    (if (codegen-option 'special-reg-instructions)
        (emit as (il:call '() iltype-void il-instructions
                          (string-append "reg" (number->string (operand1 instruction)))
                          '()))
        ((instr-runtime-method 'reg 1) instruction as))))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction/line "setreg" instruction as)
    (emit as
          (il:set-register
           (operand1 instruction)
           (il:load-register 'result))))
  (lambda (instruction as)
    (if (codegen-option 'special-setreg-instructions)
        (emit as (il:call '() iltype-void il-instructions
                          (string-append "setreg" (number->string (operand1 instruction)))
                          '()))
        ((instr-runtime-method 'setreg 1) instruction as))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction/line "movereg" instruction as)
    (emit as
          (il:set-register 
           (operand2 instruction)
           (il:load-register (operand1 instruction)))))
  (lambda (instruction as)
    (emit as
          (il:call '() iltype-void il-instructions
                    (string-append "movereg_"
                                   (number->string (operand1 instruction))
                                   "_"
                                   (number->string (operand2 instruction)))
                   '()))))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction/line "nop" instruction as))
  (instr-runtime-method 'nop 0))

;; Control Flow
(define-instruction $jump
  (lambda (instruction as)
    (list-instruction/line "jump" instruction as)
    (emit as
          ;; Set reg0 to right procedure
          (il:set-register
           ENV-REGISTER
           (list 
            (rep:load-static-link (operand1 instruction))))
          
          (il:load-register ENV-REGISTER)
          ;(il 'castclass iltype-procedure)
          (rep:procedure-entrypoint)
          ;; Load the jump index (delayed, will be forced
          ;; in patch-up, when all info is available)
          (il:delay (list (il 'ldc.i4 (as:label->index as (operand2 instruction)))))
          ;; And call.
          ;; NOTE: no fuel used on jump (FIXME???)
          (il:call-scheme))
    (assembler-value! as 'basic-block-closed #t)))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction/line "skip" instruction as)
    (emit as (il:branch 'br (operand1 instruction)))
    (assembler-value! as 'basic-block-closed #t)))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction/line "branch" instruction as)
    (emit as (il:br/use-fuel (operand1 instruction)))
    (assembler-value! as 'basic-block-closed #t)))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction/line "branchf" instruction as)
    (let ((no-branch-label (allocate-label as)))
      (emit as
            (il:load-register 'result)
            (il:load-constant #f)
            (il:branch-s 'bne.un no-branch-label)
            (il:br/use-fuel (operand1 instruction))
            (il:label no-branch-label)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction/line "check" instruction as)
    (emit as
          (il:load-register 'result)
          (il:load-constant #f)
          (il:branch 'beq (operand4 instruction)))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction/line "trap" instruction as)
    (emit as
          (if (not (zero? (operand1 instruction)))
              (il:set-register 'result 
                               (il:load-register (operand1 instruction)))
              '())
          (if (not (zero? (operand2 instruction)))
              (il:set-register 'second 
                               (il:load-register (operand2 instruction)))
              '())
          (if (not (zero? (operand3 instruction)))
              (il:set-register 'third 
                               (il:load-register (operand3 instruction)))
              '())
          (il:fault-abort/message
           (operand4 instruction)
           "Trap occurred")))
  (instr-runtime-method/ret 'trap 4))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction/line "const/setreg" instruction as)
    (if (immediate-constant? (operand1 instruction))
        (emit as
              (il:set-register
               (operand2 instruction)
               (il:load-constant (operand1 instruction))))
        (emit as
              (il:set-register
               (operand2 instruction)
               (il:load-constant/vector 
                (emit-datum as (operand1 instruction))))))))

;; /Instructions ---
