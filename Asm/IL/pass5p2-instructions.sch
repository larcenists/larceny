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
(define instr-methodname-alist '())
(define (define-instr-methodname instr methodname)
  (set! instr-methodname-alist 
        (cons (cons instr methodname) instr-methodname-alist)))

(define (il:instr-method-call instr)
  (let ((methodentry (assq instr instr-methodname-alist)))
    (or (cdr methodentry)
        (asm-error "Mnemonic has no IL method: " instr))))

(define-syntax instr-method/full
  (syntax-rules ()
   ((_ opts class name (argtype ...))
     (define-instr-methodname 'name
                              (il:call 'opts
                                       iltype-void 
                                       class 
                                       (symbol->string 'name)
                                       (list argtype ...))))))

(define-syntax instr-method
  (syntax-rules ()
    ((_ name (argtype ...))
     (instr-method/full () il-instructions name (argtype ...)))))
(define-syntax instr-method/ret
  (syntax-rules ()
    ((_ name (argtype ...))
     (instr-method/full (tail) il-instructions name (argtype ...)))))

;; Default implementations: These are used if inlining is not
;; specified for the instruction (see config.sch). Some instructions
;; have no runtime implementation (like branchf).

(instr-method nop ())
(instr-method global (iltype-int32 iltype-string))
(instr-method setglbl (iltype-int32))
(instr-method constant (iltype-int32))
(instr-method imm_constant (iltype-schemeobject))
(instr-method reg (iltype-int32))
(instr-method setreg (iltype-int32))
(instr-method movereg (iltype-int32 iltype-int32))

(instr-method/ret invoke (iltype-int32))
(instr-method/ret apply (iltype-int32 iltype-int32))

(instr-method lambda (iltype-codevector iltype-int32 iltype-int32))
(instr-method lexes (iltype-int32))
(instr-method lexical (iltype-int32 iltype-int32))
(instr-method setlex (iltype-int32 iltype-int32))
(instr-method argseq (iltype-int32))
(instr-method argsge (iltype-int32))

(instr-method pop (iltype-int32))
(instr-method save (iltype-int32))
(instr-method/ret rtn ())
(instr-method setrtn (iltype-codevector iltype-int32))
(instr-method stack (iltype-int32))
(instr-method setstk (iltype-int32))
(instr-method load (iltype-int32 iltype-int32))
(instr-method store (iltype-int32 iltype-int32))

;(instr-method restore (iltype-int32))
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
      (list-entry/line ".entry" instruction as)
      (if local-variables-closed
          (emit as (il:comment/info "local-variables-closed" 
                                    local-variables-closed))))))

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
    (emit as
          (il 'ldc.i4 (emit-datum as (operand1 instruction)))
          (il:instr-method-call 'constant))))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction/line "global" instruction as)
    (let ((defined-label (allocate-label as))
          (global-index (emit-global as (operand1 instruction))))
      (emit as
            (il:load-global-cell global-index)
            (il 'ldfld (il-field iltype-schemeobject il-schemepair "first"))
            (il 'dup)
            (il:load-constant (undefined))
            (il:branch-s 'bne.un defined-label)
            (il 'pop)
            (il:set-register 'result (il:load-global-cell global-index))
            (il:fault-abort/message 
             $ex.undef-global
             (string-append "reference to unbound global: " 
                            (symbol->string (operand1 instruction))))
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
          (il:load-register 'result)
          (il 'stfld (il-field iltype-schemeobject il-schemepair "first"))))
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
           (set-constantvector-slot! as 'codevector 
                                     code-offset (car segment))
           (set-constantvector-slot! as 'constantvector 
                                     const-offset (cdr segment)))
         (as-user as))
        (set! code-offset (emit-codevector as (nested-id)))
        (set! const-offset (emit-constantvector as (nested-id)))
        
        (emit as
              (il:comment/info "lambda-local-variables" 
                               (operand2 instruction))
              (il:load-codevector
               (codevector-name (codevector-id as entry))
               (as-il-namespace as))
              (il 'ldc.i4 const-offset)
              (il 'ldc.i4 (operand2 instruction))
              (il:instr-method-call 'lambda)
              (il:recache-result))))))

(define-instruction $lexes
  (instr-runtime-method 'lexes 1))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction/line "args=" instruction as)
    (let ((okay-label (allocate-label as)))
      (emit as 
            (il:load-register 'result)
            ; unchecked: we trust call convention
            (il 'castclass iltype-fixnum)
            (il:ldfld iltype-int32 il-fixnum "value")
            (il 'ldc.i4 (operand1 instruction))
            (il:branch-s 'beq okay-label)
            (il:fault-abort/message 
             $ex.argc 
             (string-append "args= " (number->string (operand1 instruction))))
            (il:label okay-label))))
  (instr-runtime-method 'argseq 1))

;; FIXME: Incomplete -- not used
(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction/line "args>=" instruction as)
    (let ((okay-label (allocate-label as)))
      (emit as 
            (il:load-register 'result)
            (il 'castclass iltype-fixnum)
              ; unchecked: we trust call convention
            (il 'ldfld (il-field iltype-int32 il-fixnum "value"))
            (il 'ldc.i4 (operand1 instruction))
            (il:branch-s 'bge okay-label)
            (il:fault-abort $ex.argc)
            
            (il:label okay-label))))
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
              (il:fault-abort $ex.nonproc))
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
             (il 'ldfld 
                 (il-field iltype-codevector 
                           il-procedure "entrypoint"))
             (il 'ldc.i4 FIRST-JUMP-INDEX)
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (instr-runtime-method/ret 'invoke 1))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction/line "apply" instruction as)
    (with-il-locals as (list iltype-procedure)
     (lambda (procedure-local)
       (emit as
             (il:load-register 'result)
             (il:check-type 
              iltype-procedure
              (il:fault-abort $ex.nonproc))
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
             (il:make-fixnum)
             (il:set-register/pop 'result)
             
             (il:use-fuel/call)
             (il 'ldloc procedure-local)
             (il 'ldfld (il-field iltype-codevector
                                  il-procedure
                                  "entrypoint"))
             (il 'ldc.i4 FIRST-JUMP-INDEX)
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (instr-runtime-method/ret 'apply 2))

;; Stack
(define-instruction $save
;  (lambda (instruction as)
;    (list-instruction/line "save" instruction as)
;    (let ((after-label (allocate-label as))
;          (done-label (allocate-label as)))
;      (emit as
;            (il:load-current-frame)
;            (il:ldfld iltype-cache-frame il-cache-frame "after")
;            (il 'dup)
;            (il:branch-s 'brtrue above-label)
;            (il 'pop)
;            (il 'ldc.i4 (operand1 instruction))
;            (il:call iltype-void il-cont "save" (list iltype-int32))
;            (il:branch-s 'br done-label)
;            
;            (il:label above-label)
;            (il 'dup)
;            (il:stsfld iltype-cache-frame il-cont "cont")
;            
;            (il 'ldc.i4 (operand1 instruction))
;            (il:call '(instance) iltype-void il-continuation-frame "prepare" 
;                     (list iltype-int32))
;            (il:set-current-frame-slot 
;             0
;             (il:load-register ENV-REGISTER))
;            (il:label done-label))))
  (instr-runtime-method 'save 1))

(define-instruction $restore
  (instr-runtime-method 'restore 1))

(define-instruction $pop
;  (lambda (instruction as)
;    (list-instruction/line "pop" instruction as)
;    (let ((set-below-label (allocate-label as))
;          (done-label (allocate-label as)))
;      (emit as
;            (il:load-current-frame)
;            (il 'ldfld (il-field iltype-cache-frame il-cache-frame "below"))
;            (il 'dup)
;            (il:branch-s 'brtrue set-below-label)
;            (il 'pop)
;            (il:call '() iltype-void il-cont "fillCache" '())
;            (il:branch-s 'br done-label)
;            
;            (il:label set-below-label)
;            (il:stsfld iltype-cache-frame il-cont "cont")
;            (il:label done-label))))
  (instr-runtime-method 'pop 1))

(define-instruction $popstk
  (lambda (instruction as)
    (error "POPSTK is not implemented by this assembler.")))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction/line "stack" instruction as)
    (emit as
          (il:set-register 'result
                           (il:load-current-frame-slot 
                            (operand1 instruction)))))
  (instr-runtime-method 'stack 1))

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
           (il:load-current-frame-slot (operand2 instruction)))))
  (instr-runtime-method 'load 2))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction/line "store" instruction as)
    (emit as
          (il:set-current-frame-slot 
           (operand2 instruction) 
           (il:load-register (operand1 instruction)))))
  (instr-runtime-method 'store 2))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction/line "return" instruction as)
    (with-il-locals as (list iltype-continuation-frame)
     (lambda (contframe)
       (emit as
             (il:load-current-frame)
             (il 'stloc contframe)
             (il 'ldloc contframe)
             (il:load-frame-slot 0)
             (il 'dup)
             (il:set-register/pop ENV-REGISTER)
             (il 'castclass iltype-procedure)
             (il:ldfld iltype-codevector il-procedure "entrypoint")
             (il 'ldloc contframe)
             (il:ldfld iltype-int32 il-continuation-frame "returnIndex")
             (il:call-scheme))))
    (assembler-value! as 'basic-block-closed #t))
  (instr-runtime-method/ret 'rtn 0))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction/line "setrtn" instruction as)
    (emit as
          (il:load-current-frame)
          (il 'ldc.i4 (intern-label as (operand1 instruction)))
          (il:stfld iltype-int32 il-continuation-frame "returnIndex")))
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
                            (il:load-rib (operand1 instruction))
                            (il 'ldc.i4 (operand2 instruction))
                            (il 'ldelem.ref)))))
  (instr-runtime-method 'lexical 2))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction/line "setlex" instruction as)
    (emit as
          (il:load-rib (operand1 instruction))
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
  (instr-runtime-method 'reg 1))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction/line "setreg" instruction as)
    (emit as
          (il:set-register
           (operand1 instruction)
           (il:load-register 'result))))
  (instr-runtime-method 'setreg 1))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction/line "movereg" instruction as)
    (emit as
          (il:set-register 
           (operand2 instruction)
           (il:load-register (operand1 instruction)))))
  (instr-runtime-method 'movereg 2))

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
            (il:load-static-link (operand1 instruction))))
          
          (il:load-register ENV-REGISTER)
          (il 'castclass iltype-procedure)
          (il 'ldfld 
              (il-field iltype-codevector il-procedure 
                        "entrypoint"))
          ;; Load the jump index (delayed, will be forced
          ;; in patch-up, when all info is available)
          (il:delay (il 'ldc.i4 (as:label->index as (operand2 instruction))))
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
            (il:load-constant #f)
            (il:load-register 'result)
            (il:branch-s 'bne.un no-branch-label)
            (il:br/use-fuel (operand1 instruction))
            (il:label no-branch-label)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction/line "check" instruction as)
    (emit as
          (il:load-constant #f)
          (il:load-register 'result)
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
