; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Standard-C machine assembler

;; OVERVIEW

;; ** INPUT **

;; When code from a Scheme file being compiled reaches this point, it 
;; is represented in the following form:

;; A file consists of a sequence of top-level expressions. Each top-level 
;; expression is represented by a list of MacScheme instructions, consisting
;; of an operation and the operands it requires.

;; An instruction is:
;;  - ($constant vector)
;;  - ($global symbol)
;;  - ($setglbl symbol)
;;  - ($lambda (listof instruction) number vector)
;;  - ($.label number)
;;  - a boring-instruction (which has no interesting structure)

;; In the case of $constant, $global, and $setglbl, the operand is a constant.
;; A new slot is allocated in the current constant vector for the operand. The 
;; code emitted for the instruction will refer to the constant by its offset 
;; in the constant vector. (In some cases (eg, immediates, fixnums) the slot 
;; allocation may be avoided.)

;; $lambda indicates the start of a new, nested codevector. It has separate 
;; labels, and it has its own constant vector. The nested codevector (sequence
;; of instructions) is processed, and the resulting codevector representation 
;; and its constant vector are placed in the next two slots of the constant 
;; vector of the instruction sequence corresponding to the $lambda instruction.
;; Thus the code emitted by this phase is not emitted in a linear fashion, but
;; as trees contained within the top-level constant vectors. The task of 
;; flattening this tree is handled in dumpheap-extra.sch.

;; $.label indicates the start of a new basic block. Labels are unique within
;; a code vector, but are not unique in larger scopes. (Aside: is this
;; strictly a basic block? Don't we have jumps out of middle?)

;; The other instructions do not have complex structure, and they have little
;; implications for the structure of generated code.

;; ** OUTPUT **

;; A procedure is represented by an instance of the Procedure class, which 
;; contains a CodeVector, a constant vector (as a SchemeObject), and an 
;; environment rib. The top-level expressions are compiled into a class with 
;; two static members containing the codevector and constant vector. At
;; "load-time", a procedure is created with these two components and invoked.

;; A codevector is represented as a singleton class (private constructor, 
;; single instance in a static field) which contains a single method (an 
;; instance method): void call(int).  The call() method 
;; contains the code emitted for that codevector's instructions. The int 
;; argument is the label of the basic block with which to start execution.
;; The call() method starts with a switch on this label number.

;; A basic block is represented by a case label (eg, "case 1001:") followed
;; by sequence of C# statements. Local jumps (within a codevector) are 
;; expressed as "goto case <label>".


;; TEMPORARY
(define (op1-implicit-continuation? . args) #f)
(define (op2-implicit-continuation? . args) #f)
(define (op3-implicit-continuation? . args) #f)
(define (op2imm-implicit-continuation? . args) #f)

;; -----------------
;; Assembler
;; -----------------

;; Table of instructions
(define (assembly-table) $csharp-assembly-table$)

;; $standard-csharp-assembly-table$ : assembly-table
(define $csharp-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

;; assembly-start : assembler -> ??
;; Initializes the assembler?
(define (assembly-start as)
  (let ((u (as-user as)))
    (user-data.proc-counter! u 0)
    (user-data.toplevel-counter! u (+ 1 (user-data.toplevel-counter u))))
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as)))))

;; assembly-end : assembler ?? -> ??
(define (assembly-end as segment)
  (list (car segment) (cdr segment) (lookup-functions as)))

;; assembly-user-data : -> user-data
(define (assembly-user-data)
  (make-user-data))

; User-data structure has two fields:
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for labels

(define (make-user-data) (list '$$:user-data 0 0))
(define (user-data.toplevel-counter u) (cadr u))
(define (user-data.proc-counter u) (caddr u))
(define (user-data.toplevel-counter! u x) (set-car! (cdr u) x))
(define (user-data.proc-counter! u x) (set-car! (cddr u) x))

;; Assembler value slots:
;;   'codevector-clases : a list of classes used to represent basic blocks
;;   'within-codevector-class : #f if emitting text at top level, name of 
;;           class if currently emitting text into a class declaration
;;   'basic-block-closed : boolean indicating whether control will have 
;;           always been transferred out of the basic block at this point
;;           (eg, by a branch, rtn, etc)

;; /Assembler ------

;; -----------------
;; Code Emission
;; -----------------

;; emit-text : assembler format-string . args -> void
;; Emits formatted text nicely, with indention and linebreaks
(define emit-text
  (let ((linebreak (string #\newline)))
    (lambda (as fmt . operands)
      (emit-string! as code-indentation)
      (emit-string! as (apply twobit-format #f fmt operands))
      (emit-string! as linebreak))))

;; indention control
(define code-indentation "")
(define code-indentation-length 0)
(define code-indention-delta 2)
(define (indent-up!)
  (set! code-indentation-length 
        (+ code-indention-delta code-indentation-length))
  (set! code-indentation 
        (make-string code-indentation-length #\space)))
(define (indent-down!)
  (set! code-indentation-length 
        (- code-indentation-length code-indention-delta))
  (set! code-indentation 
        (make-string code-indentation-length #\space)))

;; /Code Emission --

;; -----------------
;; CodeVectors
;; -----------------

;; FIRST-LABEL : number
;; Label given to first basic block in a codevector.
(define FIRST-LABEL 0)

;; begin-codevector-class : assember label boolean -> void
;; Begins a new CodeVector class declaration for a basic block.
(define (begin-codevector-class as label entrypoint?)
  (end-codevector-class as)
  (let ((name (codevector-name as label)))
    (emit-text as "class ~a : CodeVector {" name)
    (indent-up!)
    (emit-text as "private ~a() {}" name)
    (emit-text 
     as "public static readonly CodeVector instance = new ~a();" name)
    (add-function as name #t entrypoint?) ;; mimicked from C version
    (emit-text 
     as "public override void call(int label_index) {")
    (indent-up!)
    (emit-text as "switch (label_index) { ")
    (emit-text as "case ~a:" FIRST-LABEL)
    (indent-up!)
    (assembler-value! as 'within-codevector-class-declaration name)
    (assembler-value! 
     as 'codevector-classes
     (cons (list name) (assembler-value as 'codevector-classes)))
    (assembler-value! as 'basic-block-closed #f)))

;; end-codevector-class : assembler -> void
;; Close a class declaration.
(define (end-codevector-class as)
  (if (assembler-value as 'within-codevector-class-declaration)
      (begin
        (indent-down!)
        (emit-text as "}")
        (indent-down!)
        (emit-text as "}")
        (indent-down!)
        (emit-text as "}~%")
        (assembler-value! as 'within-codevector-class-declaration #f)
        (assembler-value! as 'basic-block-closed #f))))

;; codevector-name : assembler label -> string
;; Generates a class name for a CodeVector class
(define (codevector-name as label)
  (twobit-format #f "Proc_CodeVector_~a_~a" 
                 (user-data.toplevel-counter (as-user as)) 
                 label))

;; codevector-instance : assembler entry boolean -> string
;; Generates a reference to an instance of a CodeVector.
(define (codevector-instance as entry)
  (string-append (codevector-name as entry) ".instance"))

;; /CodeVectors ----

;; -----------------
;; Basic blocks
;; -----------------

;; begin-basic-block : assembler label -> void
(define (begin-basic-block as label)
  (end-basic-block as label)
  (assembler-value! as 'basic-block-closed #f)
  (let ((basic-block-label (basic-block-name as label)))
    (indent-down!)
    (emit-text as "~a:" basic-block-label)
    (indent-up!)))

;; end-basic-block : assembler label boolean boolean -> void
;; Intended to catch fall-through case: basic block ends without an explicit
;; unconditional transfer of control. (eg: loop.sch)
;; Note: new-label is the label being started, not the one being closed.
(define (end-basic-block as new-label)
  (if (not (assembler-value as 'basic-block-closed))
      (begin (emit-local-jump as new-label)
             (emit-text as "/* Control falls through to ~a */" new-label)
             (assembler-value! as 'basic-block-closed #t))))

;; emit-implicit-continuation-break : assembler number -> void
(define (emit-implicit-continuation-break as numeric)
  (begin-basic-block as numeric))

;; basic-block-name : assembler label -> void
(define (basic-block-name as label)
  (twobit-format #f "case ~a" label))


;; /Basic blocks ---

;; -----------------
;; C# translation
;; -----------------

;; csharp-op-name : symbol -> string
;; For op1, op2, etc instructions: translate operation name to legal C#.
;; eg: + to plus, char<? to char_lt, null? to nullp, set! to set
(define (csharp-op-name sym)
  (case sym
    ((+) "plus")
    ((-) "minus")
    ((*) "multiply")
    ((/) "divide")
    ((=) "numeric_equals")
    ((<) "less_than")
    ((>) "greater_than")
    ((<=) "less_or_equal")
    ((>=) "greater_or_equal")
    ((char<?) "char_lt")
    ((char<=?) "char_le")
    ((char>?) "char_gt")
    ((char>=?) "char_ge")
    ((char=?) "char_equals")
    ((--) "negative")
    (else
     (apply string-append
            (map csharp-switch-char 
                 (string->list (symbol->string sym)))))))

;; csharp-switch-char : char -> string
;; String to replace char in operation names (see above)
(define (csharp-switch-char char)
  (case char
    ((#\- #\> #\< #\:) "_")
    ((#\?) "p")
    ((#\!) "")
    (else (string char))))
  
;; immediate-constant? : any -> boolean
;; Is the value a constant which may be expressed "inlined" in
;; the program? (eg, fixnum constants, certain special values...)
(define (immediate-constant? x)
  (or (fixnum? x)
      (null? x)
      (boolean? x)
      (char? x)
      (eof-object? x)
      (equal? x (unspecified))
      (equal? x (undefined))))

;; constant-value : any -> string
;; Express a constant as a C# expression
;; FIXME: NEEDS EXAMINATION: This might assume the representations used 
;; by normal Larceny, which must be changed for Common Larceny.
(define (constant-value x)
  
  (define (exact-int->fixnum x)
    (twobit-format #f "SchemeFixnum.makeFixnum(~a)" x))
  
  (define (char->immediate c)
    (twobit-format #f "SchemeImmediate.makeChar(~a)" (char->integer c)))
  
  (cond ((fixnum? x)              (exact-int->fixnum x))
        ((eq? x #t)               "SchemeObject.True")
        ((eq? x #f)               "SchemeObject.False")
	((equal? x (eof-object))  "SchemeObject.Eof")
	((equal? x (unspecified)) "SchemeObject.Void")
	((equal? x (undefined))   "SchemeObject.Undefined")
        ((null? x)                "SchemeObject.Null")
        ((char? x)                (char->immediate x))
	(else ???)))

;; emit-local-jump : assembler label -> void
;; Emit code for a jump from one basic block to another within the same 
;; codevector (see branch, skip, etc)
(define (emit-local-jump as label)
  (emit-text as "goto case ~a;" label))

;; /C# translation -

;; -----------------
;; Attic
;; -----------------

;;(define (add-compiled-scheme-function as label entrypoint? start?)
;;  (let ((name (compiled-procedure as label start?)))
;;    (if (not (assoc name (lookup-functions as)))
;;        (add-function as name #t entrypoint?))))


(define (lookup-functions as)
  '(twobit-format (current-output-port)
                 "functions are: ~s~%" (assembler-value as 'functions))
  (or (assembler-value as 'functions) '()))

(define (add-function as name definite? entrypoint?)
  (assembler-value! as 'functions (cons (list name definite? entrypoint?)
                                        (lookup-functions as)))
  name)

(define (implicit-procedure as)
  (let ((id (new-proc-id as)))
    (values
     id
     ;; in C, was "compiled_temp_~a_~a" 
     (twobit-format #f "Implicit_Procedure_~a_~a"
                    (user-data.toplevel-counter (as-user as))
                    id))))

(define (new-proc-id as)
  (let* ((u (as-user as))
	 (x (user-data.proc-counter u)))
    (user-data.proc-counter! u (+ 1 x))
    x))

;; /Attic ----------

;; -----------------
;; Instructions
;; -----------------


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
                      (emit-text as "/* End */")
                      (end-codevector-class as)))

(define-instruction $.entry
                    (lambda (instruction as)
                      ;; Entry point... marks beginning of codevector
                      (list-instruction ".entry" instruction)
                      (begin-codevector-class as 
                                              (operand1 instruction)
                                              (operand2 instruction))
                      (emit-text as "/* Entry point: ~a, ~a */"
                                 (operand1 instruction)
                                 (operand2 instruction))))

(define-instruction $.label
                    (lambda (instruction as)
                      ;; Marks beginning of basic block
                      (list-label instruction)
                      (begin-basic-block as (operand1 instruction))
                      (emit-text as "/* Label: ~a */"
                                 (operand1 instruction))))

(define-instruction $.proc
                    (lambda (instruction as)
                      (list-instruction ".proc" instruction)))

(define-instruction $.proc-doc
                    (lambda (instruction as)
                      (list-instruction ".proc-doc" instruction)
                      (add-documentation as (operand1 instruction))))

(define-instruction $.singlestep
                    (lambda (instruction as)
                      ;; We don't handle
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
                             (emit-text 
                              as "Ops.op1_~a(~a); /* (opcode ~a) ~a */"
                              (csharp-op-name (operand1 instruction))
                              numeric  ;; symbolic
                              (op1-primcode (operand1 instruction))
                              (operand1 instruction))
                             (emit-implicit-continuation-break as numeric)))
                          (emit-text as "Ops.op1_~a(); /* (opcode ~a) ~a */"
                                     (csharp-op-name (operand1 instruction))
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
                             (emit-text
                              as 
                              "Ops.op2_~a(~a, ~a); /* (opcode ~a) ~a */"
                              (csharp-op-name (operand1 instruction))
                              (operand2 instruction)
                              numeric  ;; symbolic
                              (op2-primcode (operand1 instruction))
                              (operand1 instruction))
                             (emit-implicit-continuation-break as numeric)))
                          (emit-text as "Ops.op2_~a(~a); /* (opcode ~a) ~a */"
                                     (csharp-op-name (operand1 instruction))
                                     (operand2 instruction)
                                     (op2-primcode (operand1 instruction))
                                     (operand1 instruction)))))

(define-instruction $op2imm
                    (lambda (instruction as)
                      (list-instruction "op2imm" instruction)
                      (if (op2imm-implicit-continuation? (operand1 instruction))
                          (call-with-values
                           (lambda () (implicit-procedure as))
                           (lambda (numeric symbolic)
                             (add-function as symbolic #f #f)
                             (emit-text 
                              as 
                              "Ops.op2imm_~a(~a, ~a); /* (opcode ~a) ~a */"
                              (csharp-op-name (operand1 instruction))
                              (constant-value (operand2 instruction))
                              numeric  ;; symbolic
                              (op2imm-primcode (operand1 instruction))
                              (operand1 instruction))
                             (emit-implicit-continuation-break as numeric)))
                          (emit-text 
                           as
                           "Ops.op2imm_~a(~a); /* (opcode ~a) ~a */"
                           (csharp-op-name (operand1 instruction))
                           (constant-value (operand2 instruction))
                           (op2imm-primcode (operand1 instruction))
                           (operand1 instruction)))))

(define-instruction $op3
                    (lambda (instruction as)
                      (list-instruction "op3" instruction)
                      (if (op3-implicit-continuation? (operand1 instruction))
                          (call-with-values
                           (lambda () (implicit-procedure as))
                           (lambda (numeric symbolic)
                             (add-function as symbolic #f #f)
                             (emit-text
                              as 
                              "Ops.op3_~a(~a, ~a, ~a); /* (opcode ~a) ~a */"
                              (csharp-op-name (operand1 instruction))
                              (operand2 instruction)
                              (operand3 instruction)
                              numeric  ;; symbolic
                              (op3-primcode (operand1 instruction))
                              (operand1 instruction))
                             (emit-implicit-continuation-break as numeric)))
                          (emit-text 
                           as "Ops.op3_~a(~a, ~a); /* (opcode ~a) ~a */"
                           (csharp-op-name (operand1 instruction))
                           (operand2 instruction)
                           (operand3 instruction)
                           (op3-primcode (operand1 instruction))
                           (operand1 instruction)))))

(define-instruction $const
                    (lambda (instruction as)
                      (list-instruction "const" instruction)
                      (if (immediate-constant? (operand1 instruction))
                          (emit-text as "Reg.imm_constant( ~a ); /* ~a */"
                                     (constant-value (operand1 instruction))
                                     (operand1 instruction))
                          (emit-text as "Reg.constant( ~a );"
                                     (emit-datum as (operand1 instruction))))))

(define-instruction $global
                    (lambda (instruction as)
                      (list-instruction "global" instruction)
                      (emit-text as "Reg.global( ~a ); /* identifier: ~a */"
                                 (emit-global as (operand1 instruction))
                                 (operand1 instruction))))

(define-instruction $setglbl
                    (lambda (instruction as)
                      (list-instruction "setglbl" instruction)
                      (emit-text as "Reg.setglbl( ~a ); /* identifier: ~a */"
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
                           (assembler-value! 
                            as 'functions
                            (append (lookup-functions as)
                                    (lookup-functions nested-as)))
                           (set-constant! as code-offset (car segment))
                           (set-constant! as const-offset (cdr segment)))
                         (as-user as))
                        (list-lambda-end)
                        (set! code-offset (emit-codevector as 0))
                        (set! const-offset (emit-constantvector as 0))
                        (emit-text as "Closure.lambda( ~a, ~a, ~a );"
                                   (codevector-instance as entry)
                                   const-offset
                                   (operand2 instruction)))))

(define-instruction $lexes
                    (lambda (instruction as)
                      (list-instruction "lexes" instruction)
                      (emit-text as "Closure.lexes( ~a );" 
                                 (operand1 instruction))))

(define-instruction $args=
                    (lambda (instruction as)
                      (list-instruction "args=" instruction)
                      (if (not (unsafe-code))
                          (emit-text as "Closure.argseq( ~a );" 
                                     (operand1 instruction)))))

(define-instruction $args>=
                    (lambda (instruction as)
                      (list-instruction "args>=" instruction)
                      (if (not (unsafe-code))
                          (emit-text as "Closure.argsge( ~a );" 
                                     (operand1 instruction)))))

(define-instruction $invoke
                    (lambda (instruction as)
                      (list-instruction "invoke" instruction)
                      (emit-text as "Call.invoke( ~a );" (operand1 instruction))
                      (emit-text as "// TAIL CALL (invoke)")
                      (emit-text as "return;")
                      (assembler-value! as 'basic-block-closed #t)))

;; FIXME: why negative numbers, > NREGS registers?
(define-instruction $restore
                    (lambda (instruction as)
                      (if (not (negative? (operand1 instruction)))
                          (begin
                            (list-instruction "restore" instruction)
                            (emit-text as "Call.restore( ~a );"
                                       (min (operand1 instruction) 
                                            (- *nregs* 1)))))))

;; FIXME: why negative numbers?
(define-instruction $pop
                    (lambda (instruction as)
                      (if (not (negative? (operand1 instruction)))
                          (begin
                            (list-instruction "pop" instruction)
                            (emit-text as "Cont.pop( ~a );" 
                                       (operand1 instruction))))))

(define-instruction $popstk
                    (lambda (instruction as)
                      (error "POPSTK is not implemented by this assembler.")))

(define-instruction $stack
                    (lambda (instruction as)
                      (list-instruction "stack" instruction)
                      (emit-text as "Cont.stack(~a);" (operand1 instruction))))

(define-instruction $setstk
                    (lambda (instruction as)
                      (list-instruction "setstk" instruction)
                      (emit-text as "Cont.setstk(~a);" (operand1 instruction))))

(define-instruction $load
                    (lambda (instruction as)
                      (list-instruction "load" instruction)
                      (emit-text as "Cont.load(~a, ~a);"
                                 (operand1 instruction) 
                                 (operand2 instruction))))

(define-instruction $store
                    (lambda (instruction as)
                      (list-instruction "store" instruction)
                      (emit-text as "Cont.store(~a, ~a);"
                                 (operand1 instruction)
                                 (operand2 instruction))))

(define-instruction $lexical
                    (lambda (instruction as)
                      (list-instruction "lexical" instruction)
                      (emit-text as "Closure.lexical(~a, ~a);"
                                 (operand1 instruction)
                                 (operand2 instruction))))

(define-instruction $setlex
                    (lambda (instruction as)
                      (list-instruction "setlex" instruction)
                      (emit-text as "Closure.setlex(~a, ~a);"
                                 (operand1 instruction)
                                 (operand2 instruction))))

(define-instruction $reg
                    (lambda (instruction as)
                      (list-instruction "reg" instruction)
                      (emit-text as "Reg.reg(~a);" (operand1 instruction))))

(define-instruction $setreg
                    (lambda (instruction as)
                      (list-instruction "setreg" instruction)
                      (emit-text as "Reg.setreg(~a);" (operand1 instruction))))

(define-instruction $movereg
                    (lambda (instruction as)
                      (list-instruction "movereg" instruction)
                      (emit-text as "Reg.movereg(~a, ~a);"
                                 (operand1 instruction) 
                                 (operand2 instruction))))

(define-instruction $return
                    (lambda (instruction as)
                      (list-instruction "return" instruction)
                      (emit-text as "Cont.rtn();")
                      (emit-text as "// TAIL CALL (return)")
                      (emit-text as "return;")
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $nop
                    (lambda (instruction as)
                      (list-instruction "nop" instruction)
                      (emit-text as "Reg.nop();")))

(define-instruction $save
                    (lambda (instruction as)
                      (if (not (negative? (operand1 instruction)))
                          (begin
                            (list-instruction "save" instruction)
                            (emit-text as "Cont.save(~a);" 
                                       (operand1 instruction))))))

(define-instruction $setrtn
                    (lambda (instruction as)
                      (list-instruction "setrtn" instruction)
                      (emit-text as "Cont.setrtn(this, ~a);"
                                 (operand1 instruction))))

(define-instruction $apply
                    (lambda (instruction as)
                      (list-instruction "apply" instruction)
                      (emit-text as "Call.apply(~a, ~a);"
                                 (operand1 instruction)
                                 (operand2 instruction))
                      (emit-text as "// TAIL CALL (apply)")
                      (emit-text as "return;")
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $jump
                    (lambda (instruction as)
                      (list-instruction "jump" instruction)
                      ;; The procedure M links up the static chain is stored 
                      ;; in lexical address (M-1, 0). We make that the current
                      ;; procedure and branch.
                      (emit-text as "{")
                      (indent-up!)
                      (emit-text as "SchemeObject temp = Reg.Result;")
                      (emit-text as "Procedure proc;")
                      (emit-text as "Closure.lexical(~a-1, 0);" (operand1 instruction))
                      (emit-text as "proc = (Procedure) Reg.Result;")
                      (emit-text as "Reg.setreg(0);")
                      (emit-text as "Reg.Result = temp;")
                      (emit-text as "Call.call(proc.entrypoint, ~a);"
                                 (operand2 instruction))
                      (emit-text as "// TAIL CALL (jump)")
                      (emit-text as "return;")
                      (indent-down!)
                      (emit-text as "}")
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $skip
                    (lambda (instruction as)
                      (list-instruction "skip" instruction)
                      (emit-text as "/* skip to ~a */"
                                 (operand1 instruction))
                      (emit-local-jump as (operand1 instruction))
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $branch
                    (lambda (instruction as)
                      (list-instruction "branch" instruction)
                      (emit-text as "/* branch to ~a */"
                                 (operand1 instruction))
                      (emit-local-jump as (operand1 instruction))
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $branchf
                    (lambda (instruction as)
                      (list-instruction "branchf" instruction)
                      (emit-text as "if (Reg.Result == SchemeObject.False) {")
                      (indent-up!)
                      (emit-text as "/* branchf to ~a */"
                                 (operand1 instruction))
                      (emit-local-jump as (operand1 instruction))
                      (indent-down!)
                      (emit-text as "}")))

(define-instruction $check
                    (lambda (instruction as)
                      (list-instruction "check" instruction)
                      (emit-text as "/* CHECK (~a, ~a, ~a) => ~a */"
                                 (operand1 instruction)
                                 (operand2 instruction) 
                                 (operand3 instruction)
                                 (operand4 instruction))
                      (emit-text as "if (Reg.Result == SchemeObject.False) {")
                      (indent-up!)
                      (emit-local-jump as (operand4 instruction))
                      (indent-down!)
                      (emit-text as "}")))

(define-instruction $trap
                    (lambda (instruction as)
                      (list-instruction "trap" instruction)
                      (emit-text as "Exn.trap( ~a, ~a, ~a, ~a );"
                                 (operand1 instruction)
                                 (operand2 instruction)
                                 (operand3 instruction)
                                 (operand4 instruction))
                      (emit-text as "// TAIL CALL (trap)")
                      (emit-text as "return;")
                      (assembler-value! as 'basic-block-closed #t)))

(define-instruction $const/setreg
                    (lambda (instruction as)
                      (list-instruction "const/setreg" instruction)
                      (if (immediate-constant? (operand1 instruction))
                          (emit-text 
                           as "Reg.imm_const_setreg( ~a, ~a ); /* ~a */"
                           (constant-value (operand1 instruction))
                           (operand2 instruction)
                           (operand1 instruction))
                          (emit-text as "Reg.const_setreg( ~a, ~a );"
                                     (emit-datum as (operand1 instruction))
                                     (operand2 instruction)))))

;; /Instructions ---
