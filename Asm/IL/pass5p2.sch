; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$

;; NOTES
;; Uses syntax defined in il-gen, thus order of loading is important

;; -----------------
;; Assembler
;; -----------------

;; Table of instructions
(define (assembly-table) $il-assembly-table$)
(define $il-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (error "Unrecognized mnemonic " instruction))))

(define (define-instruction i implementation0 . implementations)
  (cond ((null? implementations)
         (define-instruction/helper i implementation0))
        ((and (pair? implementations) (null? (cdr implementations)))
         (define-instruction/helper i 
                                    (if (codegen-option i)
                                        implementation0 ;; inlining
                                        (car implementations)))) ;; standard
        (else (error "bad define-instruction"))))

(define (define-instruction/helper i proc)
  (vector-set! $il-assembly-table$ i proc)
  #t)

;; new-proc-id : assembler -> number
(define (new-proc-id as)
  (let* ((u (as-user as))
	 (x (user-data.proc-counter u)))
    (user-data.proc-counter! u (+ 1 x))
    x))

;; new-label : assembler -> number
(define (new-label as)
  (new-proc-id as))
;  (let* ((u (as-user as))
;         (x (user-data.label-counter u)))
;    (user-data.label-counter! u (+ 1 x))
;    x))

;; emit-constantvector-slot : as symbol value -> number
(define (emit-constantvector-slot as kind value)
  (let ((all-constants (as-constants as))
        (item (list kind value)))
    (let loop ((n 0) (constants all-constants))
      (cond ((null? constants)
             (begin (as-constants! as (append! all-constants (list item)))
                    n))
            ((equal? (car constants) item)
             n)
            (else
             (loop (+ 1 n) (cdr constants)))))))

;; set-constantvector-slot! : as symbol number value -> void
(define (set-constantvector-slot! as kind index value)
  (let ((constants (as-constants as)))
    (let loop ((n index) (constants constants))
      (cond ((zero? n)
             (set-car! (cdr (car constants)) value))
            (else
             (loop (- n 1) (cdr constants)))))))

;; emit-X : as value -> number
;; Emits an X into the constant vector, returning the number of 
;; all X before it (can be used as index into X-only array)
(define (emit-datum as datum)
  (emit-constantvector-slot as 'data datum))
(define (emit-global as identifier)
  (emit-constantvector-slot as 'global identifier))
(define (emit-codevector as codevector)
  (emit-constantvector-slot as 'codevector codevector))
(define (emit-constantvector as constantvector)
  (emit-constantvector-slot as 'constantvector constantvector))

; User-data structure is shared between all assembly structures operating
; on a particular source; assemble-nested-lambda passes user-data to new 
; assembly structure.

; User-data structure has these fields:
;  il-namespace         A unique namespace identifier for this module
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for codevectors
;  label-counter        A serial number for labels
;  label-map            alist: num => (cons num cvid)
;                       Mapping twobit label to (jump-index . codevector-id)

(vector-struct $$user-data raw-make-user-data user-data?
               (user-data.il-namespace user-data.il-namespace!)
               (user-data.toplevel-counter user-data.toplevel-counter!)
               (user-data.proc-counter user-data.proc-counter!)
               (user-data.label-counter user-data.label-counter!)
               (user-data.label-map user-data.label-map!))

(define (make-user-data)
  (raw-make-user-data (generate-globally-unique-id) 0 0 1 (make-gvector '())))

(define *unique-id-counter* 0)
(define (generate-globally-unique-id)
  (set! *unique-id-counter* (+ 1 *unique-id-counter*))
  (twobit-format #f "ns~sc~s" 
                 (remainder (current-seconds) 10000)
                 *unique-id-counter*))

(define (as-il-namespace as)
  (user-data.il-namespace (as-user as)))

;; Assembler value slots:
;;   'current-codevector : (cons num num)
;;   'basic-block-closed : boolean indicating whether control will have 
;;           always been transferred out of the basic block at this point
;;           (eg, by a branch, rtn, etc)
;;   'next-jump-index : number indicating the next jump index to allocate
;;                      for the current codevector
;;   'local-variables : ??? (taken from ilprops)

(define-syntax define-assembler-get/set
  (syntax-rules ()
    ((_ (get set prop) ...)
     (begin (define (get as) (assembler-value as prop)) ...
            (define (set as v) (assembler-value! as prop v)) ...))))

(define-assembler-get/set
 (as:current-codevector as:current-codevector! 'current-codevector)
 (as:basic-block-closed as:basic-block-closed! 'basic-block-closed)
 (as:next-jump-index as:next-jump-index! 'next-jump-index)
 (as:local-variables as:local-variables! 'local-variables))

;; assemble-finalize! : assembler -> void
(define (assemble-finalize! as)
  (for-each (lambda (nested-as-proc)
              (nested-as-proc))
            (as-nested as)))

;; /Assembler ------

;; cvclass 
;; Data structure for holding the essential code portion of a 
;; codevector class. Contains the id=(cons num num) of the codevector
;; represented, the IL instruction stream (in correct order), and 
;; the data from the constant vector (but not the globals and nested 
;; constant vectors).

(vector-struct $$cvclass make-cvclass cvclass?
               (cvclass-il-namespace cvclass-il-namespace!)
               (cvclass-id cvclass-id!)
               (cvclass-instrs cvclass-instrs!)
               (cvclass-constants cvclass-constants!))

;; /Code Emission --

;; -----------------
;; CodeVectors
;; -----------------

;; FIRST-JUMP-INDEX : number 
(define FIRST-JUMP-INDEX 0)

;; LOCAL-RESULT : number | #f
(define LOCAL-RESULT 
  (if (codegen-option 'cache-result)
      (let ((RESULT FIRST-LOCAL))
        (set! FIRST-LOCAL (+ 1 FIRST-LOCAL))
        RESULT)
      #f))

;; LOCAL-CONSTANT-VECTOR : number | #f
(define LOCAL-CONSTANT-VECTOR
  (if (codegen-option 'cache-constant-vector)
      (let ((CONSTANTS FIRST-LOCAL))
        (set! FIRST-LOCAL (+ 1 FIRST-LOCAL))
        CONSTANTS)
      #f))

;; begin-codevector-class : assember label boolean -> void
;; Begins a new CodeVector class declaration for a basic block.
(define (begin-codevector-class as label entrypoint?)
  (end-codevector-class as)
  (let* ((id (codevector-id as label))
         (name (codevector-name id)))
    
    (add-function as id (as-il-namespace as) #t entrypoint?)
    ;; mimicked from C version
    
    (as:current-codevector! as id)
    (as:next-jump-index! as FIRST-JUMP-INDEX)
    (as:local-variables! as '())

    (as-code! as '())
    (emit as 
          (il:delay
           (il:directive 'local (map car (as:local-variables as))))
          ;; WARNING: Keep same order as LOCAL-RESULT, LOCAL-CONSTANT-VECTOR above
          (if (codegen-option 'cache-result)
              (begin 
                (allocate-il-local as iltype-schemeobject)
                (list
                 (il:comment "Caching Reg.Result in local variable ~s" 
                             LOCAL-RESULT)
                 (il:recache-result)))
              '())
          (if (codegen-option 'cache-constant-vector)
              (begin
                (allocate-il-local as iltype-schemeobject-array)
                (list
                 (il:comment "Caching constant vector in local variable ~s"
                             LOCAL-CONSTANT-VECTOR)
                 (il:recache-constant-vector)))
              '())
          (il:comment "Switch on jump index")
          (il 'ldarg 1)
          (il:delay 
           (il 'switch (map label-name (as:collect-local-labels as id))))
          (il:comment "First (default) target")
          (intern-label as id)
          (il:label/header id))))

;; end-codevector-class : assembler -> void
;; Close a class declaration.
(define (end-codevector-class as)
  (if (as:current-codevector as)
      (let* ((user (as-user as))
             (current-codevector (as:current-codevector as)))
        (as-code! 
         as 
         (make-cvclass (user-data.il-namespace user)
                       current-codevector 
                       (reverse (as-code as))
                       '()))
        (as:basic-block-closed! as #t))))

;; codevector-id : assembler number -> codevector-id == (cons num num)
(define (codevector-id as label)
  (cons (user-data.toplevel-counter (as-user as)) label))

;; codevector-name : codevector-id -> string
;; Generates a class name for a CodeVector class
(define (codevector-name id)
  (twobit-format #f "CodeVector_~a_~a" (car id) (cdr id)))

;; /CodeVectors ----

;; -----------------
;; Basic blocks
;; -----------------

;; begin-basic-block : assembler label -> void
(define (begin-basic-block as label)
  (end-basic-block as label)
  (intern-label as label)
  (emit as (il:label label))
  (as:basic-block-closed! as #f))

;; end-basic-block : assembler label boolean boolean -> void
;; In IL, we don't need to insert explicit fallthrough jumps
(define (end-basic-block as new-label)
  (if (not (as:basic-block-closed as))
      (begin (emit as (il:comment "Control falls through to ~a" new-label))
             (as:basic-block-closed! as #t))))

;; allocate-label : assembler -> number
(define (allocate-label as)
  (- (new-label as)))

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
    ((exact->inexact) "exact2inexact")
    ((inexact->exact) "inexact2exact")
    ((char<?) "char_lt")
    ((char<=?) "char_le")
    ((char>?) "char_gt")
    ((char>=?) "char_ge")
    ((char=?) "char_equals")
    ((char->integer) "char2integer")
    ((integer->char) "integer2char")
    ((fx+) "fxplus")
    ((fx-) "fxminus")
    ((fx*) "fxmul")
    ((fx>) "fxgreater")
    ((fx<) "fxless")
    ((fx<=) "fxless_equal")
    ((fx>=) "fxgreater_equal")
    ((fx=) "fxequal")
    ((fx--) "fxnegative")
    ((fxzero?) "fxzerop")
    ((fxpositive?) "fxpositivep")
    ((fxnegative?) "fxnegativep")
    ((--) "negative")
    (else (csharp-sanitize-name (symbol->string sym)))))

(define (csharp-sanitize-name str)
  (let ((chars (string->list str)))
    (apply string-append
           (cons ((csharp-switch-char 
                   (append csharp-initial-character-mapping
                           csharp-character-mapping))
                  (car chars))
                 (map (csharp-switch-char csharp-character-mapping)
                      (cdr chars))))))

(define csharp-character-mapping
  '((#\+ "plus")
    (#\= "equal")
    (#\- "_")
    (#\< "_")
    (#\> "_")
    (#\: "_")
    (#\? "p")
    (#\$ "_")
    (#\! "")))
(define csharp-initial-character-mapping
  '((#\- "minus")
    (#\> "greater")
    (#\< "less")))

;; csharp-switch-char : char-mapping -> char -> string
;; String to replace char in operation names (see above)
(define (csharp-switch-char mapping)
  (lambda (char)
    (cond ((assoc char mapping) => cadr)
          (else (string char)))))

;; immediate-constant? : any -> boolean
;; Is the value a constant which may be expressed "inlined" in
;; the program? (takes a single load or pool access)
;; NOTE: Symbols should never be considered immediate.
(define (immediate-constant? x)
  (or (immediate-fixnum? x)
      (null? x)
      (boolean? x)
      (immediate-char? x)
      (eof-object? x)
      (equal? x (unspecified))
      (equal? x (undefined))))

(define (immediate-fixnum? x)
  (and (fixnum? x) (<= FIXNUM-POOL-MIN x FIXNUM-POOL-MAX)))

(define (immediate-char? x)
  (and (char? x) (<= CHAR-POOL-MIN (char->integer x) CHAR-POOL-MAX)))

;; /C# translation -

;; -----------------
;; Attic
;; -----------------

(define (lookup-functions as)
  '(twobit-format (current-output-port)
                 "functions are: ~s~%" (assembler-value as 'functions))
  (or (assembler-value as 'functions) '()))

(define (add-function as name il-namespace definite? entrypoint?)
  (assembler-value! as 'functions 
                    (cons (list name il-namespace definite? entrypoint?)
                          (lookup-functions as)))
  name)

;; ------------------------------
;; Listify, .list file generation
;; ------------------------------

(define (list-instruction/line name instr as)
  (let ((line listify-counter))
    (emit as
          (if (codegen-option 'listify-line-directive)
              (il:directive 'line line listify-filename))
          (il:comment/info "instruction" (cons (string->symbol name) (cdr instr)))
          (if (and (codegen-option 'listify-debug-location)
                   (not (member name '(".end"))))
              (il:set-debug-info line listify-filename (member name '(".cont" ".proc")))
              '()))
    (if (codegen-option 'listify-write-list-file)
        (write-listify-line line name instr))))

(define (list-label/line instr as)
  (let ((line listify-counter))
    (emit as 
          (il:comment/info "Instruction" (cons '.label (cdr instr))))
    (if (codegen-option 'listify-write-list-file)
        (write-listify-label line (cadr instr)))))

;; list-entry/line : symbol instruction assembler -> number
(define (list-entry/line name instruction as)
  (if (codegen-option 'listify-write-list-file)
      (begin
        (listify-newline)
        (twobit-format listify-oport "Procedure ~s"
                       (assembler-value as 'current-codevector))
        (listify-newline)))
  (list-instruction/line name instruction as))

(define (il:set-debug-info line filename set-filename?)
  (il:comment/wrap "set-debug-info"
                   (il 'ldc.i4 line)
                   (il:stsfld iltype-int32 il-reg "debugLocation")
                   (if set-filename?
                       (list (il:ldstr listify-filename)
                             (il:stsfld iltype-string il-reg "debugFile"))
                       '())))

(define (write-listify-line line name instruction)
  (display list-indentation listify-oport)
  (display "        " listify-oport)
  (display name listify-oport)
  (display (make-string (max (- 12 (string-length name)) 1)
                        #\space)
           listify-oport)
  (if (not (null? (cdr instruction)))
      (begin (write (cadr instruction) listify-oport)
             (do ((operands (cddr instruction)
                            (cdr operands)))
               ((null? operands))
               (write-char #\, listify-oport)
               (write (car operands) listify-oport))))
  (listify-newline))

(define (write-listify-label line label)
  (display list-indentation listify-oport)
  (write-char #\L listify-oport)
  (write label listify-oport)
  (listify-newline))

(define list-indentation "")
(define listify-oport #f)
(define listify-filename #f)
(define listify-counter 0)

(define (listify-reset)
  (set! list-indentation "")
  (set! listify-oport #f)
  (set! listify-filename #f)
  (set! listify-counter 1))

(define (listify-newline)
  (newline listify-oport)
  (set! listify-counter (+ 1 listify-counter)))

;; /Attic ----------

;; Instruction implementations in pass5p2-instructions.sch

;; Operation implementations in pass5p2-ops.sch
