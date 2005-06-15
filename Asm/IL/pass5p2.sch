; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$

;; NOTES
;; Uses syntax defined in il-gen, thus order of loading is important

;; The following procedures are overridden from Twobit/Common/pass5p1:
;;   assemble-pasteup
;;   assemble-finalize!
;;   assembly-start
;;   assembly-end
;;   assembly-user-data

; An assembly structure is a vector consisting of
;
;    table          (a table of assembly routines)
;    source         (a list of symbolic instructions)
;    lc             (location counter; an integer)
;    code           (a list of bytevectors)
;    constants      (a list)
;    labels         (an alist of labels and values)
;    fixups         (an alist of locations, sizes, and labels or fixnums)
;    nested         (a list of assembly procedures for nested lambdas)
;    values         (an assoc list)
;    parent         (an assembly structure or #f)
;    retry          (a thunk or #f)
;    user-data      (anything)

;; assemble-pasteup : assembler -> (cons code constants)
;; Forces all il:delay ilpackages in the instruction stream
(define (assemble-pasteup as)
  (let* ((code (as-code as))
         (instrs (cvclass-instrs code)))
    (let loop ((instrs instrs) (processed-instrs '()))
      (cond ((null? instrs)
             (cons (make-cvclass (cvclass-il-namespace code)
                                 (cvclass-id code)
                                 (reverse processed-instrs)
                                 (cvclass-constants code)
                                 (cvclass-label-count code))
                   (list->vector (as-constants as))))
            ((string? (car instrs))
             (loop (cdr instrs) (cons (car instrs) processed-instrs)))
            ((il? (car instrs)) ;; Is it a normal IL instr?
             (loop (cdr instrs) (cons (car instrs) processed-instrs)))
            ((il-delay? (car instrs))
             (let ((forced (il-delay-force (car instrs))))
               (loop (append forced (cdr instrs)) processed-instrs)))
            (else (error 'something-else-in-assembler-code))))))

;; assemble-finalize! : assembler -> void
(define (assemble-finalize! as)
  (for-each (lambda (p) (p)) (as-nested as))
  (as-nested! as '()))

;; assembly-start : assembler -> ??
;; Initializes the assembler?
(define (assembly-start as)
  (let ((u (as-user as)))
    (user-data.proc-counter! u 0)
    (user-data.toplevel-counter! u (+ 1 (user-data.toplevel-counter u))))
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as)))))

;; assembly-end : assembler ?? -> (list code constants codevector-ids???)
(define (assembly-end as segment)
  (list (car segment) (cdr segment) (lookup-functions as)))

;; assembly-user-data : -> user-data
(define (assembly-user-data)
  (make-user-data))

;; lookup-functions : as -> ??
(define (lookup-functions as)
  '(twobit-format (current-output-port)
                 "functions are: ~s~%" (assembler-value as 'functions))
  (or (assembler-value as 'functions) '()))

;; add-function : as ... -> void
(define (add-function as name il-namespace definite? entrypoint?)
  (assembler-value! as 'functions
                    (cons (list name il-namespace definite? entrypoint?)
                          (lookup-functions as)))
  name)

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

;; Set this string to something meaningful before
;; assembling and then you can figure out what namespace
;; belongs to what file.
(define *unique-id-cookie* "")
(define *unique-id-counter* 0)
(define (generate-globally-unique-id)
  (set! *unique-id-counter* (+ 1 *unique-id-counter*))
  (twobit-format #f "ns~a~sc~s"
                 *unique-id-cookie*
                 (an-arbitrary-number) ;; Defined by compat package
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
               (cvclass-constants cvclass-constants!)
               (cvclass-label-count cvclass-label-count!))

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
           (list (il:directive 'local (map car (as:local-variables as)))))
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
          (il:delay
           (let ((labels (as:collect-local-labels as id)))
             (cond ((null? (cdr labels))
                    (list (il:comment "Jump index ignored.")))
                   ((= (length labels) 2)
                    (list
                     (il:comment "Branch on jump index")
                     (il 'ldarg 1)
                     (il:branch 'brtrue (cadr labels))))
                   ((= (length labels) 3)
                    (list
                     (il:comment "Branch on jump index")
                     (il 'ldarg 1)
                     (il:branch-s 'brfalse (car labels))
                     (il 'ldarg 1)
                     (il 'ldc.i4 1)
                     (il:branch 'beq (cadr labels))
                     (il:branch 'br (caddr labels))))
                   (else
                    (list
                     (il:comment "Switch on jump index")
                     (il 'ldarg 1)
                     (il 'switch (map label-name labels)))))))
          (il:comment "First (default) target")
          (intern-label as id)
          (il:label/header id))))

;; end-codevector-class : assembler -> void
;; Close a class declaration.
(define (end-codevector-class as)
  (if (as:current-codevector as)
      (let* ((user (as-user as))
             (current-codevector (as:current-codevector as))
             (cvclass (make-cvclass
                       (user-data.il-namespace user)
                       current-codevector
                       (reverse (as-code as))
                       '()
                       0)))
        (as-code! as cvclass)
        (as:basic-block-closed! as #t)
        (cvclass-label-count! cvclass (length (as:collect-local-labels as current-codevector))))))

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

;; Instruction implementations in pass5p2-instructions.sch

;; Operation implementations in pass5p2-ops.sch

;; "listify" code in pass5p2-listify.sch
