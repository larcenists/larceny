;; il-gen.sch
;; Procedures for emitting abstract representations of IL code.

;; ------------------------
;; IL abstraction catalogue
;; ------------------------

;; IL Abstract Representation

;; An ilpackage is 
;; - (il code arg ...)
;; - (il:delay ilpackage ...)
;; - (list ilpackage ...)
;; - procedure : assembler -> ilpackage
;; - string
;; 
;; An IL consumer is either
;; - an assembler structure (as)
;; - procedure : ilpackage -> void

(vector-struct $$il raw:make-il il?
               (il.code #f)
               (il.args #f))
(vector-struct $$il-delay raw:make-il-delay il-delay?
               (il-delay.il #f))

;; il : symbol arg ... -> ilpackage
(define (il code . args)
  (cond ((symbol? code)
         (raw:make-il code args))
        (else 
         (error "procedure IL expects symbol as first arg, got " code
                "; other arguments were: " args))))

;; il:delay SYNTAX
;; (il:delay expr ...)
;; Delays evaluation of each expr until patch-up time in assembler.
;; ASM ONLY
(define-syntax il:delay
  (syntax-rules ()
    ((_ il)
     (raw:make-il-delay (lambda () il)))
    ((_ il ...)
     (list
      (raw:make-il-delay (lambda () il))
      ...))))

;; il-delay-force : il-delay -> ilpackage
;; Forces a delayed ilpackage
(define (il-delay-force il-delay)
  ((il-delay.il il-delay)))

;; EMIT
;; ----

;; emit : consumer ilpackage ... -> void
;; Adds a representation of IL code to the current bytevector.
;; Representations include lists and promises which yield lists.
(define (emit consumer . ilpackages)
  (define (emit/h ilpackage)
    (cond ((il? ilpackage)
           (emit/il consumer ilpackage))
          ((il-delay? ilpackage)
           (emit/il consumer ilpackage))
          ((procedure? ilpackage)
           (emit/h (ilpackage consumer)))
          ((list? ilpackage)
           (for-each emit/h ilpackage))))
  (emit/h ilpackages))

;; emit/il : assembler|(ilpackage -> void) (union il il-delay) -> void
;; Emit a single IL representation to the consumer. Only IL structures and
;; IL-delay structures.
(define (emit/il consumer il)
  (cond ((procedure? consumer) 
         (consumer (pickle-il il)))
        (else
         (as-code! consumer (cons (pickle-il il) (as-code consumer))))))


;; =========================================================
;; SCHEME VALUES AND CONSTANTS

;; il:load-constant : datum -> ilpackage
;; IL to load the SchemeObject representation of a constant onto the stack.
(define (il:load-constant datum)
  (cond ((immediate-fixnum? datum) 
         ;; Fixnum in preallocated fixnum pool
         (list
          (il:comment "Fetching SFixnum(~s) from pool" datum)
          (il:ldsfld iltype-fixnum-array il-fixnum "pool")
          (il 'ldc.i4 (- datum FIXNUM-POOL-MIN))
          (il 'ldelem.ref)))
        ((fixnum? datum)
         ;; General case: not in preallocated fixnum pool
         (list (il 'ldc.i4 datum)
               (il:make-fixnum)))
;        ((bignum/ulong/sign? datum)
;         (list (il:load-int8 (abs datum))
;               (il 'ldc.i4 (if (negative? datum) 0 1))
;               (il:make-bignum/ulong/sign)))
        ((bignum? datum)
         (let ((pos? (>= datum 0)))
           (let loop ((datum (abs datum)) (bigits '()))
             (if (zero? datum)
                 (list (il:load-int16-array 
                        (reverse bigits))
                       (il 'ldc.i4 (if pos? 1 0))
                       (il:make-bignum/data))
                 (loop (rsha datum 16) 
                       (cons (logand datum #xFFFF) bigits))))))
        ((and (rational? datum) (exact? datum))
         (list (il:load-constant (numerator datum))
               (il:load-constant (denominator datum))
               (il:make-ratnum)))
        ((flonum? datum)
         (list (il:load-real8-as-int8 datum)
               (il:make-flonum-from-int8)))
        ((and (complex? datum) (exact? datum))
         (list (il:load-constant (real-part datum))
               (il:load-constant (imag-part datum))
               (il:make-rectnum)))
        ((and (complex? datum) (inexact? datum))
         (list (il:load-real8-as-int8 (real-part datum))
               (il:load-real8-as-int8 (imag-part datum))
               (il:make-compnum-from-int8)))
        ((number? datum)
         (error "Missed a case in il:load-constant for ~s" datum))
        ((eq? datum #t)
         (il:ldsfld iltype-immediate il-schemeobject "True"))
        ((eq? datum #f)
         (il:ldsfld iltype-immediate il-schemeobject "False"))
        ((equal? datum (eof-object))
         (il:ldsfld iltype-immediate il-schemeobject "Eof"))
        ((equal? datum (unspecified))
         (il:ldsfld iltype-immediate il-schemeobject "Unspecified"))
        ((equal? datum (undefined))
         (il:ldsfld iltype-immediate il-schemeobject "Undefined"))
        ((null? datum)
         (il:ldsfld iltype-immediate il-schemeobject "Null"))
        ((immediate-char? datum) ;; Load from pool
         (list (il:ldsfld iltype-schemechar-array il-schemechar "characters")
               (il 'ldc.i4 (char->integer datum))
               (il 'ldelem.ref)))
        ((char? datum) ;; General case
         (list (il 'ldc.i4 (char->integer datum))
               (il:make-char)))
        ((pair? datum)
         (list 
          ;; Builds CDR first so that long lists still take small space
          (il:load-constant (cdr datum))
          (il:load-constant (car datum))
          (il:make-pair-reversed)))
        ((symbol? datum)
         (list 
          (il:ldstr (symbol->string datum))
          (il:make-interned-symbol)))
        ((string? datum)
         (list
          (il:ldstr datum)
          (il:make-string)))
        ((vector? datum)
         (list
          (il 'ldc.i4 (vector-length datum))
          (il 'newarr iltype-schemeobject)
          (let loop ((index 0) (items (vector->list datum)))
            (cond ((null? items) '())
                  ((pair? items)
                   (list
                    (il 'dup)
                    (il 'ldc.i4 index)
                    (il:load-constant (car items))
                    (il 'stelem.ref)
                    (loop (+ 1 index) (cdr items))))))
          (il:make-vector)))
        (else
         (twobit-format (current-error-port)
                        "cannot emit IL to load constant: ~s~%" datum)
         (il:comment "WARNING: Placeholder for ~s" datum)
         (il:load-constant #f))))

;; il:load-constant/vector : number -> ilpackage
;; IL code to load a SchemeObject constant from the constant vector.
;; ASM ONLY
(define (il:load-constant/vector index)
  (if (codegen-option 'cache-constant-vector)
      (list (il 'ldloc LOCAL-CONSTANT-VECTOR)
            (il 'ldc.i4 index)
            (il 'ldelem.ref))
      (list (il:load-register ENV-REGISTER)
            (il 'castclass iltype-procedure)
            (il:ldfld iltype-schemeobject-array il-procedure "constants")
            (il 'ldc.i4 index)
            (il 'ldelem.ref))))

;; =========================================================
;; IL MACROS

;; il:ldstr : string -> ilpackage
;; IL code to load a System.String onto the stack
(define (il:ldstr string)
  (il 'ldstr (twobit-format #f "~s" string)))

;; il:ldfld : iltype ilclass string -> ilpackage
;; IL code to load from (instance) field
(define (il:ldfld type class field)
  (il 'ldfld (il-field type class field)))

;; il:ldfld : iltype ilclass string -> ilpackage
;; IL code to store to (instance) field
(define (il:stfld type class field)
  (il 'stfld (il-field type class field)))

;; il:ldsfld : iltype ilclass string -> ilpackage
;; IL code to load from a static field
(define (il:ldsfld type class field)
  (il 'ldsfld (il-field type class field)))

;; il:stsfld : iltype ilclass string -> ilpackage
;; IL code to store to a static field
(define (il:stsfld type class field)
  (il 'stsfld (il-field type class field)))

;; il:load-real8-as-int8 : real -> ilpackage
;; FIXME: Assumes same target as host endianness
(define (il:load-real8-as-int8 datum)
  (il:load-int8
   (integer-byte-string->integer
    (real->floating-point-byte-string datum 8) #f)))

;; il:load-int8 : integer -> ilpackage
(define (il:load-int8 datum)
  (il 'ldc.i8 (string-append "0x" (number->string datum 16))))

;; il:load-1nt16-array : (listof integer) -> ilpackage
(define (il:load-int16-array data)
  (list (il 'ldc.i4 (length data))
        (il 'newarr iltype-int16)
        (let loop ((data data) (i 0))
          (if (null? data)
              '()
              (list (il 'dup)
                    (il 'ldc.i4 i)
                    (il 'ldc.i4 (car data))
                    (il 'stelem.i2)
                    (loop (cdr data) (+ 1 i)))))))
              
;; il:call : (listof sym) iltype ilclass string (listof iltype) -> ilpackage
;; IL to call method; opts may include '(virtual instance tail). If 'tail 
;; option is given, generates 'ret automatically
(define (il:call opts type class method argtypes)
  (let ((method-name (if (memq 'instance opts) il-instance-method il-method)))
    (il:call-name opts (method-name type class method argtypes))))

;; il:call-name : (listof sym) string -> ilpackage
;; DEPRECATED for external use
(define (il:call-name opts method-name)
  (let ((call-code (cond ((memq 'new opts) 'newobj)
                         ((memq 'virtual opts) 'callvirt)
                         (else 'call)))
        (combine (if (memq 'tail opts)
                     (lambda (call-code call-name)
                       (list (if (codegen-option 'direct-tail-calls)
                                 (il 'tail.)
                                 '())
                             (il call-code call-name) 
                             (il 'ret)))
                     il)))
    (combine call-code method-name)))

;; il:call-scheme : -> ilpackage
;; Code for a scheme-to-scheme call. A codevector and jump index 
;; should be the only things on the stack. Doesn't return.
(define (il:call-scheme)
  (list (il:flush-result-cache)
        (if (codegen-option 'direct-tail-calls)
            (il:call '(virtual instance tail)
                     iltype-void
                     il-codevector
                     "call"
                     (list iltype-int32))
            (list
             (il:call '()
                      iltype-void
                      il-call
                      "call"
                      (list iltype-codevector iltype-int32))
             (il 'ret)))))

;; il:comment : format-string value ... -> ilpackage
(define (il:comment format-string . args)
  (il 'comment (string-append "// " (apply twobit-format #f format-string args))))

(define (il:comment/info key arg)
  (il 'comment (string-append "//* " key (format " ~s" arg))))

(define (il:comment/wrap key . wrapped-il)
  (list 
   (il 'comment (string-append "//( " key))
   wrapped-il
   (il 'comment "//)")))

;; il:directive : symbol value ... -> ilpackage
;; Represents a directive (.maxstack, .locals, ...)
(define (il:directive directive . args)
  (apply il 'directive directive args))

;; il:branch : symbol number -> ilpackage
;; Takes a branch code ('br, 'bgt, ...) and a destination label number.
(define (il:branch type target)
  (il type (label-name target)))

;; il:branch-s : symbol number -> ilpackage
;; Emits the short branch form of the given branch
(define (il:branch-s type num)
  (il (string->symbol (twobit-format #f "~a.s" type)) (label-name num)))

;; il:check-type : iltype ilpackage -> ilpackage
;; Generates IL to test the top stack element as a given type.
;; If it is, then control transfers after the check with the top of the stack
;; having been cast to the given type. If the test fails, then the failure 
;; code is executed with the stack having been popped.
;; NOTE: If code-if-fail does not escape (return/tail call...), then it must push
;; one item onto the stack (of the appropriate type).
(define (il:check-type iltype code-if-fail)
  (lambda (as)
    (let ((success-label (allocate-label as)))
      (list
       (il 'isinst iltype)
       (il 'dup)
       (il:branch-s 'brtrue success-label)
       (il 'pop)
       code-if-fail
       (il:label success-label)))))

;; il:check-bool : iltype ilpackage -> ilpackage
;; Generates IL which pops a bool off the stack, branches to failure
;; code if false, otherwise continues executing with next item in stack cast to
;; supplied iltype. Like above, failure code must either push appropriate type 
;; onto stack or escape.
(define (il:check-bool iltype code-if-fail)
  (lambda (as)
    (let ((success-label (allocate-label as)))
      (list
       (il:branch-s 'brtrue)
       (il 'pop)
       code-if-fail
       (il:label success-label)
       (il 'isinst iltype)))))


;; =========================================================
;; IL NAME CREATION

;; il-quote-name : string -> string
;; Quote names which are also IL keywords
(define (il-quote-name name)
  (cond ((memq (string->symbol name) il-keywords-must-quote)
         (twobit-format #f "'~a'" name))
        (else name)))

;; il-keywords-must-quote : (listof symbol)
;; If used as names, these must be quoted. (May be incomplete)
(define il-keywords-must-quote
  '(private public static instance 
    extends call class virtual cil managed fault value nested
    auto ansi beforefieldinit hidebysig specialname rtspecialname 
    pop 
    ))

;; il-field : string(type) string(class) string(fieldname) -> string
(define (il-field type class fieldname)
  (string-append type " " class "::" (il-quote-name fieldname)))

;; il-method : string string string (listof string) -> string
(define (il-method type class methodname argtypes)
  (let ((arglist
         (apply string-append
                (map/separated (lambda (x) x) (lambda () ",") argtypes))))
    (string-append type " " class "::" (il-quote-name methodname) 
                   "(" arglist ")")))

;; il-instance-method : string string string (listof string) ->  string
(define (il-instance-method type class methodname argtypes)
  (string-append "instance " (il-method type class methodname argtypes)))

;; il-class : string|#f string|#f|(listof string) string -> string
(define (il-class assembly namespaces classname)
  (apply string-append
         (append (if assembly `("[" ,assembly "]") '())
                 (map/separated il-quote-name
                                (lambda () ".")
                                (if (string? namespaces)
                                    (list namespaces)
                                    (or namespaces '())))
                 (list "." (il-quote-name classname)))))

;; il-class-type : string -> string
(define (il-class-type classname)
  (string-append "class " classname))

;; =========================================================
;; CODEVECTORS

;; il:load-codevector : string -> ilpackage
;; IL to load a codevector instance based on class name
(define (il:load-codevector cvname ns)
  (il:ldsfld iltype-codevector (il-class #f ns cvname) "instance"))

;; =========================================================
;; REGISTERS, STATIC AND DYNAMIC LINKS, GLOBALS

;; il:load-register : int|symbol -> ilpackage
;; IL to load a MacScheme virtual register
(define (il:load-register register)
  (cond ((and (codegen-option 'cache-result) (eq? register 'result))
         (il 'ldloc LOCAL-RESULT))
        ((assq register '((result "Result") 
                          (real-result "Result")
                          (second "Second")
                          (third "Third")))
         =>
         (lambda (regpair)
           (il:ldsfld iltype-schemeobject il-reg (cadr regpair))))
        ((number? register)
         (il:ldsfld iltype-schemeobject
                    il-reg
                    (twobit-format #f "register~a" register)))
        (error "il:load-register: cannot emit IL to load register: " 
               register)))

;; il:set-register : int|symbol ilpackage -> ilpackage
;; IL to store the value pushed by the given IL fragment into the specified
;; MacScheme virtual rgister
(define (il:set-register register ilpackage)
  (cond ((and (codegen-option 'cache-result) (eq? register 'result))
         (list ilpackage
               (il 'stloc LOCAL-RESULT)))
        ((assq register '((result "Result") 
                          (real-result "Result")
                          (second "Second")
                          (third "Third")))
         =>
         (lambda (regpair)
           (list ilpackage
                 (il:stsfld iltype-schemeobject il-reg (cadr regpair)))))
        ((number? register) 
         (list ilpackage
               (il:stsfld 
                iltype-schemeobject
                il-reg
                (twobit-format #f "register~a" register))))
        (else 
         (error 'unimplemented " cannot emit IL to set register: "
                register))))

;; il:set-register/pop : int|symbol -> ilpackage
;; Stores the top item on the stack into the specified register
;; NOTE: implementation assumes registers as static fields
(define (il:set-register/pop register)
  (il:set-register register '()))

;; If option 'cache-result is set, then we sometimes need to 
;; sync the local variable with the Reg field
;; The following do syncing automatically:
;;   il:call-scheme
;;   il:fault*

;; il:recache-result : -> ilpackage
(define (il:recache-result)
  (if (codegen-option 'cache-result)
      (il:set-register 'result (il:load-register 'real-result))
      '()))

;; il:flush-result-cache : -> ilpackage
(define (il:flush-result-cache)
  (if (codegen-option 'cache-result)
      (il:set-register 'real-result (il:load-register 'result))
      '()))

;; il:recache-constant-vector : -> ilpackage
(define (il:recache-constant-vector)
  (if (codegen-option 'cache-constant-vector)
      (list
       (il:load-register ENV-REGISTER)
       (il 'castclass iltype-procedure)
       (il:ldfld iltype-schemeobject-array il-procedure "constants")
       (il 'stloc LOCAL-CONSTANT-VECTOR))
      '()))


;; il:load-static-link : number -> ilpackage
;; IL to load the MacScheme Procedure the given number of levels up the 
;; static link chain.
(define (il:load-static-link up)
  (list
   (il:load-register ENV-REGISTER)
   (il 'castclass iltype-procedure)
   (let loop ((count up))
     (cond ((zero? count) '())
           (else (list (il 'ldfld (il-field iltype-schemeobject-array
                                            il-procedure
                                            "rib"))
                       (il 'ldc.i4 0)
                       (il 'ldelem.ref)
                       (il 'castclass iltype-procedure)
                       (loop (- count 1))))))))

;; il:load-rib : number -> ilpackage
;; IL to load the MacScheme rib (array) the given number of levels up 
;; the static link chain.
(define (il:load-rib up)
  (list (il:load-static-link up)
        (il 'ldfld (il-field iltype-schemeobject-array il-procedure "rib"))))

;; il:load-global-cell : number -> ilpackage
;; Loads a global cell (not its contents)
(define (il:load-global-cell index)
  (list (il:load-constant/vector index)
        (il 'castclass iltype-schemepair)))

;; =========================================================
;; EXCEPTIONS AND INTERRUPTS

(define (il:fault-abort excode)
   (il:fault excode))

(define (il:fault-abort/message excode message)
  (if (codegen-option 'fault-error-messages)
      (list 
       (il:flush-result-cache)
       (il 'ldc.i4 excode)
       (il:ldstr message)
       ;; We don't want to make this a tail call; we 
       ;; want to keep it on the stack.
       (il:call '() iltype-void il-exn "fault" 
                (list iltype-int32 iltype-string))
       (il 'ret))
      (il:fault excode)))

(define (il:fault excode)
  (list 
   (il:flush-result-cache)
   (il 'ldc.i4 excode)
   ;; No tail call! See above.
   (il:call '() iltype-void il-exn "fault" (list iltype-int32))
   (il 'ret)))

(define (il:fault/timer jump-index)
  (list 
   (il:flush-result-cache)
   (il 'ldc.i4 jump-index)
   ;; No tail call! See above.
   (il:call '() iltype-void il-exn "faultTimer" (list iltype-int32))
   (il 'ret)))

(define (il:set-implicit-continuation label)
  (lambda (as)
    (list (il 'ldc.i4 (intern-label as label))
          (il:stsfld iltype-int32 il-reg "implicitContinuation"))))

(define (il:reset-implicit-continuation)
  (list (il 'ldc.i4 -1)
        (il:stsfld iltype-int32 il-reg "implicitContinuation")))

;; =========================================================
;; FUEL (BRANCH / CALL)

(define (il:use-fuel/call)
  (if (codegen-option 'insert-use-fuel)
      (il:use-fuel FIRST-JUMP-INDEX #f)
      '()))

(define (il:br/use-fuel target)
  (if (codegen-option 'insert-use-fuel)
      (lambda (as) (il:use-fuel (intern-label as target) target))
      (il:branch 'br target)))

;; il:use-fuel : number Label|#f -> ilpackage
(define (il:use-fuel jump-index resume-label)
  (lambda (as)
    (let ((label-okay (or resume-label (allocate-label as))))
      (list (il:ldsfld iltype-int32 il-reg "timer")
            (il 'ldc.i4 1)
            (il 'sub)
            (il 'dup)
            (il:stsfld iltype-int32 il-reg "timer")
            (il:branch 'brtrue label-okay)
            (il:fault/timer jump-index)
            (if resume-label '() (il:label label-okay))))))

;; =========================================================
;; CONTINUATIONS

(define (il:load-current-frame)
  (il:ldsfld iltype-cache-frame il-cont "cont"))

(define (il:load-frame-slot slot)
  (if (< slot CONTINUATION-FRAME-SLOTS)
      (list
       (il:ldfld iltype-schemeobject il-continuation-frame 
                 (twobit-format #f "slot~s" slot)))
      (list 
       (il:ldfld iltype-schemeobject-array il-continuation-frame "overflowSlots")
       (il 'ldc.i4 (- slot CONTINUATION-FRAME-SLOTS))
       (il 'ldelem.ref))))

(define (il:set-frame-slot slot ilpackage)
  (if (< slot CONTINUATION-FRAME-SLOTS)
      (list
       ilpackage
       (il:stfld iltype-schemeobject il-continuation-frame 
                 (twobit-format #f "slot~s" slot)))
      (list
       (il:ldfld iltype-schemeobject-array il-continuation-frame "overflowSlots")
       (il 'ldc.i4 (- slot CONTINUATION-FRAME-SLOTS))
       ilpackage
       (il 'stelem.ref))))

(define (il:load-current-frame-slot slot)
  (list (il:load-current-frame)
        (il:load-frame-slot slot)))

(define (il:set-current-frame-slot slot ilpackage)
  (list (il:load-current-frame)
        (il:set-frame-slot slot ilpackage)))

;; =========================================================
;; LOCAL VARIABLES

;; with-il-locals : assembler (listof string) (string ... -> 'a) -> 'a
(define (with-il-locals as types consumer)
  (let ((variables (map (lambda (t) (allocate-il-local as t)) types)))
    (let ((result (apply consumer variables)))
      (for-each (lambda (v) (deallocate-il-local as v)) variables)
      result)))

;; allocate-il-local : assembler iltype -> num
(define (allocate-il-local as type)
  (let loop ((n 0) (vars (as:local-variables as)))
    (cond ((null? vars)
           (as:local-variables! as
                                (append (as:local-variables as)
                                        (list (cons type #f))))
           n)
          ((and (equal? (caar vars) type) (cdar vars))
           (set-cdr! (car vars) #f)
           n)
          (else (loop (+ n 1) (cdr vars))))))

;; deallocate-il-local : assembler number -> void
(define (deallocate-il-local as n)
  (let loop ((n n) (vars (as:local-variables as)))
    (cond ((zero? n)
           (set-cdr! (car vars) #t))
          (else (loop (- n 1) (cdr vars))))))

;; =========================================================
;; LABELS

;; il:label : number -> ilpackage
;; Represents an IL label based on a number
(define (il:label num)
  (il 'label (label-name num)))

;; il:label/header : cvid -> ilpackage
;; Used for the first part of each codevector, with jump index 0.
(define (il:label/header id)
  (il 'label (twobit-format #f "LABEL_HEADER_~a_~a" (car id) (cdr id))))

;; intern-label : as number|cvid -> number
(define (intern-label as label)
  (let* ((user (as-user as))
         (id (as:current-codevector as))
         (label-map (gvector-ref (user-data.label-map user) (car id))))
    (cond ((assoc label label-map)
           => (lambda (p) (cadr p)))
          (else
           (let ((jump-index (as:next-jump-index as)))
             (as:next-jump-index! as (+ 1 jump-index))
             (gvector-set! 
              (user-data.label-map user)
              (car id)
              (cons (cons label (cons jump-index (as:current-codevector as)))
                    label-map))
             jump-index)))))

;; as:collect-local-lables : as cvid -> number
(define (as:collect-local-labels as id)
  (let* ((label-map (gvector-ref (user-data.label-map (as-user as)) (car id)))
         (label-map/id (filter (lambda (p) (equal? id (cddr p))) label-map))
         (labels (map (lambda (p) (cons (car p) (cadr p))) label-map/id)))
    (map car (compat:sort labels (lambda (a b) (<= (cdr a) (cdr b)))))))

;; as:label->index : assembler num -> num
(define (as:label->index as label)
  (let ((user (as-user as))
        (id (as:current-codevector as)))
    (cond ((assoc label (gvector-ref (user-data.label-map user) (car id)))
           => cadr)
          (else
           (error 'foreign-label->index " label " label " not found in "
                  (user-data.label-map user))))))

;; label-name : number|cvid -> string
(define (label-name label)
  (cond ((pair? label)
         (twobit-format #f "LABEL_HEADER_~a_~a" (car label) (cdr label)))
        ((positive? label)
         (twobit-format #f "LABEL_~a" label))
        ((negative? label)
         (twobit-format #f "L_~a" (- label)))
        (else (error 'label-name ": bad label: " label))))

;; =========================================================
;; DUMPHEAP

;; il:loader-name : number -> string
(define (il:loader-name n)
  (twobit-format #f "Loader_~a" (+ 1 n)))

;; il:constant-vector-max-stack : constant-vector -> number
(define (il:constant-vector-max-stack constants)
  (apply max
         (map (lambda (constant)
                (case (car constant)
                  ((global) 4)  ;; by inspection of IL code below
                  ((constantvector)
                   (+ 2 (il:constant-vector-max-stack (cadr constant))))
                  (else 2)))
              (vector->list constants))))

;; il:constant-max-stack : constant -> number
(define (il:constant-max-stack constant)
  (cond ((pair? constant)
         (+ 1 (max (il:constant-max-stack (car constant))
                   (il:constant-max-stack (cdr constant)))))
        ((vector? constant)
         (+ 2 (apply max (cons 1 (map il:constant-max-stack
                                      (vector->list constant))))))
        (else 4)))

;; il:constant-vector : constant-vector -> ilpackage
(define (il:constant-vector constants)
  (list (il 'ldc.i4 (length constants))
        (il 'newarr iltype-schemeobject)
        (il:fill-ref-array constants il:resolve-constant)
        (il:make-vector)))

;; il:resolve-constant : tagged-constant -> ilpackage
(define (il:resolve-constant constant)
  (case (car constant)
    ((data doc)
     (il:load-constant (cadr constant)))
    ((global)
     (list (il:ldstr (symbol->string (cadr constant)))
           (il:call '() iltype-schemepair il-reg "globalCell" 
                    (list iltype-string))))
    ((constantvector)
     (il:constant-vector (vector->list (cadr constant))))
    ((codevector)
     (il:load-constant #f))
    (else (error "not a proper constant: " constant))))

(define (il:fill-X-array store-command)
  (lambda (items f)
    (let loop ((index 0) (items items))
      (cond 
        ((null? items) '())
        ((pair? items)
         (list (il 'dup)
               (il 'ldc.i4 index)
               (f (car items))
               store-command
               (loop (+ 1 index) (cdr items))))))))
(define il:fill-ref-array (il:fill-X-array (il 'stelem.ref)))

