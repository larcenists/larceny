;; il-gen.sch
;; Procedures for emitting abstract representations of IL code.

;; ------------------------
;; IL abstraction catalogue
;; ------------------------

;; IL Abstract Representation

;; An ilpackage is
;; - (il code arg ...)
;; - (il:delay ilpackage ...)
;;     [[ alternatively, for a single delay:      ]]
;;     [[     (raw:make-il-delay (-> ilpackage))  ]]
;; - (list ilpackage ...)
;; - procedure : assembler -> ilpackage
;; - IL-ref
;; - string
;; 
;; An IL-ref is one of:
;; - IL-class
;; - IL-method
;; - IL-field
;; - IL-type
;; - IL-label
;;
;; An IL consumer is either
;; - an assembler structure (as)
;; - procedure : ilpackage -> void
;;
;; (see util-structs.sch)

;; [pnkfelix] IL-class, IL-type, IL-method, and IL-field represent
;; references in the IL code to classes, types, methods, and fields,
;; respectively.  Do not confuse them with clr-class, clr-method, and
;; [clr-]field (defined in dumpheap-il.sch), which are used to
;; represent DEFINITIONS of such entities.  IL-type was added later, to
;; represented structured references to constructed types like arrays.
;; Likewise, IL-label was added to allow preservation of the label
;; structure that was being turned into a string prematurely.
;; 
;; For example, a [clr-]field does not need a class argument, because
;; it is implicitly used in the context of some class definition,
;; while an IL-field needs a class argument, because field references
;; in the IL code need to know the class name as well as the field
;; name.
;;
;; (see util-structs.sch)

;; An IL-class is one of
;; - (make-il-class [Maybe String] [Maybe String] String)
;; - (make-il-class [Maybe String] [Listof String] String)

;; An IL-type is one of
;; - (make-il-primtype  String IL-class)
;; - (make-il-classtype IL-class)
;; - (make-il-arraytype IL-type)

;; il-type? : Any -> Bool
;; Returns non-false iff x is an IL-type.
(define (il-type? x)
  (or (il-primtype? x)
      (il-classtype? x)
      (and (il-arraytype? x)
	   (il-type? (il-arraytype.basetype x)))))
	       
;; An IL-method is a 
;;  (make-il-method Bool IL-type IL-class String [Listof IL-type])

;; An IL-label is one of
;;  (make-il-label (cons Number Number))
;;  (make-il-label Number)

;; Positive unary numbers are introduced by Twobit.  Negative unary
;; numbers are used for labels that are introduced by the compilation
;; of MacScheme to lower level code.
;; Ryan tried to explained to Felix what the pairs are for (JUMP
;; instruction?) but became unsure of himself mid-explanation, so
;; we'll avoid defining them formally.

(define (make-il-instance-method type class name argtypes)
  (make-il-method #t type class name argtypes))

(define (make-il-static-method type class name argtypes)
  (make-il-method #f type class name argtypes))

;; An IL-field is a
;;  (make-il-field IL-type IL-class String)

;; il : symbol arg ... -> ilpackage
(define (il code . args)
  (cond ((symbol? code)
         (raw:make-il code args))
        (else
         (error "procedure IL expects symbol as first arg, got " code
                "; other arguments were: " args))))

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
  (cond ((procedure? consumer) (consumer il))
        (else
         (as-code! consumer (cons il (as-code consumer))))))


;; =========================================================
;; SCHEME VALUES AND CONSTANTS

;; il:load-constant : datum -> ilpackage
;; IL to load the SchemeObject representation of a constant onto the stack.
(define (il:load-constant datum)
  (cond ((immediate-fixnum? datum)
         ;; Fixnum in preallocated fixnum pool
         (cond ((zero? datum)
                (il:ldsfld iltype-fixnum il-fixnum "zero"))
               ((= datum 1)
                (il:ldsfld iltype-fixnum il-fixnum "one"))
               ((= datum 2)
                (il:ldsfld iltype-fixnum il-fixnum "two"))
               ((= datum 3)
                (il:ldsfld iltype-fixnum il-fixnum "three"))
               ((= datum 4)
                (il:ldsfld iltype-fixnum il-fixnum "four"))
               (else (rep:fixnum-from-pool datum))))
        ((fixnum? datum)
         ;; General case: not in preallocated fixnum pool
         (list (il 'ldc.i4 datum)
               (rep:make-fixnum)))
        ((bignum? datum)
         (let ((pos? (>= datum 0))
               (2^16 #x10000))
           (let loop ((datum (abs datum)) (bigits '()))
             (if (zero? datum)
                 (list (il:load-int16-array
                        (reverse bigits))
                       (il 'ldc.i4 (if pos? 1 0))
                       (rep:make-bignum/data))
                 (loop (quotient datum 2^16)        ;; (rsha datum 16)
                       (cons (remainder datum 2^16) ;; (logand datum #xFFFF)
                             bigits))))))
        ((and (rational? datum) (exact? datum))
         (list (il:load-constant (numerator datum))
               (il:load-constant (denominator datum))
               (rep:make-ratnum)))
        ((flonum? datum)
         (list (il:load-real8 datum)
               (rep:make-flonum)))
        ((and (complex? datum) (exact? datum))
         (list (il:load-constant (real-part datum))
               (il:load-constant (imag-part datum))
               (rep:make-rectnum)))
        ((and (complex? datum) (inexact? datum))
         (list (il:load-real8 (real-part datum))
               (il:load-real8 (imag-part datum))
               (rep:make-compnum)))
        ((number? datum)
         (error "Missed a case in il:load-constant for ~s" datum))
        ((eq? datum #t)
         (il:ldsfld iltype-immediate il-schemefactory "True"))
        ((eq? datum #f)
         (il:ldsfld iltype-immediate il-schemefactory "False"))
        ((equal? datum (eof-object))
         (il:ldsfld iltype-immediate il-schemefactory "Eof"))
        ((equal? datum (unspecified))
         (il:ldsfld iltype-immediate il-schemefactory "Unspecified"))
        ((equal? datum (undefined))
         (il:ldsfld iltype-immediate il-schemefactory "Undefined"))
        ((null? datum)
         (il:ldsfld iltype-immediate il-schemefactory "Null"))
        ((immediate-char? datum) ;; Load from pool
         (rep:char-from-pool datum))
        ((char? datum) ;; General case
         (list (il 'ldc.i4 (char->integer datum))
               (rep:make-char)))
        ((pair? datum)
         (list
          ;; Builds CDR first so that long lists still take small space
          (il:load-constant (cdr datum))
          (il:load-constant (car datum))
          (rep:make-pair-reversed)))
        ((symbol? datum)
         (list
          (il:ldstr (symbol->string datum))
          (rep:make-interned-symbol)))
        ((string? datum)
         (list
          (il:ldstr datum)
          (rep:make-string)))
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
          (rep:make-vector)))
        (else
         (error "cannot emit IL to load constant: ~s~%" datum)
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
            ;; (il 'castclass iltype-procedure)
            (il:ldfld iltype-schemeobject-array il-procedure "constants")
            (il 'ldc.i4 index)
            (il 'ldelem.ref))))

;; =========================================================
;; IL MACROS

;; il:ldstr : string -> ilpackage
;; IL code to load a System.String onto the stack
(define (il:ldstr str0)
  (let* ((str1 (twobit-format #f "~s" str0))
         (strs (map (lambda (c)
                      (if (char=? c #\newline)
                          "\\n"
                          (string c)))
                    (string->list str1)))
         (str2 (apply string-append strs)))
    (il 'ldstr str2)))

;; il:ldfld : iltype ilclass string -> ilpackage
;; IL code to load from (instance) field
(define (il:ldfld type class field)
  (il 'ldfld (make-il-field type class field)))

;; il:stfld : iltype ilclass string -> ilpackage
;; IL code to store to (instance) field
(define (il:stfld type class field)
  (il 'stfld (make-il-field type class field)))

;; il:ldsfld : iltype ilclass string -> ilpackage
;; IL code to load from a static field
(define (il:ldsfld type class field)
  (il 'ldsfld (make-il-field type class field)))

;; il:stsfld : iltype ilclass string -> ilpackage
;; IL code to store to a static field
(define (il:stsfld type class field)
  (il 'stsfld (make-il-field type class field)))

;; il:load-real8 : real -> ilpackage
(define (il:load-real8 datum)
  (cond ((and (inexact? datum)
              (real? datum)
              (not (= datum datum)))
         (il:ldsfld iltype-double il-schemefactory "Nan"))
        ((equal? datum +inf.0)
         (il:ldsfld iltype-double il-schemefactory "PositiveInfinity"))
        ((equal? datum -inf.0)
         (il:ldsfld iltype-double il-schemefactory "NegativeInfinity"))
        (else
         (il 'ldc.r8 (exact->inexact datum)))))

;; il:load-int8 : integer -> ilpackage
(define (il:load-int8 datum)
  (il 'ldc.i8 (string-append "0x" (number->string datum 16))))

;; il:load-int16-array : (listof integer) -> ilpackage
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
  (let ((method-name (if (memq 'instance opts) 
			 make-il-instance-method 
			 make-il-static-method)))
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
                     iltype-code-address
                     il-codevector
                     "call"
                     (list iltype-int32))
            (list
             (il:call '()
                      iltype-code-address
                      il-call
                      "call"
                      (list iltype-codevector iltype-int32))
             (il 'ret)))))

;; il:comment : format-string value ... -> ilpackage
(define (il:comment format-string . args)
  (il 'comment (string-append "// " (apply twobit-format #f format-string args))))

(define (il:comment/info key arg)
  (il 'comment (string-append "//* " key (twobit-format #f " ~s" arg))))

(define (il:comment/wrap key . wrapped-il)
  (list
   (il 'comment (string-append "//( " key))
   wrapped-il
   (il 'comment "//)")))

;; il:directive : symbol value ... -> ilpackage
;; Represents a directive (.maxstack, .locals, ...)
;; One of: entrypoint maxstack module 
;;         assembly-extern assembly local line
(define (il:directive directive . args)
  (apply il 'directive directive args))

;; il:branch : symbol number -> ilpackage
;; Takes a branch code ('br, 'bgt, ...) and a destination label number.
(define (il:branch type target)
  (il type (make-il-label target)))

;; il:branch-s : symbol number -> ilpackage
;; Emits the short branch form of the given branch
(define (il:branch-s type num)
  (il (string->symbol (twobit-format #f "~a.s" type)) (make-il-label num)))

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
    pop sealed
    ))

;; il-field->string : IL-field -> string
(define (il-field->string fld)
  (let ((type (il-field.type fld))
	(class (il-field.class fld))
	(fieldname (il-field.name fld)))
    (string-append (il-type->string type)
		   " " (if (il-class? class)
			   (il-class->string class)
			   class)
		   "::" (il-quote-name fieldname))))

;; il-method->string : IL-method -> string
(define (il-method->string mthd)
  (let ((type (il-method.type mthd))
	(class (il-method.class mthd))
	(methodname (il-method.name mthd))
	(argtypes (il-method.argtypes mthd)))
    (let ((arglist
	   (apply string-append
		  (map/separated (lambda (x) (il-type->string x)) 
				 (lambda () ",") 
				 argtypes))))
      (string-append (if (il-method.instance? mthd)
			 "instance "
			 "")
		     (il-type->string type )
		     " " (if (il-class? class)
			     (il-class->string class) 
			     class)
		     "::" (il-quote-name methodname)
		     "(" arglist ")"))))

;; il-class->string : IL-class -> string
(define (il-class->string cls)
  (let ((assembly (il-class.assembly cls))
	(namespaces (il-class.namespaces cls))
	(classname (il-class.name cls)))
    (apply string-append
	   (append (if assembly
		       `("[" ,assembly "]")
		       '())
		   (map/separated il-quote-name
				  (lambda () ".")
				  (if (string? namespaces)
				      (list namespaces)
				      (or namespaces '())))
		   (list "." (il-quote-name classname))))))
;; il-type->string : IL-type -> string
(define (il-type->string typ)
  (cond ((il-primtype? typ)
	 (il-primtype.string typ))
	((il-classtype? typ) 
	 (il-classtype->string typ))
	((il-arraytype? typ) 
	 (string-append (il-type->string 
			 (il-arraytype.basetype typ))
			"[]"))
	(else (error 'il-type->string 
		     (twobit-format #f "Invalid argument: ~a" typ)))))

;; il-classtype->string : IL-classtype -> string
(define (il-classtype->string classname)
  (string-append 
   "class "
   (il-class->string (il-classtype.class classname))))

;; =========================================================
;; CODEVECTORS

;; il:load-codevector : string -> ilpackage
;; IL to load a codevector instance based on class name
(define (il:load-codevector cvname ns)
  (il:ldsfld iltype-codevector (make-il-class #f ns cvname) "instance"))

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
         (if (codegen-option 'special-reg-instructions)
             (il:call '()
                      (if (zero? register) iltype-procedure iltype-schemeobject)
                      il-reg
                      (if (zero? register) 
                          "get_ProcRegister0"
                          (string-append "get_Register" (number->string register)))
                      '())

             (il:ldsfld (if (zero? register) iltype-procedure iltype-schemeobject)
                        il-reg
                        (twobit-format #f "r~a" register))))
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
         (if (codegen-option 'special-setreg-instructions)
             (list ilpackage
                   (il:call '()
                            iltype-void
                            il-reg
                            (if (zero? register) "set_ProcRegister0" (string-append "set_Register" (number->string register)))
                            (list (if (zero? register) iltype-procedure iltype-schemeobject))))
             (list ilpackage
                   (il:stsfld
                    (if (zero? register) iltype-procedure iltype-schemeobject)
                    il-reg
                    (twobit-format #f "r~a" register)))))
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

(define (il:with-saved-result . il)
  (list (il:flush-result-cache)
        il
        (il:recache-result)))

;; il:recache-constant-vector : -> ilpackage
(define (il:recache-constant-vector)
  (if (codegen-option 'cache-constant-vector)
      (list
       (il:load-register ENV-REGISTER)
       ;; (il 'castclass iltype-procedure)
       (rep:procedure-constants)
       (il 'stloc LOCAL-CONSTANT-VECTOR))
      '()))

;; il:load-global-cell : number -> ilpackage
;; Loads a global cell (not its contents)
(define (il:load-global-cell index)
  (list (il:load-constant/vector index)
        ;; Avoid this cast, it is known that elements in the constant vector
        ;; are of type SObject, so use the virtual call.
        ;; (il 'castclass iltype-schemepair)
        ))

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
       (il:call '() iltype-code-address il-exn "fault"
                (list iltype-int32 iltype-string))
       (il 'ret))
      (il:fault excode)))

(define (il:fault excode)
  (list
   (il:flush-result-cache)
   (il 'ldc.i4 excode)
   ;; No tail call! See above.
   (il:call '() iltype-code-address il-exn "fault" (list iltype-int32))
   (il 'ret)))

;; Specific Faults

(define (il:fault/timer jump-index)
  (list
   (il:flush-result-cache)
   (il 'ldc.i4 jump-index)
   ;; No tail call! See above.
   (il:call '() iltype-void il-exn "faultTimer" (list iltype-int32))
   (il 'ret)))

(define (il:fault/invoke-nonproc argc)
  (list (il 'ldc.i4 argc)
        (il:call '() iltype-code-address il-exn "faultInvokeNonProc" (list iltype-int32))
        (il 'ret)))

(define (il:fault/apply-nonproc k1 k2)
  (list (il 'ldc.i4 k1)
        (il 'ldc.i4 k2)
        (il:call '() iltype-code-address il-exn "faultApplyNonProc"
                 (list iltype-int32 iltype-int32))
        (il 'ret)))

(define (il:fault/undef-global index)
  (list (il 'ldc.i4 index)
        (il:call '() iltype-code-address il-exn "faultGlobal" (list iltype-int32))
        (il 'ret)))

(define (il:fault/argc expectedc)
  (list (il 'ldc.i4 expectedc)
        (il:call '() iltype-code-address il-exn "faultArgCount" (list iltype-int32))
        (il 'ret)))

(define (il:fault/vargc expectedc)
  (list (il 'ldc.i4 expectedc)
        (il:call '() iltype-code-address il-exn "faultVarArgCount" (list iltype-int32))
        (il 'ret)))

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
      (list (rep:get-timer)
            (il 'ldc.i4 1)
            (il 'sub)
            (il 'dup)
            (rep:set-timer)
            (il:branch 'brtrue label-okay)
            (il:fault/timer jump-index)
            (if resume-label '() (il:label label-okay))))))

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
  (il 'label (make-il-label num)))

;; il:label/header : cvid -> ilpackage
;; Used for the first part of each codevector, with jump index 0.
(define (il:label/header id)
  (il 'label (make-il-label id)))

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

;; il-label->string : IL-label -> string
(define (il-label->string label)
  (let ((label (il-label.key label)))
    (cond ((pair? label)
	   (twobit-format #f "LABEL_HEADER_~a_~a" (car label) (cdr label)))
	  ((positive? label)
	   (twobit-format #f "LABEL_~a" label))
	  ((negative? label)
	   (twobit-format #f "L_~a" (- label)))
	  (else (error 'il-label->string ": bad label: " label)))))

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
        (rep:make-vector)))

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
