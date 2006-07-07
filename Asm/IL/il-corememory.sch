;; This file uses javadot notation and only works when run atop a
;; Common Larceny runtime.
;; FSK: this is changing, so take out the expensive call.
'(enable-dotnet!)
;; FSK: Howvever, since I am developing, lets turn on debugging...
(load "Debugger/debug.sch")
(load "Debugger/inspect-cont.sch")
(load "Debugger/trace.sch")
(install-debugger)

(define (default-assembly-basename)
  "larsembly")

;; current-assembly-basename : [Param String]
(define current-assembly-basename (make-parameter "current-assembly-basename" (default-assembly-basename)))

;; current-assembly-name : [Param System.Reflection.AssemblyName]
(define current-assembly-name (make-parameter "current-assembly-name" #f))

;; current-domain : [Param System.AppDomain]
(define current-domain        (make-parameter "current-domain" #f))

;; current-assembly-builder : [Param System.Reflection.Emit.AssemblyBuilder]
(define current-assembly-builder 
  (make-parameter "current-assembly-builder" #f))

;; current-module-builder : [Param System.Reflection.Emit.ModuleBuilder]
(define current-module-builder    
  (make-parameter "current-module-builder" #f))

;; current-il-namespace : [Param String]
(define current-il-namespace  (make-parameter "current-il-namespace" #f))

;; current-type-builder : [Param System.Reflection.Emit.TypeBuilder]
(define current-type-builder  (make-parameter "current-type-builder" #f))

;; current-method-builder : 
;;   [Param [Oneof System.Reflection.Emit.MethodBuilder 
;;                 System.Reflection.Emit.ConstructorBuilder]]
(define current-method-builder (make-parameter "current-method-builder" #f))

;; current-il-generator : [Param System.Reflection.Emit.ILGenerator]
(define current-il-generator  (make-parameter "current-il-generator" #f))

;; current-registered-class-table : [Map (list CanonNS String) TypeBuilder]
(define current-registered-class-table
  (make-parameter "current-registered-class-table" '()))

;; current-registered-superclass-table : [Map TypeBuilder Type]
(define current-registered-superclass-table
  (make-parameter "current-registered-superclass-table" '()))

;; current-registered-field-table : [Map (list Type String) FieldInfo]
(define current-registered-field-table
  (make-parameter "current-registered-field-table" '()))

;; current-registered-method-table : [Map (list Type String [Vectorof Type]) [Oneof MethodBuilder ConstructorBuilder]]
(define current-registered-method-table
  (make-parameter "current-registered-method-table" '()))

;; current-saved-manifests : [Listof PseudoManifest]
;; Stored in *reverse* order!
(define current-saved-manifests (make-parameter "current-saved-manifests" '()))

;; Resets the parameters which don't require any input state for their
;; default value.
(define (reset-valueless-parameters!)
  (current-registered-class-table '())
  (current-registered-superclass-table '())
  (current-registered-field-table '())
  (current-registered-method-table '())
  (current-saved-manifests '()))

;; Similar to with-fresh-dynamic-assembly-setup, except the names are
;; all derived from the single basename
(define (with-simple-fresh-dynamic-assembly-setup basename thunk)
  (with-fresh-dynamic-assembly-setup
   (string-append basename "-assembly")
   (string-append basename "-module")
   (string-append basename ".dll")
   ""
   thunk))

(define clr-type-handle/system-reflection-assemblyname
  (clr/%get-type "System.Reflection.AssemblyName"))
(define clr-type-handle/system-threading-thread
  (clr/%get-type "System.Threading.Thread"))
(define clr-type-handle/system-reflection-emit-assemblybuilder
  (clr/%get-type "System.Reflection.Emit.AssemblyBuilder"))
(define clr-type-handle/system-reflection-emit-assemblybuilderaccess
  (clr/%get-type "System.Reflection.Emit.AssemblyBuilderAccess"))
(define clr-type-handle/system-reflection-emit-modulebuilder
  (clr/%get-type "System.Reflection.Emit.ModuleBuilder"))
(define clr-type-handle/system-reflection-typeattributes
  (clr/%get-type "System.Reflection.TypeAttributes"))
(define clr-type-handle/system-reflection-emit-ilgenerator
  (clr/%get-type "System.Reflection.Emit.ILGenerator"))
(define clr-type-handle/system-reflection-emit-opcode
  (clr/%get-type "System.Reflection.Emit.OpCode"))
(define clr-type-handle/system-reflection-emit-label
  (clr/%get-type "System.Reflection.Emit.Label"))
(define clr-type-handle/system-reflection-fieldinfo
  (clr/%get-type "System.Reflection.FieldInfo"))
(define clr-type-handle/system-reflection-emit-localbuilder
  (clr/%get-type "System.Reflection.Emit.LocalBuilder"))
(define clr-type-handle/system-type-array
  (clr/%get-type "System.Type[]"))
(define clr-type-handle/system-reflection-emit-label
  (clr/%get-type "System.Reflection.Emit.Label"))
(define clr-type-handle/system-reflection-emit-label-array
  (clr/%get-type "System.Reflection.Emit.Label[]"))
(define clr-type-handle/system-reflection-constructorinfo
  (clr/%get-type "System.Reflection.ConstructorInfo"))
(define clr-type-handle/system-reflection-methodinfo
  (clr/%get-type "System.Reflection.MethodInfo"))
(define clr-type-handle/system-reflection-emit-typebuilder
  (clr/%get-type "System.Reflection.Emit.TypeBuilder"))
(define clr-type-handle/system-reflection-fieldattributes
  (clr/%get-type "System.Reflection.FieldAttributes"))
(define clr-type-handle/system-reflection-methodattributes
  (clr/%get-type "System.Reflection.MethodAttributes"))
(define clr-type-handle/system-reflection-callingconventions
  (clr/%get-type "System.Reflection.CallingConventions"))
(define clr-type-handle/scheme-rt-load
  (clr/%get-type "Scheme.RT.Load"))

(define-syntax define-traced
  (syntax-rules ()
    ((define-traced (NAME ARGS ...) BODY ...)
     (define (NAME ARGS ...)
       (begin (write (quasiquote (NAME (unquote ARGS) ...))) 
              (newline))
       (let ((ret-val (begin BODY ...)))
         (begin (write (quasiquote (end NAME (unquote ARGS) ...))) 
                (newline))
         ret-val)))))

(define-syntax define-ilc
  (syntax-rules ()
    ;((define-ilc ARGS ...) (define-traced ARGS ...))
    ((define-ilc ARGS ...) (define ARGS ...))
    ))

(define-ilc (ilc/%make-assembly-name)
  (let* ((type-recv clr-type-handle/system-reflection-assemblyname)
         (ctor (clr/%get-constructor type-recv '#())))
    (clr/%invoke-constructor ctor '#())))

(define-ilc (ilc/%thread-get-domain)
  (let* ((type-recv clr-type-handle/system-threading-thread)
         (meth (clr/%get-method type-recv "GetDomain" '#())))
    (clr/%invoke meth #f '#())))

(define-ilc (ilc/%define-dynamic-assembly dom asm-name perms)
  (let* ((type-recv clr-type-handle/system-appdomain)
         (type-arg1 clr-type-handle/system-reflection-assemblyname)
         (type-arg2 clr-type-handle/system-reflection-emit-assemblybuilderaccess)
         (meth (clr/%get-method type-recv "DefineDynamicAssembly" 
                                (vector type-arg1 type-arg2))))
    (clr/%invoke meth dom (vector asm-name perms))))

(define-ilc (ilc/%define-dynamic-assembly/storage dom asm-name perms storage-dir)
  (let* ((type-recv clr-type-handle/system-appdomain)
         (type-arg1 clr-type-handle/system-reflection-assemblyname)
         (type-arg2 clr-type-handle/system-reflection-emit-assemblybuilderaccess)
         (type-arg3 clr-type-handle/system-string)
         (meth (clr/%get-method type-recv "DefineDynamicAssembly" 
                                (vector type-arg1 type-arg2 type-arg3))))
    (clr/%invoke meth dom (vector asm-name perms storage-dir))))

(define-ilc (ilc/%define-dynamic-module asm-bldr mod-name dll-name)
  (let* ((type-recv clr-type-handle/system-reflection-emit-assemblybuilder)
         (type-arg1 clr-type-handle/system-string)
         (type-arg2 clr-type-handle/system-string)
         (meth (clr/%get-method type-recv "DefineDynamicModule" 
                                (vector type-arg1 type-arg2))))
    (clr/%invoke meth asm-bldr (vector mod-name dll-name))))

(define-ilc (ilc/%run-and-save-permissions)
  (let* ((type-recv clr-type-handle/system-reflection-emit-assemblybuilderaccess)
         (info (clr/%get-field type-recv "RunAndSave")))
    (clr/%field-ref info)))

(define-ilc (ilc/%set-assembly-name-name! asm-name string)
  (let* ((type-recv clr-type-handle/system-reflection-assemblyname)
         (prop-info (clr/%get-property type-recv "Name" '#())))
    (clr/%property-set! prop-info asm-name (clr/%string->foreign string) '#())))

(define-ilc (ilc/%define-type! mod-bldr name type-attributes parent)
  (let* ((type-recv clr-type-handle/system-reflection-emit-modulebuilder)
         (type-arg1 clr-type-handle/system-string)
         (type-arg2 clr-type-handle/system-reflection-typeattributes)
         (type-arg3 clr-type-handle/system-type)
         (meth (clr/%get-method type-recv "DefineType"
                                (vector type-arg1 type-arg2 type-arg3))))
    (clr/%invoke meth mod-bldr (vector name type-attributes parent))))

(define-ilc (ilc/%define-label! ilgen)
  (let* ((type-recv clr-type-handle/system-reflection-emit-ilgenerator)
         (meth (clr/%get-method type-recv "DefineLabel" (vector))))
    (clr/%invoke meth ilgen (vector))))

(define-ilc (ilc/%set-entry-point! asm-name meth-bldr)
  (let* ((type-recv clr-type-handle/system-reflection-assemblyname)
         (type-arg1 clr-type-handle/system-reflection-emit-methodbuilder)
         (meth (clr/%get-method type-recv "SetEntryPoint" (vector type-arg1))))
    (clr/%invoke meth asm-name (vector meth-bldr))))

(define-ilc (ilc/%declare-local! ilgen type)
  (let* ((type-recv clr-type-handle/system-reflection-emit-ilgenerator)
         (type-arg1 clr-type-handle/system-type)
         (meth (clr/%get-method type-recv "DeclareLocal" (vector type-arg1))))
    (clr/%invoke meth ilgen (vector type))))

(define-syntax define-emit-form 
  (syntax-rules ()
    ((define-emit-form NAME (ARG-ID ARG-TYPE) ...)
     (define NAME
       (let* ((type-recv clr-type-handle/system-reflection-emit-ilgenerator)
              (type-arg1 clr-type-handle/system-reflection-emit-opcode) 
              (meth (clr/%get-method type-recv "Emit" (vector type-arg1 ARG-TYPE ...))))
         (define-ilc (NAME ilgen opc ARG-ID ...)
           (clr/%invoke meth ilgen (vector opc ARG-ID ...)))
         NAME)))))

(define-emit-form ilc/%emit)
(define-emit-form ilc/%emit/label 
  (label clr-type-handle/system-reflection-emit-label))
(define-emit-form ilc/%emit/short
  (short clr-type-handle/system-uint16))
(define-emit-form ilc/%emit/int
  (int clr-type-handle/system-int32))
(define-emit-form ilc/%emit/long
  (long clr-type-handle/system-int64))
(define-emit-form ilc/%emit/double
  (double clr-type-handle/system-double))
(define-emit-form ilc/%emit/field-info
  (field-info clr-type-handle/system-reflection-fieldinfo))
(define-emit-form ilc/%emit/local-builder
  (local-bldr clr-type-handle/system-reflection-emit-localbuilder))
(define-emit-form ilc/%emit/string
  (string clr-type-handle/system-string))
(define-emit-form ilc/%emit/type
  (type clr-type-handle/system-type))
(define-emit-form ilc/%emit/label-array
  (labels clr-type-handle/system-reflection-emit-label-array))
(define-emit-form ilc/%emit/constructor-info
  (ctor-info clr-type-handle/system-reflection-constructorinfo))
(define-emit-form ilc/%emit/method-info
  (meth-info clr-type-handle/system-reflection-methodinfo))
(define-emit-form ilc/%emit/method-info-type-array
  (meth-info clr-type-handle/system-reflection-methodinfo)
  (types     clr-type-handle/system-type-array))

(define-ilc (ilc/%mark-label ilgen label)
  (let* ((type-recv clr-type-handle/system-reflection-emit-ilgenerator)
         (type-arg1 clr-type-handle/system-reflection-emit-label)
         (meth (clr/%get-method type-recv "MarkLabel" (vector type-arg1))))
    (clr/%invoke meth ilgen (vector label))))

(define-ilc (ilc/%define-field type-bldr name cls field-attrs)
  (let* ((type-recv clr-type-handle/system-reflection-emit-typebuilder)
         (type-arg1 clr-type-handle/system-string)
         (type-arg2 clr-type-handle/system-type)
         (type-arg3 clr-type-handle/system-reflection-fieldattributes)
         (meth (clr/%get-method type-recv "DefineField" 
                                (vector type-arg1 type-arg2 type-arg3))))
    (clr/%invoke meth type-bldr (vector name cls field-attrs))))

(define-ilc (ilc/%define-constructor type-bldr meth-attrs calling-conv arg-infos)
  (let* ((type-recv clr-type-handle/system-reflection-emit-typebuilder)
         (type-arg1 clr-type-handle/system-reflection-methodattributes)
         (type-arg2 clr-type-handle/system-reflection-callingconventions)
         (type-arg3 clr-type-handle/system-type-array)
         (meth (clr/%get-method type-recv "DefineConstructor" 
                                (vector type-arg1 type-arg2 type-arg3))))
    (clr/%invoke meth type-bldr (vector meth-attrs calling-conv arg-infos))))

(define-ilc (ilc/%standard-calling-conventions)
  (clr/%field-ref (clr/%get-field 
                   clr-type-handle/system-reflection-callingconventions
                   "Standard")
                  #f))

(define-ilc (ilc/%define-type-initializer type-bldr)
  (let* ((type-recv clr-type-handle/system-reflection-emit-typebuilder)
         (meth (clr/%get-method type-recv "DefineTypeInitializer" '#())))
    (clr/%invoke meth type-bldr '#())))

(define-ilc (ilc/%define-method type-bldr name meth-attrs ret-type arg-types)
  (let* ((type-recv clr-type-handle/system-reflection-emit-typebuilder)
         (type-arg1 clr-type-handle/system-string)
         (type-arg2 clr-type-handle/system-reflection-methodattributes)
         (type-arg3 clr-type-handle/system-type)
         (type-arg4 clr-type-handle/system-type-array)
         (meth (clr/%get-method type-recv "DefineMethod" 
                                (vector type-arg1 type-arg2 
                                        type-arg3 type-arg4))))
    (clr/%invoke meth type-bldr (vector name meth-attrs ret-type arg-types))))

(define-ilc (ilc/%create-type type-bldr)
  (let* ((type-recv clr-type-handle/system-reflection-emit-typebuilder)
         (meth (clr/%get-method type-recv "CreateType" '#())))
    (clr/%invoke meth type-bldr '#())))

(define-ilc (ilc/%get-ilgenerator/meth meth-bldr)
  (let* ((type-recv clr-type-handle/system-reflection-emit-methodbuilder)
         (meth (clr/%get-method type-recv "GetILGenerator" '#())))
    (clr/%invoke meth meth-bldr '#())))

(define-ilc (ilc/%get-ilgenerator/ctor ctor-bldr)
  (let* ((type-recv clr-type-handle/system-reflection-emit-constructorbuilder)
         (meth (clr/%get-method type-recv "GetILGenerator" '#())))
    (clr/%invoke meth ctor-bldr '#())))

(define-ilc (ilc/%get-ilgenerator bldr)
  (cond ((clr/%isa? bldr clr-type-handle/system-reflection-emit-constructorbuilder)
         (ilc/%get-ilgenerator/ctor bldr))
        ((clr/%isa? bldr clr-type-handle/system-reflection-emit-methodbuilder)
         (ilc/%get-ilgenerator/meth bldr))
        (else (error 'ilc/%get-ilgenerator))))

(define-ilc (ilc/%find-code-in-assembly asm-bldr il-ns segnum)
  ;; static method!
  (let* ((type-recv clr-type-handle/scheme-rt-load)
         (type-arg0 clr-type-handle/system-reflection-emit-assemblybuilder)
         (type-arg1 clr-type-handle/system-string)
         (type-arg2 clr-type-handle/system-int32)
         (meth (clr/%get-method type-recv "findCodeInAssembly" 
                                (vector type-arg0 type-arg1 type-arg2))))
    (clr/%invoke meth #f (vector asm-bldr il-ns segnum))))

(define-ilc (ilc/%save asm-bldr dll-name)
  (let* ((type-recv clr-type-handle/system-reflection-emit-assemblybuilder)
         (type-arg1 clr-type-handle/system-string)
         (meth (clr/%get-method type-recv "Save" (vector type-arg1))))
    (clr/%invoke meth asm-bldr (vector dll-name))))

;; with-fresh-dynamic-assembly-setup : String String String String (-> X) -> X
;; Sets up assembly-name, domain, asm-builder, and module.
;; Does *not* set up type-builder.
(define (with-fresh-dynamic-assembly-setup assembly-name 
					   module-name dll-file-name 
					   storage-directory
					   thunk)
  (let ((my-asm-name (ilc/%make-assembly-name))) 
    (if assembly-name 
        (ilc/%set-assembly-name-name! my-asm-name assembly-name))
    (parameterize ((current-assembly-name my-asm-name))
      (parameterize ((current-domain (ilc/%thread-get-domain)))
	(parameterize ((current-assembly-builder 
			((lambda (dom anm perms dir)
                           (if (string=? "" dir)
                               (ilc/%define-dynamic-assembly dom anm perms)
                               (ilc/%define-dynamic-assembly/storage dom anm perms dir)))
			 (current-domain) 
                         (current-assembly-name)
			 ;; below permissions are liberal; might allow
			 ;; developer to select Run or Save alone
			 (ilc/%run-and-save-permissions)
                         storage-directory)))
	  (parameterize ((current-module-builder
			  (ilc/%define-dynamic-module
			   (current-assembly-builder) 
                           (clr/%string->foreign module-name)
                           (clr/%string->foreign dll-file-name))))
	    (parameterize ((current-registered-class-table '())
			   (current-registered-superclass-table '())
			   (current-registered-field-table '())
			   (current-registered-method-table '())
			   (current-label-intern-table '())
			   (current-saved-manifests '()))
	      (thunk)
	      )))))))

;; Not really as safe to use as with-fresh-dynamic-assembly-setup,
;; but more convenient at times.
(define (fresh-dynamic-assembly-setup! 
	 assembly-name module-name dll-file-name)
  (let ((my-asm-name (ilc/%make-assembly-name)))

    (if assembly-name 
        (ilc/%set-assembly-name-name! my-asm-name assembly-name))
    (current-assembly-name my-asm-name)
    (current-domain (ilc/%thread-get-domain))
    (current-assembly-builder 
     (ilc/%define-dynamic-assembly 
                             (current-domain) (current-assembly-name)
			     ;; below permissions are liberal; might allow
			     ;; developer to select Run or Save alone
			     (ilc/%run-and-save-permissions)))
    (current-module-builder (ilc/%define-dynamic-module
			     (current-assembly-builder) 
                             (clr/%string->foreign module-name)
                             (clr/%string->foreign dll-file-name)))
    (reset-valueless-parameters!)
    ))

;; Resetting default values of parameters above to something
;; reasonable.
(let ((basename (default-assembly-basename)))
  (fresh-dynamic-assembly-setup!
   (string-append basename "-assembly")
   (string-append basename "-module")
   (string-append basename ".dll")))

(define (fresh-type-setup! name)
  (current-type-builder (ilc/%define-type!
                         (current-module-builder)
                         (clr/%string->foreign name)
                         (clr/int->foreign 0)
                         clr-type-handle/system-object)))

;; Members of TypeAttributes enumeration:
;; AnsiClass AutoClass UnicodeClass
;; Abstract AutoLayout BeforeFieldInit Class ClassSemanticsMask
;; ExplicitLayout HasSecurity Import Interface LayoutMask
;; NestedAssembly NestedFamORAssem NestedPrivate NestedPublic
;; NotPublic Public ReservedMask RTSpecialName Sealed SequentialLayout
;; Serializable SpecialName StringFormatMask VisibilityMask

;; Members of MethodAttributes enumeration: 
;; Abstract Assembly CheckAccessOnOverride FamANDAssem Family
;; FamORAssem Final HasSecurity HideBySig MemberAccessMask NewSlot
;; PinvokeImpl Private PrivateScope Public RequireSecObject
;; ReservedMask ReuseSlot RTSpecialName SpecialName Static
;; UnmanagedExport Virtual VtableLayoutMask

;; Members of FieldAttributes enumeration:
;; Assembly FamANDAssem Family FamORAssem FieldAccessMask HasDefault
;; HasFieldMarshal HasFieldRVA InitOnly Literal NotSerialized
;; PinvokeImpl Private PrivateScope Public ReservedMask RTSpecialName
;; SpecialName Static

(define-values (option->type-attribute 
		option->method-attribute 
		option->field-attribute 
		il:code->opcode)
  (letrec-syntax 
      ;; maps each list elem to its canonical rep.
      ;; (canon-case 'exp-context 'y ((a b c) (x y z))) ==> x
      ((canon-case 
	(syntax-rules ()
	  ((_ CTXT X (TAG REST ...) ...)
	   (let ((obj X))
	     (case obj
	       ((TAG REST ...) (quote TAG)) ...
	       (else (error CTXT
			    (twobit-format #f "Unmatched ~a" obj))))))))
       ;; TODO: clr/find-static-field-getter is from dotnet.sch;
       ;; replace with appropriate calls to clr/%get-field
       ;; and clr/%field-ref
       (lookup/adding-prefix 
	(syntax-rules ()
	  ((_ CTXT SYM PREFIX-STR CANON-CASES ...)
	   ((lambda (class-str field-str)
              (clr/%field-ref (clr/%get-field (clr/%get-type class-str) field-str) #f))
            (let ((chomp (lambda (s) (substring s 0 (- (string-length s) 1)))))
              (chomp PREFIX-STR))
            (canon-case CTXT SYM CANON-CASES ...))))))
    (define (option->type-attribute x)
      (lookup/adding-prefix 
       'option->type-attribute
       x "System.Reflection.TypeAttributes."
       ("AnsiClass" ansiclass ansi) ("AutoClass" autoclass) 
       ("Abstract" abstract) ("AutoLayout" autolayout auto) 
       ("BeforeFieldInit" beforefieldinit)
       ("Class" class) ("ClassSemanticsMask" classsemanticsmask) 
       ("ExplicitLayout" explicitlayout) 
       ("HasSecurity" hassecurity) ("Import" import) 
       ("Interface" interface) ("LayoutMask" layoutmask)
       ("NestedAssembly" nestedassembly) ("NestedFamORAssem" nestedfamorassem) 
       ("NestedPrivate" nestedprivate) ("NestedPublic" nestedpublic)
       ("NotPublic" notpublic private) ("Public" public)
       ("ReservedMask" reservedmask) ("RTSpecialName" rtspecialname)
       ("Sealed" sealed) ("SequentialLayout" sequentiallayout) 
       ("Serializable" serializable) 
       ("SpecialName" specialname) ("StringFormatMask" stringformatmask)
       ("UnicodeClass" unicodeclass) ("VisibiltyMask" visibilitymask)))
    
    (define (option->method-attribute x)
      (case x 
	((instance cil managed) #f) ;; special cases, yuck!
	(else 
	 (lookup/adding-prefix 
	  'option->method-attribute
	  x "System.Reflection.MethodAttributes."
	  ("Abstract" abstract) ("Assembly" assembly) 
          ("CheckAccessOnOverride" checkaccessonoverride) 
	  ("FamANDAssem" famandassem) ("Family" family) 
          ("FamORAssem" famorassem) ("Final" final) 
	  ("HasSecurity" hassecurity) ("HideBySig" hidebysig) 
          ("MemberAccessMask" memberaccessmask)
	  ("NewSlot" newslot) ("PinvokeImpl" pinvokeimpl) 
          ("Private" private) ("PrivateScope" privatescope)
	  ("Public" public) ("RequireSecObject" requiresecobject) 
          ("ReservedMask" reservedmask)
	  ("ReuseSlot" reuseslot) ("RTSpecialName" rtspecialname) 
          ("SpecialName" specialname) ("Static" static)
	  ("UnmanagedExport" unmanagedexport) ("Virtual" virtual) 
          ("VtableLayoutMask" vtablelayoutmask)))))
    
    (define (option->field-attribute x)
      (lookup/adding-prefix 
       'option->field-attribute
       x "System.Reflection.FieldAttributes."
       ("Assembly" assembly) ("FamANDAssem" famandassem) 
       ("Family" family) ("FamORAssem" famorassem) 
       ("FieldAccessMask" fieldaccessmask) 
       ("HasDefault" hasdefault) 
       ("HasFieldMarshal" hasfieldmarshal) ("HasFieldRVA" hasfieldrva) 
       ("InitOnly" initonly)
       ("Literal" literal) 
       ("NotSerialized" notserialized) ("PinvokeImpl" pinvokeimpl) 
       ("Private" private)
       ("PrivateScope" privatescope) ("Public" public) 
       ("ReservedMask" reservedmask) ("RTSpecialName" rtspecialname)
       ("SpecialName" specialname) ("Static" static)))
    
    (define (il:code->opcode x)
      (lookup/adding-prefix 
       'il:code->opcode 
       x "System.Reflection.Emit.OpCodes."
       ("Add" add) ("Add_Ovf" add_ovf add.ovf) 
       ("Add_Ovf_Un" add_ovf_un add.ovf.un)
       ("And" and) ("Arglist" arglist)
       ("Beq" beq) ("Beq_S" beq_s beq.s) 
       ("Bge" bge) ("Bge_S" bge_s bge.s)
       ("Bge_Un" bge_un bge.un) ("Bge_Un_S" bge_un_s bge.un.s)
       ("Bgt" bgt) ("Bgt_S" bgt_s bgt.s) 
       ("Bgt_Un" bgt_un bgt.un) ("Bgt_Un_S" bgt_un_s bgt.un.s)
       ("Ble" ble) ("Ble_S" ble_s ble.s)
       ("Ble_Un" ble_un ble.un) ("Ble_Un_S" ble_un_s ble.un.s)
       ("Blt" blt) ("Blt_S" blt_s blt.s)
       ("Blt_Un" blt_un blt.un) ("Blt_Un_S" blt_un_s blt.un.s)
       ("Bne_Un" bne_un bne.un) ("Bne_Un_S" bne_un_s bne.un.s) 
       ("Box" box)
       ("Br" br) ("Br_S" br_s br.s)
       ("Break" break)
       ("Brfalse" brfalse) ("Brfalse_S" brfalse_s brfalse.s) 
       ("Brtrue" brtrue) ("Brtrue_S" brtrue_s brtrue.s)
       ("Call" call) ("Calli" calli) ("Callvirt" callvirt) 
       ("Castclass" castclass) ("Ceq" ceq) 
       ("Cgt" cgt) ("Cgt_Un" cgt_un cgt.un) ("Ckfinite" ckfinite)
       ("Clt" clt) ("Clt_Un" clt_un clt.un) ("Constrained" constrained)
       ("Conv_I" conv_i conv.i) ("Conv_I1" conv_i1 conv.i1) 
       ("Conv_I2" conv_i2 conv.i2) ("Conv_I4" conv_i4 conv.i4) 
       ("Conv_I8" conv_i8 conv.i8)
       ("Conv_Ovf_I" conv_ovf_i conv.ovf.i)   
       ("Conv_Ovf_I_Un" conv_ovf_i_un conv.ovf.i.un)
       ("Conv_Ovf_I1" conv_ovf_i1 conv.ovf.i1) 
       ("Conv_Ovf_I1_Un" conv_ovf_i1_un conv.ovf.i1.un)       
       ("Conv_Ovf_I2" conv_ovf_i2 conv.ovf.i2) 
       ("Conv_Ovf_I2_Un" conv_ovf_i2_un conv.ovf.i2.un)       
       ("Conv_Ovf_I4" conv_ovf_i4 conv.ovf.i4) 
       ("Conv_Ovf_I4_Un" conv_ovf_i4_un conv.ovf.i4.un)       
       ("Conv_Ovf_I8" conv_ovf_i8 conv.ovr.i8) 
       ("Conv_Ovf_I8_Un" conv_ovf_i8_un conv.ovf.i8.un)
       ("Conv_Ovf_U" conv_ovf_u conv.ovf.u)   
       ("Conv_Ovf_U_Un" conv_ovf_u_un conv.ovf.u.un)
       ("Conv_Ovf_U1" conv_ovf_u1 conv.ovf.u1) 
       ("Conv_Ovf_U1_Un" conv_ovf_u1_un conv.ovf.u1.un)
       ("Conv_Ovf_U2" conv_ovf_u2 conv.ovf.u2) 
       ("Conv_Ovf_U2_Un" conv_ovf_u2_un conv.ovf.u2.un)
       ("Conv_Ovf_U4" conv_ovf_u4 conv.ovf.u4) 
       ("Conv_Ovf_U4_Un" conv_ovf_u4_un conv.ovf.u4.un)
       ("Conv_Ovf_U8" conv_ovf_u8 conv.ovf.u8) 
       ("Conv_Ovf_U8_Un" conv_ovf_u8_un conv.ovf.u8.un)
       ("Conv_R_Un" conv_r_un conv.r.un) 
       ("Conv_R4" conv_r4 conv.r4) 
       ("Conv_R8" conv_r8 conv.r8)
       ("Conv_U" conv_u conv.u) 
       ("Conv_U1" conv_u1 conv.u1) 
       ("Conv_U2" conv_u2 conv.u2) 
       ("Conv_U4" conv_u4 conv.u4) 
       ("Conv_U8" conv_u8 conv.u8) 
       (comment) 
       ("Cpblk" cpblk) ("Cpobj" cpobj)
       (directive) 
       ("Div" div) ("Div_Un" div_un div.un)
       ("Dup" dup) 
       ("Endfilter" endfilter) ("Endfinally" endfinally)
       ("Initblk" initblk) ("Initobj" initobj)
       ("Isinst" isinst) ("Jmp" jmp)
       (label) 
       ("Ldarg" ldarg)
       ("Ldarg_0" ldarg_0 ldarg.0)
       ("Ldarg_1" ldarg_1 ldarg.1)
       ("Ldarg_2" ldarg_2 ldarg.2)
       ("Ldarg_3" ldarg_3 ldarg.3)
       ("Ldarg_S" ldarg_S ldarg.s)       
       ("Ldc_I4" ldc_i4 ldc.i4) 
       ("Ldc_I4_0" ldc_i4_0 ldc.i4_0 ldc.i4.0)  
       ("Ldc_I4_1" ldc_i4_1 ldc.i4_1 ldc.i4.1)  
       ("Ldc_I4_2" ldc_i4_2 ldc.i4_2 ldc.i4.2)  
       ("Ldc_I4_3" ldc_i4_3 ldc.i4_3 ldc.i4.3)  
       ("Ldc_I4_4" ldc_i4_4 ldc.i4_4 ldc.i4.4)  
       ("Ldc_I4_5" ldc_i4_5 ldc.i4_5 ldc.i4.5)  
       ("Ldc_I4_6" ldc_i4_6 ldc.i4_6 ldc.i4.6)  
       ("Ldc_I4_7" ldc_i4_7 ldc.i4_7 ldc.i4.7)  
       ("Ldc_I4_8" ldc_i4_8 ldc.i4_8 ldc.i4.8)  
       ("Ldc_I4_M1" ldc_i4_M1 ldc.i4_M1 ldc.i4.m1) 
       ("Ldc_I4_S" ldc_i4_S ldc.i4_S ldc.i4.s)
       ("Ldc_I8" ldc_i8 ldc.i8) 
       ("Ldc_R4" ldc_r4 ldc.r4)
       ("Ldc_R8" ldc_r8 ldc.r8)
       ("Ldelem_I" ldelem_I ldelem.I) 
       ("Ldelem_I1" ldelem_I1 ldelem.I1) 
       ("Ldelem_I2" ldelem_I2 ldelem.I2) 
       ("Ldelem_I4" ldelem_I4 ldelem.I4) 
       ("Ldelem_I8" ldelem_I8 ldelem.I8) 
       ("Ldelem_R4" ldelem_R4 ldelem.R4) 
       ("Ldelem_R8" ldelem_R8 ldelem.R8) 
       ("Ldelem_Ref" ldelem_ref ldelem.ref) 
       ("Ldelem_U1" ldelem_u1 ldelem.u1) 
       ("Ldelem_U2" ldelem_u2 ldelem.u2) 
       ("Ldelem_U4" ldelem_u4 ldelem.u4) 
       ("Ldelema" ldelema) 
       ("Ldfld" ldfld)        
       ("Ldflda" ldflda) 
       ("Ldftn" ldftn) 
       ("Ldind_I" ldind_i ldind.i)        
       ("Ldind_I1" ldind_i1 ldind.i1)
       ("Ldind_I2" ldind_i2 ldind.i2)
       ("Ldind_I4" ldind_i4 ldind.i4)
       ("Ldind_I8" ldind_i8 ldind.i8)
       ("Ldind_R4" ldind_r4 ldind.r4)
       ("Ldind_R8" ldind_r8 ldind.r8)
       ("Ldind_Ref" ldind_ref ldind.ref)
       ("Ldind_U1" ldind_u1 ldind.u1)
       ("Ldind_U2" ldind_u2 ldind.u2)
       ("Ldind_U4" ldind_u4 ldind.u4)
       ("Ldlen" ldlen) 
       ("Ldloc" ldloc) 
       ("Ldloc_0" ldloc_0 ldloc.1) 
       ("Ldloc_1" ldloc_1 ldloc.2) 
       ("Ldloc_2" ldloc_2 ldloc.2) 
       ("Ldloc_3" ldloc_3 ldloc.3) 
       ("Ldloc_S" ldloc_s ldloc.s) 
       ("Ldloca" ldloca) 
       ("Ldloca_S" ldloca_s ldloca.s) 
       ("Ldnull" ldnull)
       ("Ldobj" ldobj)
       ("Ldsfld" ldsfld) 
       ("Ldsflda" ldsflda) 
       ("Ldstr" ldstr)
       ("Ldtoken" ldtoken)
       ("Ldvirtftn" ldvirtftn)
       ("Leave" leave)
       ("Leave_S" leave_s leave.s)
       ("Localloc" localloc)
       ("Mkrefany" mkrefany)
       ("Mul" mul)
       ("Mul_Ovf" mul_ovf mul.ovf)
       ("Mul_Ovf_Un" mul_ovf_un mul.ovf.un)
       ("Neq" neq)
       ("Newarr" newarr) ("Newobj" newobj) 
       ("Nop" nop) ("Not" not) ("Or" or)
       ("Pop" pop) ("Readonly" readonly)
       ("Refanytype" refanytype)
       ("Rem" rem) ("Rem_Un" rem_un rem.un)
       ("Ret" ret) ("Rethrow" rethrow)
       ("Shl" shl) ("Shr" shr) ("Shr_Un" shr_un shr.un)
       ("Sizeof" sizeof) 
       ("Starg" starg) ("Starg_S" starg_s starg.s)
       ("Stelem" stelem) 
       ("Stelem_I" stelem_i stelem.i) 
       ("Stelem_I1" stelem_i1 stelem.i1) 
       ("Stelem_I2" stelem_i2 stelem.i2) 
       ("Stelem_I4" stelem_i4 stelem.i4) 
       ("Stelem_I8" stelem_i8 stelem.i8) 
       ("Stelem_R4" stelem_r4 stelem.r4)
       ("Stelem_R8" stelem_r8 stelem.r8)
       ("Stelem_Ref" stelem_ref stelem.ref)
       ("Stfld" stfld)
       ("Stind_I" stind_i stind.i)
       ("Stind_I1" stind_i1 stind.i1)
       ("Stind_I2" stind_i2 stind.i2)
       ("Stind_I4" stind_i4 stind.i4)
       ("Stind_I8" stind_i8 stind.i8)
       ("Stind_R4" stind_r4 stind.r4)
       ("Stind_R8" stind_r8 stind.r8)
       ("Stind_Ref" stind_ref stind.ref)
       ("Stloc" stloc) 
       ("Stloc_0" stloc_0 stloc.0) 
       ("Stloc_1" stloc_1 stloc.1) 
       ("Stloc_2" stloc_2 stloc.2) 
       ("Stloc_3" stloc_3 stloc.3) 
       ("Stloc_S" stloc_S stloc.s) 
       ("Stobj" stobj)
       ("Stsfld" stsfld)
       ("Sub" sub)
       ("Sub_Ovf" sub_ovf sub.ovf)
       ("Sub_Ovf_Un" sub_ovf_un sub.ovf.un)
       ("Switch" switch)
       ("Tailcall" tailcall tail.)
       ("Throw" throw)
       ("Unaligned" unaligned)
       ("Unbox" unbox)
       ("Unbox_Any" unbox_any unbox.any)
       ("Volatile" volatile)
       ("Xor" xor)
       ))
    
    (values option->type-attribute
	    option->method-attribute
	    option->field-attribute
	    il:code->opcode)))

;; [pnkfelix] This code was written using il-sourcefile.sch as a
;; guideline, but I've decided against actually overriding the
;; definitions in that file.  Therefore, hide behind define-values,
;; exposing only the definitions I need.
;;;;; Never mind that for now, just make the names semi-unique and
;;;;; move on...
;;; (define-values (compile-class compile-member compile-il)
;;;   (let ()

    ;; maps IL-label objects to Emit.Label objects
    ;; Should be reset in between method constructions 
    ;; (though Felix is not sure if this is necessary)
    (define current-label-intern-table 
      (make-parameter "current-label-intern-table" '()))

    ;; IL-label -> Emit.Label
    (define (get-label-object label)
      (cond ((assoc (il-label:key label) (current-label-intern-table)) 
	     => cadr)
	    (else 
	     (let ((val (ilc/%define-label! (current-il-generator))))
	       (current-label-intern-table
		(cons (list (il-label:key label) val)
		      (current-label-intern-table)))
	       val))))


    ;; codump-directive : symbol string ... -> void
    (define (codump-directive directive . args)
      (case directive 
	((entrypoint) 
	 (ilc/%set-entry-point! (current-assembly-name) 
                                (current-method-builder)))
	((maxstack)
	 (cond 
	  (#f
	   (display (twobit-format 
		     #f "codump-directive: ignoring ~a for now" directive)) 
	   (newline))))
	((assembly-extern module line)
	 (error 'codump-directive 
		 (twobit-format 
		  #f "die on ~a for now" directive)))
	((local) 
	 (for-each 
	  (lambda (type)
	    (ilc/%declare-local! (current-il-generator) (co-find-class type)))
	  (car args)))
	((assembly) 
	 (let ((argument-name (car args))
	       (my-asm-name (current-assembly-name)))
           (ilc/%set-assembly-name-name!  my-asm-name argument-name)
	   (for-each codump-il (cadr args))))))

    ;; codump-il : il -> void
    (define (codump-il instr)
      (let* ((bytecode (il:code instr))
	     (args     (il:args instr))
	     (IL       (current-il-generator))
	     ;; delay projection into OpCodes enumeration, 
	     ;; to avoid error on il.code's that have no opcode.
	     (opc      (lambda () (il:code->opcode bytecode)))
	     (emit     (lambda (il opc . args)
			 ;; (display `(emit ,(il:code instr) ,@args)) (newline)
			 ;; (apply .Emit il opc args))) 
                         (error 'emit "Can't use javadot emit anymore")))
	     )
	
	(if (string? instr)
	    (error 'codump-il 
		   (twobit-format 
		    #f "cannot core dump string-based il: ~a" il)))
	
	(case (il:code instr)
	  ;; ILGenerator.Emit(OpCode) form
	  ((add add_ovf add_ovf_un and arglist 
	    break ceq cgt cgt.un ckfinite clt clt.un 
	    conv.i conv.i1 conv.i2 conv.i4 conv.i8 
	    conv.ovf.i conv.ovf.i.un 
	    conv.ovf.i1 conv.ivf.i1.un
	    conv.ovf.i2 conv.ovf.i2.un 
	    conv.ovf.i4 conv.ovf.i4.un
	    conv.ovf.i8 conv.ovf.i8.un 
	    conv.ovf.u conv.ovf.u.un
	    conv.ovf.u1 conv.ovf.u1.un
	    conv.ovf.u2 conv.ovf.u2.un 
	    conv.ovf.u4 conv.ovf.u4.un
	    conv.ovf.u8 conv.ovf.u8.un 
	    cpblk div div.un dup
	    endfilter endfinally
	    initblk 
	    ldelem.ref
	    pop ret
	    stelem.i2 stelem.ref
	    sub
	    tail.
	    ) 
	   (apply ilc/%emit IL (opc) args))
	  
	  ;; ILGenerator.Emit(OpCode, Label) form
	  ((beq beq.s bge bge.s bge.un bge.un.s
	    bgt bgt.s bgt.un bgt.un.s ble ble.s ble.un ble.un.s
	    blt blt.s blt.un blt.un.s bne.un bne.un.s 
	    br brfalse brfalse.s brtrue brtrue.s br.s) 
	   (ilc/%emit/label IL (opc) (get-label-object (car args))))
	  
	  ;; ILGenerator.Emit(OpCode, short) form
	  ((ldarg)
	   (apply ilc/%emit/short IL (opc) (map clr/int->foreign args)))

	  ;; ILGenerator.Emit(OpCode, int) form
	  ((ldc.i4)
	   (apply ilc/%emit/int IL (opc) (map clr/int->foreign args)))

	  ;; ILGenerator.Emit(OpCode, long) form
	  ((ldc.i8)
           (error 'emit 
                  "Marshalling numbers to int64 not yet part of dotnet-ffi"))

	  ;; ILGenerator.Emit(OpCode, double) form
	  ((ldc.r8)
	   (apply ilc/%emit/double IL (opc) 
                  (map clr/flonum->foreign-double args)))

	  ;; ILGenerator.Emit(OpCode, FieldInfo) form
	  ((ldfld ldsfld stfld stsfld)
	   (let* ((fld (car args))
		  (type (il-field:type fld))
		  (class (il-field:class fld))
		  (name (il-field:name fld)))
	     (ilc/%emit/field-info IL (opc) (co-find-field (co-find-class class) name))))

	  ;; ILGenerator.Emit(OpCode, LocalBuilder) and
	  ;; ILGenerator.Emit(OpCode, short) forms
	  ((ldloc stloc)
	   (apply ilc/%emit/short IL (opc) 
                  (map (lambda (x) (clr/%number->foreign-int16 x)) 
                       args)))

	  ;; ILGenerator.Emit(OpCode, string) form
	  ((ldstr)
	   (apply ilc/%emit/string IL (opc) (map clr/string->foreign args)))
	  
	  ;; ILGenerator.Emit(OpCode, Type) form
	  ((box castclass cpobj initobj isinst newarr)
	   (apply ilc/%emit/type IL (opc) (map co-find-class args)))

	  ;; ILGenerator.Emit(OpCode, Label[]) form
	  ((  switch)
	   (let* ((labels (car args))
		  (label-infos (labels->foreign-array
				(map get-label-object labels))))
	     (ilc/%emit/label-array IL (opc) label-infos)))
          
	  ;; ILGenerator.Emit(OpCode, ConstructorInfo) form
	  (();; (newobj)
	   (apply ilc/%emit/constructor-info IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, MethodInfo) form
	  ((jmp)
	   (error 'not-impl-yet)) ;; see below for hint on how to do this

	  ;; ILGenerator.Emit(OpCode, MethodInfo) and
	  ;; ILGenerator.EmitCall(OpCode, MethodInfo, Type[]) forms
	  ;; ILGenerator.Emit(OpCode, ConstructorInfo) form
	  ((call callvirt newobj)
	   (let* ((il-method (car args))
		  (type (il-method:type il-method))
		  (name (il-method:name il-method))
		  (class (il-method:class il-method))
		  (argtypes (il-method:argtypes il-method)))
	     (let* ((class-info (co-find-class class))
		    (args-info  (map co-find-class argtypes))
		    (method-info 
		     (co-find-method class-info
				     name 
				     (list->vector args-info))))
	       (if (null? method-info)
		   (error 'codump-il 
			  (twobit-format 
			   #f "couldn't find method for ~a.~a ~a"
			   class-info name args-info)))
               (let ((emit (cond ((clr/%isa? method-info 
                                             clr-type-handle/system-reflection-methodinfo)
                                  ilc/%emit/method-info)
                                 ((clr/%isa? method-info 
                                             clr-type-handle/system-reflection-constructorinfo)
                                  ilc/%emit/constructor-info)
                                 (else
                                  (error 'codump-il))
                             )))
                 (emit IL (opc) method-info)))))
	  
	  ((label)      
	   (let ((label-obj (get-label-object (car args))))
	     (ilc/%mark-label IL label-obj)))
	  
	  ((comment)    (if #f 'ignore-comments))
	  ((directive)  (apply codump-directive args))
	  
	  (else (error 'codump-il 
		       (twobit-format
			#f "unknown il code: ~a" (il:code instr))))
	  )
	))

    ;; co-register-field : field -> void
    (define (co-register-field field)
      (let ((name (field-name field))
	    (type (field-type field))
	    (options (field-options field)))
	(let ((cls (co-find-class type)))
	  (let ((field-info 
		 (ilc/%define-field (current-type-builder) 
                                    (clr/%string->foreign name)
                                    cls
                                    (clr/int->foreign (options->field-attributes options)))))
	    (current-registered-field-table
	     (cons (list (make-field-table-key (current-type-builder) name) field-info)
		   (current-registered-field-table)))))))

    ;; co-register-class : class -> void
    (define (co-register-class class)
      (let ((name (clr-class-name class))
	    (namespace (canonicalize-namespacez 
			(clr-class-il-namespace class)))
	    (super (clr-class-super class))
	    (options (clr-class-options class))
	    (members (clr-class-members class)))
	;; (display `(co-register-class ,(list namespace name))) (newline)
	(let ((type-builder (ilc/%define-type! 
                             (current-module-builder)
                             (clr/%string->foreign (namespace+name->full-name namespace name))
                             (clr/int->foreign (options->type-attributes options))
                             (co-find-class super))))
	  (current-registered-class-table
	   (cons (list (make-class-table-key namespace name) type-builder)
		 (current-registered-class-table)))
	  (current-registered-superclass-table
	   (cons (list (make-superclass-table-key type-builder) (co-find-class super))
		 (current-registered-superclass-table)))
	)))

    ;; (X:%Type) [Listof X] -> [%Arrayof X]
    (define (objects->foreign-array base-type objects)
      (let* ((len (length objects))
             (arr (allocate-clr-array base-type len)))
        (let loop ((i 0)
                   (l objects))
          (cond ((< i len)
                 (clr/%foreign-aset arr i (car l))
                 (loop (+ i 1) (cdr l)))))
        arr))
      

    ;; [Listof %Type] -> [%Arrayof %Type]
    (define (types->foreign-array types)
      (objects->foreign-array clr-type-handle/system-type types))

    ;; [Listof %Label] -> [%Arrayof %Label]
    (define (labels->foreign-array labels)
      (objects->foreign-array clr-type-handle/system-reflection-emit-label labels))

    ;; make-method-table-key : Type String [Vectorof Type] -> SchemeObject
    (define (make-method-table-key type-info name arg-infos)
      (list (clr/%to-string type-info)
            name 
            (map (lambda (x) (clr/%to-string x)) (vector->list arg-infos))))

    ;; make-field-table-key : Type String -> SchemeObject
    (define (make-field-table-key type-info name)
      (list (clr/%to-string type-info) name))

    (define (make-class-table-key namespace name)
      (list namespace name))

    (define (make-superclass-table-key type)
      (list (clr/%to-string type)))

    ;; co-register-method : IL-method -> void
    (define (co-register-method method)
      (let ((name     (clr-method-name method))
	    (ret-type (clr-method-type method))
	    (argtypes (clr-method-argtypes method))
	    (options  (clr-method-options method))
	    (instrs   (clr-method-instrs method)))
	(let* ((type-info (current-type-builder))
	       (arg-infos (map co-find-class argtypes))
               (arg-infos/fgn (types->foreign-array arg-infos))
	       (method-info 
		(cond 
		 ((equal? name ".ctor") 
                  (ilc/%define-constructor
		   (current-type-builder)
		   (clr/int->foreign (options->method-attributes options))
                   (ilc/%standard-calling-conventions)
		   arg-infos/fgn))

		 ((equal? name ".cctor") 
		  (ilc/%define-type-initializer
		   (current-type-builder)))

		 (else 
		  (ilc/%define-method
		   (current-type-builder)
		   (clr/string->foreign name)
		   (clr/int->foreign (options->method-attributes options))
		   (co-find-class ret-type)
		   arg-infos/fgn)))))
	  (current-registered-method-table
	   (cons (list (make-method-table-key type-info name (list->vector arg-infos))
                       method-info)
		 (current-registered-method-table))))))

    ;; A ClassRef is one of:
    ;; - (list namespace name)
    ;; - class
    ;; - il-class

    ;; class-ref->namespace+name : ClassRef -> (list namespace name)
    (define (class-ref->namespace+name x)
      (cond ((clr-class? x)
	     (list (canonicalize-namespacez (clr-class-il-namespace x))
		   (clr-class-name x)))
	    ((il-class? x)
	     (list (canonicalize-namespacez (il-class:namespaces x))
		   (il-class:name x)))
	    ((pair? x)
	     x)
	    (else (error 'class-ref->namespace+name))))

    ;; registered-class : ClassRef -> [Maybe TypeBuilder]
    (define (registered-class class-ref)
      (let ((namespace+name (class-ref->namespace+name class-ref)))
	(cond ((assoc (apply make-class-table-key namespace+name) 
                      (current-registered-class-table))
	       => cadr)
	      (else #f))))

    ;; registered-method : Type String [Vectorof Type] -> [Maybe MethodBuilder]
    (define (registered-method type name args)
      '(begin (write `(current-registered-method-table 
                      ,(current-registered-method-table)))
             (newline))
      (let ((key (make-method-table-key type name args)))
	(cond ((assoc key (current-registered-method-table))
	       => cadr)
	      (else #f))))

    ;; co-find-superclass : TypeBuilder -> [Maybe [Oneof Type TypeBuilder]]
    (define (co-find-superclass tb)
      (cond ((assoc (make-superclass-table-key tb) (current-registered-superclass-table))
	     => cadr)
	    (else #f)))

    ;; codump-class : class -> void
    (define (codump-class class)
      (parameterize ((current-il-namespace (clr-class-il-namespace class)))
	(codump-naked-class class)))

    (define (foldior l) 
      (if (null? l) 0 (fxlogior (car l) (foldior (cdr l)))))
    (define (foldenums l)
      (foldior (map (lambda (x) (clr/%foreign->int x)) l)))
    (define (options->type-attributes option-lst)
      (foldenums (map option->type-attribute option-lst)))
    (define (options->method-attributes option-lst)
      (foldenums (let ((poor-mans-filter 
                        (lambda (l) (apply append (map (lambda (x) (if x (list x) '())) l)))))
                   (poor-mans-filter (map option->method-attribute option-lst)))))
    (define (options->field-attributes option-lst)
      (foldenums (map option->field-attribute option-lst)))

    ;; A CanonNS is a [Listof String]

    ;; canonicalize-namespacez : [Oneof String [Listof String]] -> CanonNS
    (define (canonicalize-namespacez ns)
      (cond ((string? ns) (list ns))
	    (else ns)))

    ;; namespace+name->full-name : CanonNS String -> String
    (define (namespace+name->full-name ns name)
      (apply string-append 
	     (map/separated (lambda (x) x) (lambda () ".") 
			    (reverse (cons name (reverse ns))))))

    ;; co-get-field : type string -> FieldInfo
    ;; like Type.GetField method, except this will not die when first
    ;; argument is a TypeBuidler (rather than a Type)
    (define (co-find-field type name)
      ;;; UGH!  A simple association table won't work here, 
      ;;; because fields are INHERITED, and so I need to be
      ;;; able to (e.g.) lookup the name "instance" of the 
      ;;; superclass of type: CodeVector_1_1
      (cond ((assoc (make-field-table-key type name) (current-registered-field-table))
	     => cadr)
	    ((assoc (make-superclass-table-key type) (current-registered-superclass-table))
	     ;; if type is one of our currently registered classes, 
	     ;; try using its super type to get the field...
	     => (lambda (entry)
		  (co-find-field (cadr entry) name)))
	    (else (clr/%get-field type name))))

    ;; co-find-method : type string [Vectorof type] -> MethodBase
    ;; Note that type is the type of the method receiver, not the return type.
    (define (co-find-method type name args)
      (cond ((assoc (make-method-table-key type name args)
                    (current-registered-method-table))
	     => cadr)
            ((equal? name ".ctor")
	     (clr/%get-constructor type args))
	    (else 
             (clr/%get-method type name args))))

    (define (co-find-class x)
      (cond ((symbol? x)
	     ;; at some point, this may prepend the current
	     ;; namespace to its argument
	     (clr/%get-type x))
	    ((il-arraytype? x)
	     (let ((base-type (co-find-class (il-arraytype:basetype x))))
	       ;; YUCK!  Is there a clean way to directly construct
	       ;; the reflected array type given reflected base type?
	       ;; Note that this didn't work (not that its cleaner)
	       ;; (.GetType (System.Array.CreateInstance base-type 1))
	       (let* ((name (clr/%to-string (clr-type/%full-name base-type)))
		      (array-name (string-append name "[]")))
		 (clr/%get-type array-name))
	       ))
	    ((il-classtype? x)
	     (co-find-class (il-classtype:class x)))
	    ((il-primtype? x)
	     (co-find-class (il-primtype:class x)))
	    ((il-class? x)
	     (let* ((assembly   (il-class:assembly x))
                    (namespaces (canonicalize-namespacez
                                 (il-class:namespaces x)))
                    (name (il-class:name x)))
	       (cond 
		((registered-class x)
                 => (lambda (x)
                      x))
		(else 
                 (let ((fullname (namespace+name->full-name
                                  (or namespaces '()) name)))
                   (clr/%get-type fullname))))))
	    (else (error 'co-find-class 
			 (twobit-format
			  #f "Unknown class desc format: ~a" x)))))


    
    ;; codump-naked-class : class -> void
    (define (codump-naked-class class)
      (let ((members (clr-class-members class)))
	(parameterize ((current-type-builder (registered-class class)))
	  (for-each codump-member (reverse members))
	  (ilc/%create-type (current-type-builder)))))
    
    ;; codump-member : field | method -> void 
    (define (codump-member member)
      (cond ((field? member)
	     ;; Do nothing; we defined the field in co-register-field
	     ;; (codump-field member)
	     )
	    ((clr-method? member)
	     (codump-method member))))
    
    ;; codump-method : clr-method -> void
    (define (codump-method method)
      (let ((name (clr-method-name method))
	    (type (clr-method-type method))
	    (argtypes (clr-method-argtypes method))
	    (options (clr-method-options method))
	    (instrs (clr-method-instrs method)))
	(let* ((type-info (current-type-builder))
	       (arg-infos (map co-find-class argtypes))
	       (method-info (registered-method type-info name (list->vector arg-infos))))
	  (parameterize ((current-method-builder method-info))
            (if (not method-info)
                (error 'codump-method "Unable to find method builder"))
	    (parameterize ((current-il-generator 
			    (ilc/%get-ilgenerator (current-method-builder)))
			   (current-label-intern-table '()))
	      (for-each codump-il instrs))))))
    
;;;    (define compile-class codump-class)
;;;    (define compile-member codump-member)

;;;    (define compile-il codump-il)
;;;
;;;     (values compile-class compile-member compile-il)
;;;    ))


     (define (co-create-type-builders!)
       ;; prepass creating types to represent all of the classes we
       ;; are going to construct.
       (for-each (lambda (tli)
		   (cond ((clr-class? tli)
			  (co-register-class tli))))
		 (reverse *il-top-level*)))

     (define (co-create-member-infos!)
       ;; second prepass creating fields and methods (but no actual
       ;; code) for all the classes that we are going to construct.
       (for-each 
	(letrec ((handle-member 
		  (lambda (tli)
		    (cond ((field? tli)
			   (co-register-field tli))
			  ((clr-method? tli)
			   (co-register-method tli))))))
	  (lambda (tli)
	    (cond ((clr-class? tli)
		   (parameterize ((current-type-builder 
				   (registered-class tli)))
		     (for-each handle-member
			       (clr-class-members tli))))
		  (else (handle-member tli)))))
	(reverse *il-top-level*)))
     
     (define (co-emit-object-code!)
       ;; now actually EMIT the content of the classes
       (for-each (lambda (tli)
		   ;; (display tli) (newline) (newline)
		   (cond ((clr-class? tli) (codump-class tli))
			 ((clr-method? tli) (codump-member tli))
			 ((field? tli) (codump-member tli))
			 (else (codump-il tli))))
		 (reverse *il-top-level*)))

;; link-lop-segment/clr : segment string environment -> (-> any)
;; Analogous to link-lop-segment, except that the lop-segment uses our
;; internal IL rep (rather than machine code) for codevectors).
(define (link-lop-segment/clr lop-segment name environment)
  (let-syntax ((with-potential-setup
                (syntax-rules ()
                  ;((_ name thunk) (with-simple-fresh-dynamic-assembly-setup name thunk))
                  ((_ name thunk) (thunk))
                  ))
               (d 
                (syntax-rules ()
                  ((d EXP)
                   (begin (display 'EXP) (newline) EXP)))))
    (with-potential-setup
     name
     (lambda ()
       (init-variables)
       (let* ((entrypoint (dump-segment lop-segment))
	      (pseudo-manifest (extract-manifest lop-segment name)))
	 (set! *segment-number* (+ *segment-number* 1))
	 (set! *loadables* (cons (list *seed* entrypoint) *loadables*))

	 (co-create-type-builders!)
	 (co-create-member-infos!)
	 (co-emit-object-code!)

	 (current-saved-manifests (cons pseudo-manifest
					(current-saved-manifests)))
	 
	 (patch-procedure/pseudo-manifest pseudo-manifest environment)
         )))))

;; eval/clr : sexp [environment] -> any
(define (eval/clr x . rest)
  (let ((env (cond ((null? rest) (interaction-environment))
		   (else (car rest)))))
    ((link-lop-segment/clr (assemble (compile x)) 
			   (default-assembly-basename)
			   env))))

;; escape-assembly-illegal-chars : String -> String
;; Removes occurrences of #\\ #\/ and #\. from filename,
;; possibly replacing them with something else.
(define (escape-assembly-illegal-chars filename)
  ;; very inefficient!
  (list->string
   (apply append (map (lambda (char)
			(string->list (case char
					((#\\) "(hsals)")
					((#\/) "(slash)")
					((#\:) "(colon)")
					(else (string char)))))
		      (string->list filename)))))
					 
;; load-lop-to-clr : string [environment] -> void
;; Loads a single .lop file into the current CLR runtime.
(define (load-lop/clr filename . rest)
  
  (define (get-environment)
    (cond ((null? rest)
	   (interaction-environment))
	  (else 
	   (car rest))))
  
  (with-simple-fresh-dynamic-assembly-setup
   (escape-assembly-illegal-chars filename)
   (lambda ()
     (init-variables)
     (let ((entrypoints '())
	   (il-file-name (rewrite-file-type filename ".lop" ".code-il")))
       (call-with-input-file filename
	 (lambda (in)
	   (do ((segment (read in) (read in)))
	       ((eof-object? segment)
		(set! *loadables* (cons (cons *seed* (reverse entrypoints))
					*loadables*)))
	     (set! entrypoints (cons (dump-segment segment) entrypoints))
	     (current-saved-manifests
	      (cons (extract-manifest segment filename) 
		    (current-saved-manifests)))
	     (set! *segment-number* (+ *segment-number* 1)))))

       (co-create-type-builders!)
       (co-create-member-infos!)
       (co-emit-object-code!)

       (for-each (lambda (x) 
		   ((patch-procedure/pseudo-manifest x (get-environment))))
		 (reverse (current-saved-manifests)))))))

;; patch-procedure/pseudo-manifest : PseudoManifest Envionment -> Any
(define (patch-procedure/pseudo-manifest entry env)
  (let ((base   (list-ref entry 0))
	(il-ns  (list-ref entry 1))
	(zer    (list-ref entry 2)) ;; always 0?
	(segnum (+ 1 (list-ref entry 3)))
	(constant-vec (list-ref entry 4)))
    (let* ((p (link-lop-segment (cons #f constant-vec) env))
	   ;; similar to operation of .common-patch-procedure, except
	   ;; we don't use segment-code-address because that needs to
	   ;; look for stuff in files.
	   (find-code-in-assembly ilc/%find-code-in-assembly)
	   (asm-bld (current-assembly-builder))
	   (code-vec (find-code-in-assembly asm-bld (clr/string->foreign il-ns) (clr/int->foreign segnum)))
	   ;(ignore (begin (display `(code-vec ,code-vec)) (newline)))
	   (unwrapped-code-vec (clr/foreign->schemeobject code-vec))
	   ;(ignore (begin (display `(unwrapped-code-vec ,unwrapped-code-vec)) (newline)))
	   (patched-procedure 
	    (begin (procedure-set! p 0 unwrapped-code-vec) 
		   p)))
      patched-procedure)))

(define (with-saving-assembly-to-dll/full-control
	 base-name assembly-name module-name
	 dll-filename fasl-filename
	 storage-directory
	 thunk)
  (with-fresh-dynamic-assembly-setup 
   assembly-name module-name dll-filename storage-directory
   (lambda ()
     (let ((val (thunk)))
       (with-output-to-file fasl-filename
	 (lambda ()
	   (for-each (lambda (pm)
		       (dump-fasl/pmanifest base-name pm))
		     (reverse (current-saved-manifests)))))
       (ilc/%save (current-assembly-builder) 
                  (clr/%string->foreign dll-filename))
       val))))

(define (with-saving-assembly-to-dll base-name thunk)
  (with-saving-assembly-to-dll/full-control
   base-name 
   (string-append base-name "-assembly")
   (string-append base-name "-module")
   (string-append base-name ".dll")
   (string-append base-name ".fasl")
   thunk))

(define (compile-file/clr infilename . rest)
  (let ((outfilename 
	 (if (not (null? rest))
	     (car rest)
	     (rewrite-file-type infilename 
				*scheme-file-types*
				*fasl-file-type*)))
	(user (assembly-user-data))
	(syntaxenv (syntactic-copy (the-usual-syntactic-environment))))
    (call-with-values (lambda () (split-path-string outfilename))
      (lambda (dir file)
	(let* ((basename (rewrite-file-type file *fasl-file-type* ""))
	       (fully-qualified-basename
		(if (relative-path-string? dir)
		    (make-filename (current-directory) 
				   (string-append dir basename))
		    (string-append dir basename))))
	  (with-saving-assembly-to-dll/full-control
	   fully-qualified-basename
	   (string-append (escape-assembly-illegal-chars basename) "-assem")
	   (string-append (escape-assembly-illegal-chars basename) "-mod")
	   (string-append (escape-assembly-illegal-chars basename) ".dll")
	   outfilename
	   dir
	   
	   ;; Note: *seriously* need to abstract out this extremely 
	   ;; common pattern from go2.sch and il-corememory.sch
	   (lambda ()
	     (init-variables)
	     (let ((entrypoints '()))
	       
	       (process-file 
		infilename
		`(,outfilename binary)
		(assembly-declarations user)
		dump-fasl-segment-to-port
		(lambda (expr)
		  (let ((lop-segment (assemble (compile expr syntaxenv) user)))
		    (set! entrypoints (cons (dump-segment lop-segment)
					    entrypoints))
		    (current-saved-manifests
		     (cons (extract-manifest lop-segment 
					     fully-qualified-basename)
			   (current-saved-manifests)))
		    (set! *segment-number* (+ *segment-number* 1)))))
	       
	       (co-create-type-builders!)
	       (co-create-member-infos!)
	       (co-emit-object-code!)
	       ))))))))
