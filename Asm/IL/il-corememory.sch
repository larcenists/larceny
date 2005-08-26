;; This file uses javadot notation and only works when run atop a
;; Common Larceny runtime.
(enable-dotnet!)

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

;; with-fresh-dynamic-assembly-setup : String String String String (-> X) -> X
;; Sets up assembly-name, domain, asm-builder, and module.
;; Does *not* set up type-builder.
(define (with-fresh-dynamic-assembly-setup assembly-name 
					   module-name dll-file-name 
					   storage-directory
					   thunk)
  (let ((my-asm-name (System.Reflection.AssemblyName.)))
    (if assembly-name 
	(set-.name$! my-asm-name assembly-name))
    (parameterize ((current-assembly-name my-asm-name))
      (parameterize ((current-domain (System.Threading.Thread.GetDomain)))
	(display (current-assembly-name)) (newline)
	(parameterize ((current-assembly-builder 
			(apply .DefineDynamicAssembly 
			 (current-domain) (current-assembly-name)
			 ;; below permissions are liberal; might allow
			 ;; developer to select Run or Save alone
			 (System.Reflection.Emit.AssemblyBuilderAccess.RunAndSave$)
			 ;; This expression conspires with apply above
			 ;; to pass storage-directory iff not ""
			 (if (string=? "" storage-directory)
			     (list)
			     (list storage-directory)))))
	  (parameterize ((current-module-builder
			  (.DefineDynamicModule 
			   (current-assembly-builder) module-name dll-file-name)))
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
  (let ((my-asm-name (System.Reflection.AssemblyName.)))
    (if assembly-name 
	(set-.name$! my-asm-name assembly-name))
    (current-assembly-name my-asm-name)
    (current-domain (System.Threading.Thread.GetDomain))
    (current-assembly-builder 
     (.DefineDynamicAssembly (current-domain) (current-assembly-name)
			     ;; below permissions are liberal; might allow
			     ;; developer to select Run or Save alone
			     (System.Reflection.Emit.AssemblyBuilderAccess.RunAndSave$)))
    (current-module-builder (.DefineDynamicModule 
			     (current-assembly-builder) module-name dll-file-name))
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
  (current-type-builder (.DefineType (current-module-builder)
				     name
				     '()
				     System.Object.class)))

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
      ;; (canon-case 'y ((a b c) (x y z))) ==> x
      ((canon-case 
	(syntax-rules ()
	  ((_ CTXT X (TAG REST ...) ...)
	   (let ((obj X))
	     (case obj
	       ((TAG REST ...) (quote TAG)) ...
	       (else (error CTXT
			    (twobit-format #f "Unmatched ~a" obj))))))))
       (lookup/adding-prefix 
	(syntax-rules ()
	  ((_ CTXT SYM PREFIX-STR CANON-CASES ...)
	   ((clr/find-static-field-getter
	     #t (string->symbol
		 (string-downcase ;; odd!  (see Lib/MzScheme/dotnet.sch)
		  (string-append 
		   PREFIX-STR
		   (symbol->string
		    (canon-case CTXT SYM CANON-CASES ...)))))))))))
    (define (option->type-attribute x)
      (lookup/adding-prefix 
       'option->type-attribute
       x "System.Reflection.TypeAttributes."
       (ansiclass ansi) (autoclass) (unicodeclass)
       (abstract) (autolayout auto) (beforefieldinit)
       (class) (classsemanticsmask) (explicitlayout) 
       (hassecurity) (import) (interface) (layoutmask)
       (nestedassembly) (nestedfamorassem) 
       (nestedprivate) (nestedpublic)
       (notpublic private) (public)
       (reservedmask) (rtspecialname)
       (sealed) (sequentiallayout) (serializable) 
       (specialname) (stringformatmask)
       (unicodeclass) (visibilitymask)))
    
    (define (option->method-attribute x)
      (case x 
	((instance cil managed) '()) ;; special cases, yuck!
	(else 
	 (lookup/adding-prefix 
	  'option->method-attribute
	  x "System.Reflection.MethodAttributes."
	  (abstract) (assembly) (checkaccessonoverride) 
	  (famandassem) (family) (famorassem) (final) 
	  (hassecurity) (hidebysig) (memberaccessmask)
	  (newslot) (pinvokeimpl) (private) (privatescope)
	  (public) (requiresecobject) (reservedmask)
	  (reuseslot) (rtspecialname) (specialname) (static)
	  (unmanagedexport) (virtual) (vtablelayoutmask)))))
    
    (define (option->field-attribute x)
      (lookup/adding-prefix 
       'option->field-attribute
       x "System.Reflection.FieldAttributes."
       (assembly) (famandassem) (family) (famorassem) 
       (fieldaccessmask) 
       (hasdefault) (hasfieldmarshal) (hasfieldrva) (initonly)
       (literal) (notserialized) (pinvokeimpl) (private)
       (privatescope) (public) (reservedmask) (rtspecialname)
       (specialname) (static)))
    
    (define (il:code->opcode x)
      (lookup/adding-prefix 
       'il:code->opcode 
       x "System.Reflection.Emit.OpCodes."
       (beq) (beq_s beq.s) (bne_un_s bne.un.s) (br) (brfalse)
       (brfalse_s brfalse.s) (brtrue) (brtrue_s brtrue.s)
       (call) (callvirt) (castclass) (ceq) (comment) 
       (directive) (dup) (isinst) (label) (ldarg)
       (ldc_i4 ldc.i4) (ldc_i8 ldc.i8) (ldc_r8 ldc.r8)
       (ldelem_ref ldelem.ref) (ldfld) (ldloc) (ldsfld) (ldstr)
       (newarr) (newobj) (pop) (ret)
       (stelem_i2 stelem.i2) (stelem_ref stelem.ref)
       (stfld) (stloc) (stsfld) (sub) (switch) (tailcall tail.)))
    
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
	     (let ((val (.DefineLabel (current-il-generator))))
	       (current-label-intern-table
		(cons (list (il-label:key label) val)
		      (current-label-intern-table)))
	       val))))


    ;; codump-directive : symbol string ... -> void
    (define (codump-directive directive . args)
      (case directive 
	((entrypoint) 
	 (.SetEntryPoint (current-assembly-name) (current-method-builder)))
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
	    (.DeclareLocal (current-il-generator) (co-find-class type)))
	  (car args)))
	((assembly) 
	 (let ((argument-name (car args))
	       (my-asm-name (current-assembly-name)))
	   (set-.name$! my-asm-name argument-name)
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
			 (apply .Emit il opc args)))
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
	   (apply emit IL (opc) args))
	  
	  ;; ILGenerator.Emit(OpCode, Label) form
	  ((beq beq.s bge bge.s bge.un bge.un.s
	    bgt bgt.s bgt.un bgt.un.s ble ble.s ble.un ble.un.s
	    blt blt.s blt.un blt.un.s bne.un bne.un.s 
	    br brfalse brfalse.s brtrue brtrue.s br.s) 
	   (emit IL (opc) (get-label-object (car args))))
	  
	  ;; ILGenerator.Emit(OpCode, short) form
	  ((ldarg)
	   (apply emit IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, int) form
	  ((ldc.i4)
	   (apply emit IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, long) form
	  ((ldc.i8)
	   (apply emit IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, double) form
	  ((ldc.r8)
	   (apply emit IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, FieldInfo) form
	  ((ldfld ldsfld stfld stsfld)
	   (let* ((fld (car args))
		  (type (il-field:type fld))
		  (class (il-field:class fld))
		  (name (il-field:name fld)))
	     (emit IL (opc) (co-find-field (co-find-class class) name))))

	  ;; ILGenerator.Emit(OpCode, LocalBuilder) and
	  ;; ILGenerator.Emit(OpCode, short) forms
	  ((ldloc stloc)
	   (apply emit IL (opc) args))

	  ;; ILGenerator.Emit(OpCode, string) form
	  ((ldstr)
	   (apply emit IL (opc) args))
	  
	  ;; ILGenerator.Emit(OpCode, Type) form
	  ((box castclass cpobj initobj isinst newarr)
	   (apply emit IL (opc) (map co-find-class args)))

	  ;; ILGenerator.Emit(OpCode, Label[]) form
	  ((  switch)
	   (let* ((labels (car args))
		  (label-infos (list->vector 
				(map get-label-object labels))))
	     (emit IL (opc) label-infos)))

	  ;; ILGenerator.Emit(OpCode, ConstructorInfo) form
	  (();; (newobj)
	   (apply emit IL (opc) args))

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
	       (emit IL (opc) method-info))))
	  
	  ((label)      
	   (let ((label-obj (get-label-object (car args))))
	     (.MarkLabel IL label-obj)))
	  
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
		 (.DefineField (current-type-builder) 
			       name
			       cls
			       (options->field-attributes options))))
	    (current-registered-field-table
	     (cons (list (list (current-type-builder) name) field-info)
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
	(let ((type-builder (.DefineType (current-module-builder)
					 (namespace+name->full-name 
					  namespace name)
					 (options->type-attributes options)
					 (co-find-class super))))
	  (current-registered-class-table
	   (cons (list (list namespace name) type-builder)
		 (current-registered-class-table)))
	  (current-registered-superclass-table
	   (cons (list type-builder (co-find-class super))
		 (current-registered-superclass-table)))
	)))

    ;; co-register-method : IL-method -> void
    (define (co-register-method method)
      (let ((name     (clr-method-name method))
	    (ret-type (clr-method-type method))
	    (argtypes (clr-method-argtypes method))
	    (options  (clr-method-options method))
	    (instrs   (clr-method-instrs method)))
	(let* ((type-info (current-type-builder))
	       (arg-infos (list->vector (map co-find-class argtypes)))
	       (method-info 
		(cond 
		 ((equal? name ".ctor") 
		  (.DefineConstructor 
		   (current-type-builder)
		   (options->method-attributes options)
		   (System.Reflection.CallingConventions.Standard$)
		   arg-infos))

		 ((equal? name ".cctor") 
		  (.DefineTypeInitializer 
		   (current-type-builder)))

		 (else 
		  (.DefineMethod 
		   (current-type-builder)
		   name
		   (options->method-attributes options)
		   (co-find-class ret-type)
		   arg-infos)))))
	  (current-registered-method-table
	   (cons (list (list type-info name arg-infos) method-info)
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
	(cond ((assoc namespace+name (current-registered-class-table))
	       => cadr)
	      (else #f))))

    ;; registered-method : Type String [Vectorof Type] -> [Maybe MethodBuilder]
    (define (registered-method type name args)
      (let ((key (list type name args)))
	(cond ((assoc key (current-registered-method-table))
	       => cadr)
	      (else #f))))

    ;; co-find-superclass : TypeBuilder -> [Maybe [Oneof Type TypeBuilder]]
    (define (co-find-superclass tb)
      (cond ((assoc tb (current-registered-superclass-table))
	     => cadr)
	    (else #f)))

    ;; codump-class : class -> void
    (define (codump-class class)
      (parameterize ((current-il-namespace (clr-class-il-namespace class)))
	(codump-naked-class class)))

    (define (options->type-attributes option-lst)
      (apply append (map option->type-attribute option-lst)))
    (define (options->method-attributes option-lst)
      (apply append (map option->method-attribute option-lst)))
    (define (options->field-attributes option-lst)
      (apply append (map option->field-attribute option-lst)))

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
      (cond ((assoc (list type name) (current-registered-field-table))
	     => cadr)
	    ((assoc type (current-registered-superclass-table))
	     ;; if type is one of our currently registered classes, 
	     ;; try using its super type to get the field...
	     => (lambda (entry)
		  (co-find-field (cadr entry) name)))
	    (else (.GetField type name))))

    ;; co-find-method : type string [Vectorof type] -> MethodBase
    ;; Note that type is the type of the method receiver, not the return type.
    (define (co-find-method type name args)
      (cond ((assoc (list type name args) (current-registered-method-table))
	     => cadr)
            ((equal? name ".ctor")
	     (.GetConstructor type args))
	    (else (.GetMethod type name args))))

    (define (co-find-class x)
      (cond ((symbol? x)
	     ;; at some point, this may prepend the current
	     ;; namespace to its argument
	     (clr/find-class x))
	    ((il-arraytype? x)
	     (let ((base-type (co-find-class (il-arraytype:basetype x))))
	       ;; YUCK!  Is there a clean way to directly construct
	       ;; the reflected array type given reflected base type?
	       ;; Note that this didn't work (not that its cleaner)
	       ;; (.GetType (System.Array.CreateInstance base-type 1))
	       (let* ((name (.FullName$ base-type))
		      (array-name (string-append name "[]")))
		 (System.Type.GetType array-name))
	       ))
	    ((il-classtype? x)
	     (co-find-class (il-classtype:class x)))
	    ((il-primtype? x)
	     (co-find-class (il-primtype:class x)))
	    ((il-class? x)
	     (let ((assembly   (il-class:assembly x))
		   (namespaces (canonicalize-namespacez
				(il-class:namespaces x)))
		   (name (il-class:name x)))
	       (cond 
		((registered-class x))
		(else 
		 (clr/find-class (string->symbol
				  (namespace+name->full-name
				   (or namespaces '()) name)))))))
	    (else (error 'co-find-class 
			 (twobit-format
			  #f "Unknown class desc format: ~a" x)))))


    
    ;; codump-naked-class : class -> void
    (define (codump-naked-class class)
      (let ((members (clr-class-members class)))
	(parameterize ((current-type-builder (registered-class class)))
	  (for-each codump-member (reverse members))
	  (.CreateType (current-type-builder)))))
    
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
	       (arg-infos (list->vector (map co-find-class argtypes)))
	       (method-info (registered-method type-info name arg-infos)))
	  (parameterize ((current-method-builder method-info))
	    (parameterize ((current-il-generator 
			    (.GetILGenerator (current-method-builder)))
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
  (let ((with-potential-setup 
	 ;; with-simple-fresh-dynamic-assembly-setup
	 (lambda (name thunk) 
	   (thunk))))
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
	 
	 (patch-procedure/pseudo-manifest pseudo-manifest environment))))))

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
	   (find-code-in-assembly Scheme.RT.Load.findCodeInAssembly)
	   (asm-bld (current-assembly-builder))
	   (code-vec (find-code-in-assembly asm-bld il-ns segnum))
	   (ignore (begin (display code-vec) (newline)))
	   (boxed-code-vec (clr-object/clr-handle code-vec))
	   (ignore (begin (display boxed-code-vec) (newline)))
	   (unwrapped-code-vec (clr/foreign->schemeobject boxed-code-vec))
	   (ignore (begin (display unwrapped-code-vec) (newline)))
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
       (.Save (current-assembly-builder) dll-filename)
       val))))

(define (with-saving-assembly-to-dll base-name thunk)
  (with-saving-assembly-to-dll/full-control
   base-name 
   (string-append base-name "-assembly")
   (string-append base-name "-module")
   (string-append base-name ".dll")
   (string-append base-name ".fasl")
   thunk))

(define (compile-file infilename . rest)
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
	   (string-append basename "-assem")
	   (string-append basename "-mod")
	   (string-append basename ".dll")
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
