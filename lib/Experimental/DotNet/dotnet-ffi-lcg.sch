(define clr-method->procedure/clr-invoke
  (let* ((mi-type (find-clr-type "System.Reflection.MethodInfo"))
	 (is-static-prop (clr/%get-property mi-type "IsStatic" '#()))
	 (is-static? ; : MethodInfo -> Boolean
	  (lambda (method-info)
	    (clr/foreign->bool 
	     (clr/%property-ref is-static-prop method-info '#()))))
	 (method:is-instance-of-type
	  (clr/%get-method (find-clr-type "System.Type")
			   "IsInstanceOfType" 
			   (vector (find-clr-type "System.Object"))))
	 (type:constructorinfo
	  (find-clr-type "System.Reflection.ConstructorInfo"))
	 (is-ctor? ; : Any -> Boolean
	  (lambda (method-info)
	    (clr/foreign->bool
	     (clr/%invoke method:is-instance-of-type
			  type:constructorinfo
			  (vector method-info)))))
	 )
    (define clr-method->procedure/clr-invoke
      (lambda (member-info)
	(cond 
	 ((is-ctor? member-info)
	  (lambda args (clr/%invoke-constructor member-info
						(list->vector args))))
	 ((is-static? member-info)
	  (lambda args (clr/%invoke member-info 
				    #f (list->vector args))))
	 (else 
	  (lambda args (clr/%invoke member-info 
				    (car args) (list->vector (cdr args))))
	  ))))
    clr-method->procedure/clr-invoke))

(define clr-method->procedure/lcg
  (letrec-syntax ((let*-renamethismacrototracebindings
                   (syntax-rules ()
                     ((let* () BODY ...)
                      (let () BODY ...))
                     ((let* ((ID EXP) REST ...) BODY ...)
                      (let () 
                        (display '(ID EXP)) (newline)
                        (let ((ID EXP))
                          (let* (REST ...) BODY ...))))))
                  (must (syntax-rules () 
			  ((must EXP)
			   (or EXP (error 'method->procedure "internal err"))))))
    (let* ((mi-type (find-clr-type "System.Reflection.MethodInfo"))
	   (is-static-prop (clr/%get-property mi-type "IsStatic" '#()))
	   (is-static? ; : MethodInfo -> Boolean
	    (lambda (method-info)
	      (clr/foreign->bool 
	       (clr/%property-ref is-static-prop method-info '#()))))
	   (is-virtual-prop (clr/%get-property mi-type "IsVirtual" '#()))
	   (is-virtual? ; : MethodInfo -> Boolean
	    (lambda (method-info)
	      (clr/foreign->bool
	       (clr/%property-ref is-virtual-prop method-info '#()))))
	   (type-getter (lambda (prefix)
			  (lambda (x)
			    (must (clr/%get-type (string-append prefix x))))))
	   
	   (get-sys-type (type-getter "System."))
	   (get-emit-type (type-getter "System.Reflection.Emit."))
	   (get-rep-type (type-getter "Scheme.Rep."))
	   (get-rts-type (type-getter "Scheme.RT."))
	   (get-refl-type (type-getter "System.Reflection."))
	   
	   (type:object (get-sys-type "Object"))
	   (type:type (get-sys-type "Type"))
	   (type:string (get-sys-type "String"))
	   (type:type-array (get-sys-type "Type[]"))
	   (type:reg (get-rts-type "Reg"))
	   (type:fixnum (get-rep-type "SFixnum"))
	   (type:ilgenerator (get-emit-type "ILGenerator"))
	   (type:dynamicmethod (get-emit-type "DynamicMethod"))
	   (type:fieldinfo (get-refl-type "FieldInfo"))
	   (type:int32 (get-sys-type "Int32"))
	   (type:label (get-emit-type "Label"))
	   (type:methodinfo (get-refl-type "MethodInfo"))
	   (type:constructorinfo (get-refl-type "ConstructorInfo"))
	   (type:exn (get-rts-type "Exn"))
	   (type:factory (get-rep-type "Factory"))
	   (type:instructions (get-rts-type "Instructions"))
	   (type:foreignbox (get-rep-type "ForeignBox"))
	   (type:codeaddress (get-rts-type "CodeAddress"))
	   (type:parameter-info (get-refl-type "ParameterInfo"))

	   (get-method (lambda (recv-t name arg-ts) 
			 (must (clr/%get-method recv-t name (list->vector arg-ts)))))
	   (method:get-il-generator
	    (get-method type:dynamicmethod "GetILGenerator" '()))
	   (get-emit-method 
	    (let ((type:opcode (get-emit-type "Opcode")))
	      (lambda (arg-ts)
		(get-method type:ilgenerator "Emit" (cons type:opcode arg-ts)))))
	   (method:emit-with-field (get-emit-method (list type:fieldinfo)))
	   (method:emit-with-type (get-emit-method (list type:type)))
	   (method:emit-with-int32 (get-emit-method (list type:int32)))
	   (method:define-label (get-method type:ilgenerator "DefineLabel" '()))
	   (method:mark-label 
	    (get-method type:ilgenerator "MarkLabel" (list type:label)))
	   (method:emit-with-label (get-emit-method (list type:label)))
	   (method:emit-with-method (get-emit-method (list type:methodinfo)))
	   (method:emit-with-constructor 
	    (get-emit-method (list type:constructorinfo)))
	   (method:emit (get-emit-method '()))
	   
	   (get-opcode 
	    (let ((type:opcodes (get-emit-type "Opcodes")))
	      (lambda (name)
		(must (clr/%field-ref
		       (clr/%get-field type:opcodes name) #f)))))
	   
	   (emit! 
	    (lambda (method ilgen argv) 
	      '(begin (display `("TRACING" emit! ,method ilgen ,argv)) (newline))
	      (clr/%invoke method ilgen argv)))

	   (define-label
	     (lambda (ilgen) 
	       (emit! method:define-label ilgen '#())))
	   (mark-label 
	    (lambda (ilgen label) 
	      (emit! method:mark-label ilgen (vector label))))
	   (make-emit-for-op
	    (lambda (opcode-name method)
	      (let ((opcode (get-opcode opcode-name)))
		(lambda (ilgen)
		  (emit! method ilgen (vector opcode))))))
	   (make-emit-for-op-and-onemore
	    (lambda (opcode-name method)
	      (let ((opcode (get-opcode opcode-name)))
		(lambda (ilgen onemore)
		  (emit! method ilgen (vector opcode onemore))))))

	   (emit:ldsfld
	    (make-emit-for-op-and-onemore "Ldsfld" method:emit-with-field))
	   (emit:castclass
	    (make-emit-for-op-and-onemore "Castclass" method:emit-with-type))
	   (emit:ldfld 
	    (make-emit-for-op-and-onemore "Ldfld" method:emit-with-field))
	   (emit:ldc.i4 
	    (make-emit-for-op-and-onemore "Ldc_I4" method:emit-with-int32))
	   (emit:beq.s 
	    (make-emit-for-op-and-onemore "Beq_S" method:emit-with-label))
	   (emit:call 
	    (make-emit-for-op-and-onemore "Call" method:emit-with-method))
	   (emit:callvirt
	    (make-emit-for-op-and-onemore "Callvirt" method:emit-with-method))
	   (emit:ret 
	    (make-emit-for-op "Ret" method:emit))
	   (emit:stsfld 
	    (make-emit-for-op-and-onemore "Stsfld" method:emit-with-field))
	   (emit:box 
	    (make-emit-for-op-and-onemore "Box" method:emit-with-type))
	   (emit:unbox
	    (make-emit-for-op-and-onemore "Unbox" method:emit-with-type))
	   (emit:unbox.any
	    (make-emit-for-op-and-onemore "Unbox_Any" method:emit-with-type))
	   (emit:newobj
	    (make-emit-for-op-and-onemore "Newobj" 
					  method:emit-with-constructor))

	   (get-fieldinfo (lambda (recv-t name)
			    (must (clr/%get-field recv-t name))))
	   (field:result-reg (get-fieldinfo type:reg "Result"))
	   (field:false (get-fieldinfo type:factory "False"))
	   (field:zero (get-fieldinfo type:fixnum "zero"))
	   (field:one (get-fieldinfo type:fixnum "one"))
	   (field:two (get-fieldinfo type:fixnum "two"))
	   (field:three (get-fieldinfo type:fixnum "three"))
	   (field:four (get-fieldinfo type:fixnum "four"))
	   (field:foreignbox-value (get-fieldinfo type:foreignbox "value"))
	   (field:fixnum-value (get-fieldinfo type:fixnum "value"))
	   (method:faultArgCount 
	    (get-method type:exn "faultArgCount" (list type:int32)))
	   (method:makeForeignBox 
	    (get-method type:factory "makeForeignBox" (list type:object)))
	   (method:rtn
	    (get-method type:instructions "rtn" '()))
	   (method:makeCodeVector
	    (get-method type:factory "makeCodeVector" (list type:int32
							    type:dynamicmethod)))
	   (fill-in-segment!
	    (lambda (dmethod
		     param-count 
		     generate-convert-params
		     generate-call-method
		     generate-convert-result)
	      (let* ((ilgen (clr/%invoke method:get-il-generator dmethod '#())))
		(let () ;; args= param-count
		  (emit:ldsfld ilgen field:result-reg)
		  (cond ((< param-count 5)
			 (emit:ldsfld 
			  ilgen (case param-count
				  ((0) field:zero)
				  ((1) field:one)
				  ((2) field:two)
				  ((3) field:three)
				  ((4) field:four)
				  (else 
				   (error 'method->procedure "internal err")))))
			(else
			 (emit:castclass ilgen type:fixnum)
			 (emit:ldfld ilgen field:fixnum-value)
			 (emit:ldc.i4 ilgen (clr/int->foreign param-count))))
		  (let ((okay-label (define-label ilgen)))
		    (emit:beq.s ilgen okay-label)
		    (emit:ldc.i4 ilgen (clr/int->foreign param-count))
		    (emit:call ilgen method:faultArgCount)
		    (emit:ret ilgen)
		    (mark-label ilgen okay-label)))
		(generate-convert-params ilgen) ;; push params
		(generate-call-method ilgen) ;; call method
		(generate-convert-result ilgen) ;; convert and store result if any
		(let () ;; return
		  (emit:call ilgen method:rtn)
		  (emit:ret ilgen)))))
	   (vector->foreign-array
	    (lambda (base-type vec)
	      (let* ((len (vector-length vec))
		     (arr (allocate-clr-array base-type len)))
		(let loop ((i 0))
		  (cond ((< i len)
			 (clr/%foreign-aset arr i (vector-ref vec i))
			 (loop (+ i 1)))))
		arr)))
	   (new-dynamic-method
	    (let* ((ctor (clr/%get-constructor
			  type:dynamicmethod (vector type:string 
						     type:type type:type-array)))
                   (ctor 
                    (or ctor
                        (error 'dotnet-ffi-lcg 
                               "missing appropriate dynamicmethod constructor"
                               "LCG support requires at least"
                               ".NET 2.0 Service Pack 1"))))
	      (lambda (name ret-type arg-typev)
		(clr/%invoke-constructor
		 ctor (vector (clr/string->foreign name)
			      ret-type
			      (vector->foreign-array type:type
						     arg-typev))))))
	   (make-dynamic-method 
	    (lambda (name arg-count) 
	      ;; new-dynamic-method is from pass5p2, but it would not be unreasonable
	      ;; to inline a copy of it here just to reduce the dependency issues
	      ;; (I'm already duplicating a ton of other things from there)
	      (new-dynamic-method name
				  type:codeaddress
				  (vector type:int32))))
	   (generate-segment 
	    (lambda (name
		     param-count
		     generate-convert-params
		     generate-call-method
		     generate-convert-result)
	      (let* ((dmethod (make-dynamic-method name param-count)))
		(fill-in-segment! dmethod 
				  param-count 
				  generate-convert-params
				  generate-call-method
				  generate-convert-result)
		(let* ((foreign-codevector
			(clr/%invoke method:makeCodeVector #f
				     (vector (clr/int->foreign 1) dmethod)))
		       (code (clr/%foreign->schemeobject foreign-codevector))
		       (name (string->symbol name))
		       (arity param-count))
		  `((code ,code) (name ,name) (arity ,arity))))))

	   (value-type?
	    (let* ((prop:is-value-type 
		    (clr/%get-property type:type "IsValueType" '#())))
	      (lambda (type)
		(let* ((is-value-type
			(clr/%property-ref prop:is-value-type type '#())))
		  (clr/foreign->bool is-value-type)))))
	   (emit-convert-self-param
	    (lambda (ilgen type)
	      (if (value-type? type)
		  (emit:unbox     ilgen type)
		  (emit:castclass ilgen type))))
	   (emit-convert-param 
	    (lambda (ilgen type)
	      (if (value-type? type)
		  (emit:unbox.any ilgen type)
		  (emit:castclass ilgen type))))
	   (name-and-params-and-call-and-convert->segment 
	    (lambda (name maybe-self-type param-types call-il convert-il)
	      (let* ((arg-count (length param-types))
		     (arg-count (+ (if maybe-self-type 1 0) arg-count)))
		(generate-segment
		 name
		 arg-count
		 (let ((method:n->get-reg 
			(lambda (n)
			  (let* ((nstr (number->string n))
				 (grstr "get_Register")
				 (get-reg (string-append grstr nstr)))
			    (get-method type:reg get-reg '())))))
		   (lambda (ilgen)
		     (cond (maybe-self-type
			    (emit:call ilgen (method:n->get-reg 1))
			    (emit:castclass ilgen type:foreignbox)
			    (emit:ldfld ilgen field:foreignbox-value)
			    (emit-convert-self-param ilgen maybe-self-type)))
		     (let next-type ((types param-types) 
				     (i (if maybe-self-type 2 1)))
		       (cond ((not (null? types))
			      (emit:call ilgen (method:n->get-reg i))
			      (emit:castclass ilgen type:foreignbox)
			      (emit:ldfld ilgen field:foreignbox-value)
			      (emit-convert-param ilgen (car types))
			      (next-type (cdr types) (+ i 1)))))))
		 call-il
		 convert-il))))
	   (method->declaring-type
	    (let* ((prop:declaring-type
		    (clr/%get-property type:methodinfo 
				       "DeclaringType" '#())))
	      (lambda (method-info)
		(clr/%property-ref prop:declaring-type method-info '#()))))
	   (method->name ; : MethodInfo -> String
	    (let* ((prop:name (clr/%get-property type:methodinfo "Name" '#())))
	      (lambda (method-info)
		(clr/foreign->string
		 (clr/%property-ref prop:name method-info '#())))))
	   (prop:parameter-type 
	    (clr/%get-property type:parameter-info "ParameterType" '#()))
	   (method->param-types ; : MethodInfo -> [Listof Type]
	    (let* ((type:parameter-info (get-refl-type "ParameterInfo")) 
		   (get-parameters-method
		    (clr/%get-method mi-type "GetParameters" '#())))
	      (lambda (method-info)
		(let* ((param-arr (clr/%invoke get-parameters-method 
					       method-info '#()))
		       (params (clr-array->list param-arr))
		       (param-types
			(map (lambda (param) 
			       (clr/%property-ref prop:parameter-type 
						  param '#()))
			     params)))
		  param-types))))
	   (void-type? 
	    (let* ((type:void (get-sys-type "Void"))
		   (method:equals
		    (clr/%get-method type:type "Equals" (vector type:type))))
	      (lambda (type)
		(clr/foreign->bool 
		 (clr/%invoke method:equals type (vector type:void))))))

	   (method->emit-convert-result
	    (let* ((prop:return-type
		    (clr/%get-property type:methodinfo "ReturnType" '#())))
	      (lambda (method-info)
		(let* ((ret-type
			(clr/%property-ref prop:return-type method-info '#())))
		  (cond ((void-type? ret-type)
			 (lambda (ilgen)
			   (emit:ldsfld ilgen field:false)
			   (emit:stsfld ilgen field:result-reg)))
			((value-type? ret-type)
			 (lambda (ilgen)
			   (emit:box ilgen ret-type)
			   (emit:call ilgen method:makeForeignBox)
			   (emit:stsfld ilgen field:result-reg)))
			(else
			 (lambda (ilgen)
			   (emit:call ilgen method:makeForeignBox)
			   (emit:stsfld ilgen field:result-reg))))))))
	   (static->segment 
	    (lambda (method-info) 
	      (let* ((name (method->name method-info))
		     (param-types (method->param-types method-info)))
		(name-and-params-and-call-and-convert->segment 
		 name
		 #f
		 param-types
		 (lambda (ilgen)
		   (emit:call ilgen method-info))
		 (method->emit-convert-result method-info)
		 ))))
	   (virtual->segment 
	    (lambda (method-info) 
	      (let* ((name (method->name method-info))
		     (param-types (method->param-types method-info))
		     (self-type (method->declaring-type method-info)))
		(name-and-params-and-call-and-convert->segment
		 name
		 self-type
		 param-types
		 (lambda (ilgen)
		   (emit:callvirt ilgen method-info))
		 (method->emit-convert-result method-info)
		 ))))
	   (nonvirtual->segment
	    (lambda (method-info) 
	      (let* ((name (method->name method-info))
		     (param-types (method->param-types method-info))
		     (self-type (method->declaring-type method-info)))
		(name-and-params-and-call-and-convert->segment
		 name
		 self-type
		 param-types
		 (lambda (ilgen)
		   (emit:call ilgen method-info))
		 (method->emit-convert-result method-info)
		 ))))
	   (type:constructorinfo 
	    (find-clr-type "System.Reflection.ConstructorInfo"))
	   (constructor->name ; : ConstructorInfo -> String
	    (let* ((prop:name
		    (clr/%get-property type:constructorinfo "Name" '#())))
	      (lambda (constructor-info)
		(clr/foreign->string
		 (clr/%property-ref prop:name constructor-info '#())))))
	   (constructor->param-types
	    (let* ((type:parameter-info (get-refl-type "ParameterInfo"))
		   (prop:parameter-type
		    (clr/%get-property type:parameter-info
				       "ParameterType" '#()))
		   (get-parameters-method 
		    (clr/%get-method type:constructorinfo
				     "GetParameters" '#())))
	      (lambda (constructor-info)
		(let* ((param-arr (clr/%invoke get-parameters-method 
					       constructor-info '#()))
		       (params (clr-array->list param-arr))
		       (param-types
			(map (lambda (param)
			       (clr/%property-ref prop:parameter-type 
						  param '#()))
			     params)))
		  param-types))))
	   (constructor->segment
	    (lambda (constructor-info)
	      (let* ((name (constructor->name constructor-info))
		     (param-types
		      (constructor->param-types constructor-info)))
		(name-and-params-and-call-and-convert->segment
		 name
		 #f
		 param-types
		 (lambda (ilgen)
		   (emit:newobj ilgen constructor-info))
		 (lambda (ilgen)
		   (emit:call ilgen method:makeForeignBox)
		   (emit:stsfld ilgen field:result-reg))))))
	   (method-info->segment
	    (lambda (method-info)
	      (cond ((is-static? method-info)
		     (static->segment method-info))
		    ((and (is-virtual? method-info)
			  (not (value-type? 
				(method->declaring-type method-info))))
		     (virtual->segment method-info))
		    (else
		     (nonvirtual->segment method-info)))))
	   (type-matcher
	    (let* ((is-instance-method
		    (clr/%get-method type:type "IsInstanceOfType"
				     (vector type:object))))
	      (lambda (type)
		(lambda (obj)
		  (clr/foreign->bool 
		   (clr/%invoke is-instance-method type (vector obj)))))))
	   (is-method-info? (type-matcher type:methodinfo))
	   (is-constructor-info? (type-matcher type:constructorinfo))
	   (member-info->segment
	    (lambda (member-info)
	      (cond ((is-method-info? member-info)
		     (method-info->segment member-info))
		    ((is-constructor-info? member-info)
		     (constructor->segment member-info))
		    (else
		     (error 'clr-method->procedure ": invalid info"
			    member-info)))))
	   )

      (define clr-method->procedure/lcg
	(lambda (member-info . link-segment)
	  (cond ((not member-info)
		 (error 'clr-method->procedure 
			": requires clr-method as input")))
	  (let* ((default-linker
		   (lambda (segment)
		     (let* ((p (make-procedure 3))
			    (code (cond ((assq 'code segment) => cadr)))
			    (name (cond ((assq 'name segment) => cadr)))
			    (arity (cond ((assq 'arity segment) => cadr)))
			    (constant-vector
			     ;; see procinfo.sch for doc layout
			     `#(#(,name #f ,arity #f #f))))
		       (cond 
			((and code constant-vector)
			 (procedure-set! p 0 code)
			 (procedure-set! p 1 constant-vector)
			 (procedure-set! p 2 #f)
			 p)
			(else
			 (error 'clr-method->procedure 
				": clr-method conversion failed" 
				member-info))))))
		 (segment (member-info->segment member-info))
		 (result (cond ((not (null? link-segment))
				((car link-segment) segment))
			       (else
				(default-linker segment)))))
	    result)))

      clr-method->procedure/lcg)))

(define clr-method->procedure clr-method->procedure/lcg)
