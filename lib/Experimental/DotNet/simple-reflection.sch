;;; Procedures of interest:
;;; define-type
;;; define-constructor
;;; define-method
;;; constructor->ilgen 
;;; method->ilgen
;;; ilgen->emitter

(define (find-emit-type name)
  (find-clr-type (string-append "System.Reflection.Emit." name)))
(define (find-reflection-type name)
  (find-clr-type (string-append "System.Reflection." name)))

(define appdomain-type (find-clr-type "System.AppDomain"))
(define assemblybuilder-type (find-emit-type "AssemblyBuilder"))

(define list-ref/default 
  (lambda (lst idx def)
    (let loop ((lst lst) (idx idx))
      (cond ((null? lst) def)
            ((zero? idx) (car lst))
            (else (loop (cdr lst) (- idx 1)))))))

(define define-dynamic-assembly
  (let* ((name-type (find-reflection-type "AssemblyName"))
         (name-ctor (clr/%get-constructor name-type '#()))
         (name-prop (clr/%get-property name-type "Name" '#()))
         (thread-type (find-clr-type "System.Threading.Thread"))
         (get-domain-method (clr/%get-method thread-type "GetDomain" '#()))
         (access-type (find-emit-type "AssemblyBuilderAccess"))
         (run-and-save-permissions 
          (clr/%get-field access-type "RunAndSave"))
         (dda-method
          (clr/%get-method appdomain-type "DefineDynamicAssembly" 
                           (vector name-type access-type))))
    (lambda args
      (let* ((name (list-ref/default args 0 "FreshAssembly"))
             (asm-name (clr/%invoke-constructor name-ctor '#()))
             (domain (clr/%invoke get-domain-method #f '#()))
             (perms (clr/%field-ref run-and-save-permissions #f)))
        (clr/%property-set! name-prop asm-name 
                            (clr/%string->foreign name) '#())
        (clr/%invoke dda-method domain (vector asm-name perms))))))

(define modulebuilder-type (find-emit-type "ModuleBuilder"))
(define define-dynamic-module
  (let ((ddm-method 
         (clr/%get-method assemblybuilder-type 
                          "DefineDynamicModule"
                          (vector clr-type-handle/system-string
                                  clr-type-handle/system-string))))
    (lambda args
      (let ((asm-builder (list-ref/default args 0 (define-dynamic-assembly)))
            (mod-name (list-ref/default args 1 "default-module"))
            (dll-file (list-ref/default args 2 "default.dll")))
        (clr/%invoke ddm-method asm-builder 
                     (vector (clr/%string->foreign mod-name) 
                             (clr/%string->foreign dll-file)))))))
(define typebuilder-type (find-emit-type "TypeBuilder"))
(define define-type
  (let* ((typeattrs-types (find-reflection-type "TypeAttributes"))
         (convert (enum-type->symbol->foreign typeattrs-types))
         (default-typeattrs (list 'public 'class))
         (type-type (find-clr-type "System.Type"))
         (dt-method (clr/%get-method modulebuilder-type "DefineType"
                                     (vector clr-type-handle/system-string
                                             typeattrs-types
                                             type-type))))
    (lambda (name . args)
      (let ((parent (list-ref/default args 0 clr-type-handle/system-object))
            (typeattrs (apply convert 
                              (list-ref/default args 1 default-typeattrs)))
            (mod-bldr (list-ref/default args 2 (define-dynamic-module))))
        (clr/%invoke dt-method mod-bldr (vector (clr/%string->foreign name) 
                                                typeattrs 
                                                parent))))))
(define calling-conventions-type
  (find-reflection-type "CallingConventions"))
(define standard-calling-convention
  (let* ((standard-conventions-field 
          (clr/%get-field calling-conventions-type "Standard"))
        (calling-convention
         (clr/%field-ref standard-conventions-field #f)))
    calling-convention))

(define-values (define-constructor define-method)
  (let* ((methattrs-type (find-reflection-type "MethodAttributes"))
         (convert (enum-type->symbol->foreign methattrs-type))
         (default-ctorattrs (list 'public))
         (default-methattrs (list 'public 'virtual))
         (typearray-type (clr/%get-type "System.Type[]"))
         (dc-meth (clr/%get-method typebuilder-type "DefineConstructor"
                                   (vector methattrs-type 
                                           calling-conventions-type
                                           typearray-type)))
	 (dm-meth (clr/%get-method typebuilder-type "DefineMethod"
				   (vector clr-type-handle/system-string
					   methattrs-type
					   calling-conventions-type
					   type-type
					   typearray-type))))
                                           
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

    (define (types->foreign-array types)
      (objects->foreign-array clr-type-handle/system-type types))

    (define (defctor type-bldr . args)
      (let* ((methattrs (apply convert 
                               (list-ref/default args 1 default-ctorattrs)))
             (arg-infos (list-ref/default args 0 '()))

             (arg-infos/array (types->foreign-array arg-infos)))
        (clr/%invoke dc-meth type-bldr (vector methattrs 
                                               standard-calling-convention
                                               arg-infos/array))))

    (define (defmeth type-bldr name . args)
      (let* ((methattrs (apply convert 
			       (list-ref/default args 2 default-methattrs)))
	     (arg-infos (list-ref/default args 0 '()))
	     (arg-infos/array (types->foreign-array arg-infos))
	     (rtn-info (list-ref/default args 1 clr-type-handle/system-void)))
	(clr/%invoke dm-meth type-bldr (vector (clr/%string->foreign name)
					       methattrs
					       standard-calling-convention
					       rtn-info
					       arg-infos/array))))
    (values defctor defmeth)
    ))
   
(define constructorbuilder-type 
  (find-emit-type "ConstructorBuilder"))
(define methodbuilder-type
  (find-emit-type "MethodBuilder"))
(define ilgenerator-type
  (find-emit-type "ILGenerator"))
(define constructor->ilgen 
  (let ((get-il-meth (clr/%get-method constructorbuilder-type
                                      "GetILGenerator"
                                      '#())))
    (lambda (ctor-builder)
      (clr/%invoke get-il-meth ctor-builder '#()))))
(define method->ilgen
  (let ((get-il-meth (clr/%get-method methodbuilder-type
				      "GetILGenerator"
				      '#())))
    (lambda (method-builder)
      (clr/%invoke get-il-meth method-builder '#()))))

(define-syntax let*-loud
  (syntax-rules ()
    ((_ ((ID EXP) ...) BODY ...)
     (let* ((ID (begin (display 'ID) (newline) EXP)) ...) BODY ...))))

(define opcode-type (find-emit-type "OpCode"))
(define opcodes
  (let* ((opcodes-type (find-emit-type "OpCodes"))
         (operand-type (find-emit-type "OperandType"))
         (name-prop    (clr/%get-property opcode-type "Name" '#()))
         (opcode-name  
          (lambda (opc) 
            (clr/foreign->string (clr/%property-ref name-prop opc '#()))))
         (opcodes-fields (type->fields opcodes-type))
         (is-static-prop (clr/%get-property field-info-type "IsStatic" '#()))
         (is-static?
          (lambda (fi)
            (clr/foreign->bool (clr/%property-ref is-static-prop fi '#()))))
         (static-fields (filter is-static? opcodes-fields))
         (field-values (map (lambda (field-info)
                              (list (clr/%field-ref field-info #f)
                                    field-info))
                            static-fields))
         ;; workaround for a bug in Mono; the Readonly field is set to
         ;; an object whose Name property is null (and whose toString
         ;; method causes a NullPtrExn)
         (named-fields 
          (filter (lambda (x) 
                    (not (clr/null? 
                          (clr/%property-ref name-prop (car x) '#()))))
                  field-values))
         (val*typeinfo->entries 
          (lambda (x)
            (let ((symbol-downcase 
                   (lambda (s)
                     (string->symbol (string-downcase (symbol->string s)))))
                  (val (car x))
                  (typeinfo (cadr x)))
              (list (list (string->symbol (opcode-name val)) val)
                    (list (field-info->name typeinfo) val)
                    (list (symbol-downcase (field-info->name typeinfo)) val)
                    ))))
         (opcodes-table
          (apply append 
                 (map val*typeinfo->entries named-fields))))
    (lambda args
      (if (null? args) 
          (map (lambda (x) (string->symbol (opcode-name (car x)))) 
               named-fields)
          (let ((opcode-sym (car args)))
            (cond ((assq opcode-sym opcodes-table) => cadr)
                  (else (error 'opcode ": unknown opcode " opcode-sym))))))
    ))
(define operandtype-type (find-emit-type "OperandType"))
(define opcode-operandtype
  (let ((prop (clr/%get-property opcode-type "OperandType" '#())))
    (lambda (x)
      (clr/%property-ref prop x '#()))))
(define operandtype-name (enum-type->foreign->symbol operandtype-type))

(define ilgen->emitter
  (let* ((get-emit (lambda types (clr/%get-method ilgenerator-type "Emit" (list->vector (cons opcode-type types)))))
         (emit-meth/none   (get-emit))
         (emit-meth/byte   (get-emit clr-type-handle/system-byte))
         (emit-meth/short  (get-emit clr-type-handle/system-int16))
         (emit-meth/int    (get-emit clr-type-handle/system-int32))
         (emit-meth/long   (get-emit clr-type-handle/system-int64))
         (emit-meth/single (get-emit clr-type-handle/system-single))
         (emit-meth/double (get-emit clr-type-handle/system-double))
         (emit-meth/string (get-emit clr-type-handle/system-string))
         (emit-meth/constructor-info
          (get-emit (find-reflection-type "ConstructorInfo")))
         (emit-meth/method-info
          (get-emit (find-reflection-type "MethodInfo")))
         (emit-meth/field-info 
          (get-emit (find-reflection-type "FieldInfo")))
         (emit-meth/label (get-emit (find-emit-type "Label")))
         (emit-meth/label-array (get-emit (find-emit-type "Label[]")))
         (emit-meth/type (get-emit type-type))
         (emitcalli-meth 
          (clr/%get-method 
           ilgenerator-type "EmitCalli"
           (vector opcode-type 
                   (find-reflection-type "CallingConventions")
                   (find-clr-type "System.Type")
                   (find-clr-type "System.Type[]")))))
    (lambda (ilgen)
      (lambda (opcode-sym . args)
        (let* ((opcode (opcodes opcode-sym))
               (operand-type (operandtype-name (opcode-operandtype opcode)))
               (identity (lambda (x) x))
               (invoke0 (lambda (meth) 
                          (clr/%invoke meth ilgen (vector opcode))))
               (invoke1 (lambda (meth conv)
                          (clr/%invoke meth ilgen
                                       (vector opcode (conv (car args)))))))
          (case operand-type 
            ((inlinenone) (invoke0 emit-meth/none))
            ((shortinlinevar shortinlinei)  
             (invoke1 emit-meth/byte clr/byte->foreign))
            ((inlinei)
             (invoke1 emit-meth/int clr/int->foreign))
            ((inlinei8) (error 'emit ": don't have support for int64 yet"))
            ((shortinliner)
             (invoke1 emit-meth/single clr/flonum->foreign-single))
            ((inliner)
             (invoke1 emit-meth/double clr/flonum->foreign-double))
            ((inlinemethod) 
             (invoke1 
              (cond 
               ((clr/%isa? (car args) method-info-type) 
                emit-meth/method-info)
               ((clr/%isa? (car args) constructor-info-type) 
                emit-meth/constructor-info))
             identity))
            ((inlinesig)      
             (let ((ret-type (list-ref args 0))
                   (param-types (list-ref args 1)))
               (clr/%invoke emitcalli-meth ilgen 
                            (vector opcode
                                    standard-calling-convention
                                    ret-type
                                    param-types))))
            ((shortinlinebrtarget inlinebrtarget) 
             (invoke1 emit-meth/label identity))
            ((inlineswitch)   
             (invoke1 emit-meth/label-array identity))
            ((inlinetype)     
             (invoke1 emit-meth/type identity))
            ((inlinestring)   
             (invoke1 emit-meth/string clr/string->foreign))
            ((inlinefield)    
             (invoke1 emit-meth/field-info identity))
            ((inlinetok)     
             (invoke1 
              (cond 
               ((clr/%isa? (car args) method-info-type) 
                emit-meth/method-info)
               ((clr/%isa? (car args) field-info-type)  
                emit-meth/field-info)
               ((clr/%isa? (car args) type-type)        
                emit-meth/type))
              identity))
            ((inlinevar)
             (invoke1 emit-meth/short (lambda (x) (clr/%number->foreign-int16 x))))))))))



