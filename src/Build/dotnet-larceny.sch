;; This script depends on having the compiler (driver-larceny.sch version),
;; Util/seal-twobit.sch, Asm/IL/il-corememory.sch, and the Debugger/ files 
;; loaded.
;;
;; It seals off the Twobit compiler internals, installs the debugger, 
;; and replaces the load-evaluator with one that does CompileOnLoad.
 

(let ((proc-names 
       (append 
        
        ;; From Asm/IL/il-corememory.sch
        '(link-lop-segment/clr 
          eval/clr 
          load-lop/clr 
          with-saving-assembly-to-dll/full-control 
          with-saving-assembly-to-dll 
          compile-file/clr) 
        
        ;; From Debugger/debug.sch
        '(install-debugger)
        
        ;; From Lib/MzScheme/dotnet.sch and Lib/MzScheme/init.sch
        ;; Javadot support.  Need the procedure to enable it,
        ;; as well as the supporting syntax definition for
        ;; (.javadot FORM); see eval expression below.
        ;; (See dotnet-ffi group as well)
        '(
          dot-javadot-syntax-definition
          )
        
        ;; dotnet-ffi
        '(
        *dotnet-noise-level*
        allocate-clr-array
        clr-app-domain/%current-domain ;; PROCEDURE
        clr-app-domain/%get-assemblies ;; PROCEDURE
        clr-array->list
        clr-array/length
        clr-assembly/%get-type         ;; PROCEDURE
        clr-binding-flags/instance
        clr-binding-flags/non-public
        clr-binding-flags/public
        clr-binding-flags/static
        clr-convert/%change-type       ;; PROCEDURE
        clr-enum/%get-names            ;; PROCEDURE
        clr-enum/%get-values           ;; PROCEDURE
        clr-enum/get-names
        clr-enum/get-values
        clr-enum/to-object
        clr-field-info/%get-value      ;; PROCEDURE
        clr-fieldinfo/%field-type
        clr-fieldinfo/is-init-only?
        clr-fieldinfo/is-literal?
        clr-fieldinfo/is-static?
        clr-guid/%new-guid             ;; PROCEDURE
        clr-member-type/constructor
        clr-member-type/custom
        clr-member-type/event
        clr-member-type/field
        clr-member-type/method
        clr-member-type/nested-type
        clr-member-type/property
        clr-member-type/type-info
        clr-memberinfo/%declaring-type
        clr-memberinfo/%name
        clr-memberinfo/%reflected-type
        clr-memberinfo/member-type
        clr-memberinfo/name
        clr-methodbase/%get-parameters
        clr-methodbase/is-public?
        clr-methodbase/is-static?
        clr-methodinfo/%return-type
        clr-methodinfo/contains-generic-parameters?
        clr-parameterinfo/%default-value
        clr-parameterinfo/%parameter-type
        clr-parameterinfo/is-optional?
        clr-propertyinfo/%get-get-method
        clr-propertyinfo/%get-index-parameters
        clr-propertyinfo/%property-type
        clr-propertyinfo/can-read?
        clr-propertyinfo/can-write?
        clr-type-handle/scheme-rt-ffi
        clr-type-handle/system-appdomain
        clr-type-handle/system-array
        clr-type-handle/system-boolean
        clr-type-handle/system-byte
        clr-type-handle/system-char
        clr-type-handle/system-convert
        clr-type-handle/system-double
        clr-type-handle/system-enum
        clr-type-handle/system-guid
        clr-type-handle/system-int16
        clr-type-handle/system-int32
        clr-type-handle/system-int64
        clr-type-handle/system-object
        clr-type-handle/system-reflection-assembly
        clr-type-handle/system-reflection-bindingflags
        clr-type-handle/system-reflection-constructorinfo
        clr-type-handle/system-reflection-emit-constructorbuilder
        clr-type-handle/system-reflection-emit-methodbuilder
        clr-type-handle/system-reflection-fieldinfo
        clr-type-handle/system-reflection-memberinfo
        clr-type-handle/system-reflection-membertypes
        clr-type-handle/system-reflection-methodbase
        clr-type-handle/system-reflection-methodinfo
        clr-type-handle/system-reflection-parameterinfo
        clr-type-handle/system-reflection-propertyinfo
        clr-type-handle/system-sbyte
        clr-type-handle/system-single
        clr-type-handle/system-string
        clr-type-handle/system-type
        clr-type-handle/system-uint16
        clr-type-handle/system-uint32
        clr-type-handle/system-uint64
        clr-type-handle/system-void
        clr-type/%assembly
        clr-type/%assembly-qualified-name
        clr-type/%attributes
        clr-type/%base-type
        clr-type/%full-name
        clr-type/%get-custom-attributes
        clr-type/%get-element-type
        clr-type/%get-interfaces
        clr-type/%get-members
        clr-type/contains-generic-parameters?
        clr-type/get-custom-attributes
        clr-type/is-enum?
        clr-type/is-generic?
        clr-type/is-special-name?

        clr/%type-as-string
        clr/bool->foreign
        clr/false
        clr/flonum->foreign-double
        clr/flonum->foreign-single
        clr/foreign->bool
        clr/foreign->char
        clr/foreign->int
        clr/foreign->schemeobject
        clr/foreign->string
        clr/foreign->symbol
        clr/foreign-double->flonum
        clr/foreign-single->flonum
        clr/int->foreign
        clr/null
        clr/null?
        clr/parse-enum
        clr/string->foreign
        clr/symbol->foreign
        clr/true
        clr/type-not-found
        dotnet-message
        find-clr-type
        map-clr-array
        clr/%foreign-aset
        )

        ;; dotnet
        `(
        <clr-arity-overload>
        <clr-generic>
        <clr-instance-field-getter>
        <clr-instance-field-setter>
        <clr-method>
        <clr-static-field-getter>
        <clr-static-field-setter>
        ,(string->symbol "System.Object")
        ,(string->symbol "System.RuntimeType")
        ,(string->symbol "System.Type")
        argument-marshaler
        argument-specializer
        clr-arity-overload?
        clr-dynamic-cast
        clr-object->clr-instance
        clr-object/clr-handle
        clr-object/potential-types
        ,(string->symbol "clr/StudlyName")
        clr/default-marshal-in
        clr/default-marshal-out
        clr/find-class
        clr/find-constructor
        clr/find-generic
        clr/find-instance-field-getter
        clr/find-instance-field-setter
        clr/find-static-field-getter
        clr/find-static-field-setter
        clr/find-static-method
        clr/specific-method
        enable-dotnet!
        enum/enumerates
        enum/has-flags-attribute?
        enum/value
        get-arity-vector
        list-clr-classes
        return-marshaler
        wrap-clr-object
        %foreign?
        )


        '(
        %clr-array?
        %clr-double?
        %clr-enum?
        %clr-int32?
        %clr-single?
        %clr-string?
        %clr-type?
        clr/%add-event-handler
        clr/%clr-version
        clr/%eq?
        clr/%ffi-version
        clr/%field-ref
        clr/%field-set!
        clr/%flonum->foreign-double
        clr/%flonum->foreign-single
        clr/%foreign->bytes
        clr/%foreign->int
        clr/%foreign->object
        clr/%foreign->schemeobject
        clr/%foreign->string
        clr/%foreign->void
        clr/%foreign-aref
        clr/%foreign-box
        clr/%foreign-double->flonum
        clr/%foreign-single->flonum
        clr/%get-constructor
        clr/%get-field
        clr/%get-method
        clr/%get-property
        clr/%get-type
        clr/%invoke
        clr/%invoke-constructor
        clr/%isa?
        clr/%null?
        clr/%number->foreign-byte
        clr/%number->foreign-int16
        clr/%number->foreign-int32
        clr/%number->foreign-sbyte
        clr/%number->foreign-uint16
        clr/%number->foreign-uint32
        clr/%object-type
        clr/%procedure->message-filter
        clr/%property-ref
        clr/%property-ref-bool
        clr/%property-ref-int
        clr/%property-ref-intptr-int
        clr/%property-ref-window
        clr/%property-set!
        clr/%string->foreign
        clr/%to-string
        )
        
        standard-proc-names))
      )
       

  (seal-twobit proc-names))

(eval dot-javadot-syntax-definition)

(install-debugger)

; It's necessary to set the interaction environment so that any uses of 
; EVAL in the loaded file will reference the correct environment.

(define new-load-eval
  (lambda (expr env)
    (let ((old-env (interaction-environment)))
      (define (literal? x)
        (or (procedure? x) (number? x) (string? x)))
      (dynamic-wind 
          (lambda ()
            (interaction-environment env))
          (lambda ()
            ;; Filter out procedure literals (.fasl files)
            ;; Keep in sync w/ dump-fasl in Asm/IL/dumpheap-extra.sch
            (if (and (pair? expr) 
                     (pair? (car expr))
                     (eq? '@common-patch-procedure (caar expr))
                     (every? literal? (cdar expr)))
                (let ((proc
                       (apply (eval '@common-patch-procedure env)
                              (cdar expr))))
                  (proc))
                (eval/clr expr env)))
          (lambda ()
            (if (eq? (interaction-environment) env)
                (interaction-environment old-env)))))))

(load-evaluator new-load-eval)
