;; -*-Mode: Scheme; coding: iso-8859-1 -*-

;($$trace "dotnet-ffi")

(define *dotnet-noise-level* #f)

(define (dotnet-message message-level text . objects)
  (if (and *dotnet-noise-level*
           (number? *dotnet-noise-level*)
           (>= *dotnet-noise-level* message-level))
      (begin
        (display "dotnet ")
        (display message-level)
        (display ": ")
        (do ((i 0 (+ i 1)))
            ((>= i message-level) (display text))
          (display " "))
        (for-each (lambda (object)
                    (display " ")
                    (display object))
                  objects)
        (newline))))

;;; The syscalls that make this work.

(define-syntax define-syscall
  (syntax-rules ()
    ((define-syscall name code ...)
     (define-syntax name
       (syntax-rules ()
         ((name . args)
          (syscall code ... . args)))))))

(define-syscall clr/%get-type          34 0)
(define-syscall clr/%get-method        34 1)

(define-syscall clr/%object->foreign       34 2 0)
(define-syscall clr/%schemeobject->foreign 34 2 1)
(define-syscall clr/%string->foreign       34 2 2)
;(define-syscall clr/%symbol->foreign      34 2 3)
(define-syscall clr/%bytes->foreign        34 2 4)
(define-syscall clr/%int32->foreign        34 2 5)
(define-syscall clr/%flonum->foreign       34 2 6)
(define-syscall clr/%double->foreign       34 2 7)
;(define-syscall clr/%void->foreign        34 2 8)

(define-syscall clr/%foreign->object       34 3 0)
(define-syscall clr/%foreign->schemeobject 34 3 1)
(define-syscall clr/%foreign->string       34 3 2)
;(define-syscall clr/%foreign->symbol      34 3 3)
(define-syscall clr/%foreign->bytes        34 3 4)
(define-syscall clr/%foreign->int          34 3 5)
(define-syscall clr/%foreign->flonum       34 3 6)
(define-syscall clr/%foreign->double       34 3 7)
(define-syscall clr/%foreign->void         34 3 8)

(define-syscall clr/%invoke                34 4)
(define-syscall clr/%get-field             34 5)
(define-syscall clr/%get-property          34 6)
(define-syscall clr/%isa?                  34 7)
(define-syscall clr/%field-ref             34 8)
(define-syscall clr/%field-set!            34 9)
(define-syscall %foreign?                  34 10)
(define-syscall clr/%get-constructor       34 11)
(define-syscall clr/%invoke-constructor    34 12)
(define-syscall clr/%eq?                   34 13)
(define-syscall clr/%property-ref          34 14)
(define-syscall clr/%property-set!         34 15)
(define-syscall clr/%to-string             34 16)
(define-syscall clr/%property-ref-bool     34 17)
(define-syscall clr/%property-ref-int      34 18)
(define-syscall clr/%foreign-aref          34 19)
(define-syscall clr/%object-type           34 20)

;;; Basic types needed to boostrap the rest of the dotnet interface.
(define clr-type-handle/scheme-rt-ffi                   (clr/%get-type "Scheme.RT.FFI"))
(define clr-type-handle/system-appdomain                (clr/%get-type "System.AppDomain"))
(define clr-type-handle/system-array                    (clr/%get-type "System.Array"))
(define clr-type-handle/system-boolean                  (clr/%get-type "System.Boolean"))
(define clr-type-handle/system-byte                     (clr/%get-type "System.Byte"))
(define clr-type-handle/system-char                     (clr/%get-type "System.Char"))
(define clr-type-handle/system-convert                  (clr/%get-type "System.Convert"))
(define clr-type-handle/system-enum                     (clr/%get-type "System.Enum"))
(define clr-type-handle/system-int16                    (clr/%get-type "System.Int16"))
(define clr-type-handle/system-int32                    (clr/%get-type "System.Int32"))
(define clr-type-handle/system-int64                    (clr/%get-type "System.Int64"))
(define clr-type-handle/system-object                   (clr/%get-type "System.Object"))
(define clr-type-handle/system-reflection-assembly      (clr/%get-type "System.Reflection.Assembly"))
(define clr-type-handle/system-reflection-bindingflags  (clr/%get-type "System.Reflection.BindingFlags"))
(define clr-type-handle/system-reflection-fieldinfo     (clr/%get-type "System.Reflection.FieldInfo"))
(define clr-type-handle/system-reflection-memberinfo    (clr/%get-type "System.Reflection.MemberInfo"))
(define clr-type-handle/system-reflection-membertypes   (clr/%get-type "System.Reflection.MemberTypes"))
(define clr-type-handle/system-reflection-methodbase    (clr/%get-type "System.Reflection.MethodBase"))
(define clr-type-handle/system-reflection-methodinfo    (clr/%get-type "System.Reflection.MethodInfo"))
(define clr-type-handle/system-reflection-parameterinfo (clr/%get-type "System.Reflection.ParameterInfo"))
(define clr-type-handle/system-reflection-propertyinfo  (clr/%get-type "System.Reflection.PropertyInfo"))
(define clr-type-handle/system-sbyte                    (clr/%get-type "System.SByte"))
(define clr-type-handle/system-string                   (clr/%get-type "System.String"))
(define clr-type-handle/system-type                     (clr/%get-type "System.Type"))
(define clr-type-handle/system-uint16                   (clr/%get-type "System.UInt16"))
(define clr-type-handle/system-uint32                   (clr/%get-type "System.UInt32"))
(define clr-type-handle/system-uint64                   (clr/%get-type "System.UInt64"))
(define clr-type-handle/system-void                     (clr/%get-type "System.Void"))

(define clr/false
  (clr/%field-ref (clr/%get-field clr-type-handle/scheme-rt-ffi "FALSE") #f))
(define clr/true
  (clr/%field-ref (clr/%get-field clr-type-handle/scheme-rt-ffi "TRUE") #f))
(define clr/null
  (clr/%field-ref (clr/%get-field clr-type-handle/scheme-rt-ffi "NULL") #f))

(define-syntax clr/%null?
  (syntax-rules ()
    ((clr/%null? form)
     (clr/%eq? form clr/null))))

(define (clr/null? object) (clr/%null? object))

(define (clr/bool->foreign   obj) (if obj clr/true clr/false))
(define (clr/double->foreign obj) (clr/%double->foreign obj))
(define (clr/int->foreign    obj) (clr/%int32->foreign obj))
(define (clr/symbol->foreign obj) (clr/%string->foreign (symbol->string obj)))
(define (clr/foreign->bool   obj) (not (clr/%eq? obj clr/false)))
(define (clr/foreign->char   obj) (integer->char (clr/%foreign->int obj)))
(define (clr/foreign->double obj) (clr/%foreign->double obj))
(define (clr/foreign->float  obj) (clr/%foreign->flonum obj))
(define (clr/foreign->int    obj) (clr/%foreign->int obj))
(define (clr/foreign->string obj) (clr/%foreign->string obj))
(define (clr/foreign->symbol obj) (string->symbol (clr/%foreign->string obj)))

(define (clr/%type-as-string obj)
  (clr/%to-string (clr/%object-type obj)))

;;; Some primitive predicates
(define-syntax define-ffi-predicate
  (syntax-rules ()
    ((define-ffi-predicate name type-handle)
     (define-syntax name
       (syntax-rules ()
         ((name object) (clr/%isa? object type-handle)))))))

(define-ffi-predicate %clr-array?      clr-type-handle/system-array)
(define-ffi-predicate %clr-enum?       clr-type-handle/system-enum)
(define-ffi-predicate %clr-int32?      clr-type-handle/system-int32)
(define-ffi-predicate %clr-string?     clr-type-handle/system-string)
(define-ffi-predicate %clr-type?       clr-type-handle/system-type)

(define-syntax define-clr-property
  (syntax-rules ()
    ((define-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name)))
         (lambda (foreign-object)
           (clr/%property-ref handle foreign-object '#())))))))

(define-syntax define-boolean-clr-property
  (syntax-rules ()
    ((define-boolean-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name)))
         (lambda (foreign-object)
           (clr/%property-ref-bool handle foreign-object '#())))))))

(define-syntax define-int-clr-property
  (syntax-rules ()
    ((define-int-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name)))
         (lambda (foreign-object)
           (clr/%property-ref-int handle foreign-object '#())))))))

(define-clr-property         clr-app-domain/%current-domain
  clr-type-handle/system-appdomain "CurrentDomain")

(define-int-clr-property     clr-array/length
  clr-type-handle/system-array "Length")

(define-boolean-clr-property clr-fieldinfo/is-init-only?
  clr-type-handle/system-reflection-fieldinfo "IsInitOnly")
(define-boolean-clr-property clr-fieldinfo/is-literal?
  clr-type-handle/system-reflection-fieldinfo "IsLiteral")
(define-boolean-clr-property clr-fieldinfo/is-static?
  clr-type-handle/system-reflection-fieldinfo "IsStatic")
(define-clr-property         clr-fieldinfo/%field-type
  clr-type-handle/system-reflection-fieldinfo "FieldType")

(define-clr-property         clr-memberinfo/%declaring-type
  clr-type-handle/system-reflection-memberinfo "DeclaringType")
(define-clr-property         clr-memberinfo/%name
  clr-type-handle/system-reflection-memberinfo "Name")

(define (clr-memberinfo/name memberinfo)
  (clr/foreign->string (clr-memberinfo/%name memberinfo)))

(define-int-clr-property     clr-memberinfo/member-type
  clr-type-handle/system-reflection-memberinfo "MemberType")
(define-clr-property         clr-memberinfo/%reflected-type
  clr-type-handle/system-reflection-memberinfo "ReflectedType")

(define-boolean-clr-property clr-methodbase/is-public?
  clr-type-handle/system-reflection-methodbase "IsPublic")
(define-boolean-clr-property clr-methodbase/is-static?
  clr-type-handle/system-reflection-methodbase "IsStatic")

(define-clr-property         clr-methodinfo/%return-type
  clr-type-handle/system-reflection-methodinfo "ReturnType")

(define-clr-property         clr-parameterinfo/%default-value
  clr-type-handle/system-reflection-parameterinfo "DefaultValue")
(define-boolean-clr-property clr-parameterinfo/is-optional?
  clr-type-handle/system-reflection-parameterinfo "IsOptional")
(define-clr-property         clr-parameterinfo/%parameter-type
  clr-type-handle/system-reflection-parameterinfo "ParameterType")

(define-boolean-clr-property clr-propertyinfo/can-read?
  clr-type-handle/system-reflection-propertyinfo "CanRead")
(define-boolean-clr-property clr-propertyinfo/can-write?
  clr-type-handle/system-reflection-propertyinfo "CanWrite")
(define-clr-property         clr-propertyinfo/%property-type
  clr-type-handle/system-reflection-propertyinfo "PropertyType")

(define-clr-property         clr-type/%attributes
  clr-type-handle/system-type "Attributes")
(define-clr-property         clr-type/%assembly
  clr-type-handle/system-type "Assembly")
(define-clr-property         clr-type/%base-type
  clr-type-handle/system-type "BaseType")
(define-boolean-clr-property clr-type/is-enum?
  clr-type-handle/system-type "IsEnum")

(define (map-clr-array proc handle)
  (if (%clr-array? handle clr-type-handle/system-array)
      (let loop ((result '())
                 (idx     0)
                 (limit  (clr-array/length handle)))
        (if (>= idx limit)
            (reverse! result)
            (loop (cons (proc (clr/%foreign-aref handle idx)) result)
                  (+ idx 1)
                  limit)))
      (error "map-foreign-array: not a foreign array" handle)))

;;; Some bootstrap Enums
(define clr/parse-enum
  (let* ((method-handle (clr/%get-method clr-type-handle/system-enum "Parse"
                                         (vector clr-type-handle/system-type
                                                 clr-type-handle/system-string
                                                 clr-type-handle/system-boolean))))
    (lambda (enum-type name)
      (clr/%foreign->int
       (clr/%invoke method-handle #f
                    (vector enum-type
                            (clr/%string->foreign name)
                            clr/true))))))

;;; System.Reflection.BindingFlags enumeration
(define clr-binding-flags/instance
  (clr/parse-enum clr-type-handle/system-reflection-bindingflags "Instance"))
(define clr-binding-flags/static
  (clr/parse-enum clr-type-handle/system-reflection-bindingflags "Static"))
(define clr-binding-flags/public
  (clr/parse-enum clr-type-handle/system-reflection-bindingflags "Public"))
(define clr-binding-flags/non-public
  (clr/parse-enum clr-type-handle/system-reflection-bindingflags "NonPublic"))

;;; System.Reflection.MemberTypes enumeration
(define clr-member-type/constructor
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Constructor"))
(define clr-member-type/event
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Event"))
(define clr-member-type/field
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Field"))
(define clr-member-type/method
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Method"))
(define clr-member-type/property
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Property"))
(define clr-member-type/type-info
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "TypeInfo"))
(define clr-member-type/custom
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "Custom"))
(define clr-member-type/nested-type
  (clr/parse-enum clr-type-handle/system-reflection-membertypes "NestedType"))

(define-syntax define-clr-method
  (syntax-rules ()
    ((define-clr-method (name) type-handle method-name)
     (define name
       (let ((method-handle (clr/%get-method type-handle method-name '#())))
         (lambda (object)
           (clr/%invoke method-handle object '#())))))))

(define-clr-method (clr-app-domain/%get-assemblies)
  clr-type-handle/system-appdomain "GetAssemblies")

(define-clr-method (clr-methodbase/%get-parameters)
  clr-type-handle/system-reflection-methodbase "GetParameters")

(define-clr-method (clr-propertyinfo/%get-index-parameters)
  clr-type-handle/system-reflection-propertyinfo "GetIndexParameters")

(define-clr-method (clr-type/%get-element-type)
  clr-type-handle/system-type "GetElementType")

;;; Methods that have unusual calling sequences.

(define clr-assembly/%get-type
  (let ((method (clr/%get-method clr-type-handle/system-reflection-assembly "GetType"
                                 (vector clr-type-handle/system-string
                                         clr-type-handle/system-boolean
                                         clr-type-handle/system-boolean))))
    (lambda (assembly typename)
      (clr/%invoke method assembly (vector typename clr/false clr/true)))))

(define clr-convert/%change-type
  (let ((method (clr/%get-method clr-type-handle/system-convert "ChangeType"
                                 (vector clr-type-handle/system-object
                                         clr-type-handle/system-type))))
    (lambda (object new-type)
      (clr/%invoke method #f (vector object new-type)))))

(define clr-enum/%get-names
  (let ((method (clr/%get-method clr-type-handle/system-enum "GetNames"
                                 (vector clr-type-handle/system-type))))
    (lambda (object)
      (clr/%invoke method #f (vector object)))))

(define (clr-enum/get-names enum)
  (map-clr-array clr/foreign->string (clr-enum/%get-names enum)))

(define clr-enum/%get-values
  (let ((method (clr/%get-method clr-type-handle/system-enum "GetValues"
                                 (vector clr-type-handle/system-type))))
    (lambda (object)
      (clr/%invoke method #f (vector object)))))

(define (clr-enum/get-values enum)
  (map-clr-array clr/foreign->int (clr-enum/%get-values enum)))

;;; For marshaling integers to enums.
(define clr-enum/to-object
  (let ((method-handle (clr/%get-method clr-type-handle/system-enum "ToObject"
                                        (vector clr-type-handle/system-type
                                                clr-type-handle/system-int32))))
    (lambda (class-handle number)
      (clr/%invoke method-handle #f (vector class-handle number)))))

(define clr-field-info/%get-value
  (let ((method (clr/%get-method clr-type-handle/system-reflection-fieldinfo "GetValue"
                                 (vector clr-type-handle/system-object))))
    (lambda (object)
      (clr/%invoke method object (vector clr/null)))))

;;; TRUE argument means fetch private name, false means public only.
(define clr-propertyinfo/%get-get-method
  (let ((method-handle (clr/%get-method clr-type-handle/system-reflection-propertyinfo "GetGetMethod"
                                        (vector clr-type-handle/system-boolean)))
        (private-arglist (vector clr/true))
        (public-arglist  (vector clr/false)))
    (lambda (property-info allow-private?)
      (clr/%invoke method-handle property-info
                   (if allow-private? private-arglist public-arglist)))))

(define clr-type/%get-custom-attributes
  (let ((method (clr/%get-method clr-type-handle/system-type "GetCustomAttributes"
                                 (vector clr-type-handle/system-boolean)))
        (args (vector clr/true)))
    (lambda (object)
      (clr/%invoke method object args))))

(define (clr-type/get-custom-attributes object)
  (let ((attributes (clr-type/%get-custom-attributes object)))
    (if (clr/%null? attributes)
        '()
        (map-clr-array (lambda (thing)
                         (string->symbol (clr/%to-string thing)))
                       attributes))))

(define clr-type/%get-members
  (let ((method (clr/%get-method clr-type-handle/system-type "GetMembers"
                                 (vector clr-type-handle/system-reflection-bindingflags)))
        (arglist-instance-public
         (vector
          (clr-enum/to-object clr-type-handle/system-reflection-bindingflags
                              (logior clr-binding-flags/instance
                                      clr-binding-flags/public))))
        (arglist-instance-non-public
         (vector
          (clr-enum/to-object clr-type-handle/system-reflection-bindingflags
                              (logior clr-binding-flags/instance
                                      clr-binding-flags/non-public))))
        (arglist-static-public
         (vector
          (clr-enum/to-object clr-type-handle/system-reflection-bindingflags
                              (logior clr-binding-flags/static
                                      clr-binding-flags/public))))
        (arglist-static-non-public
         (vector
          (clr-enum/to-object clr-type-handle/system-reflection-bindingflags
                              (logior clr-binding-flags/static
                                      clr-binding-flags/non-public)))))
    (lambda (type static? public?)
      (clr/%invoke method type (if static?
                                   (if public?
                                       arglist-static-public
                                       arglist-static-non-public)
                                   (if public?
                                       arglist-instance-public
                                       arglist-instance-non-public))))))

(define (find-clr-type clr-type-name)
  (let ((canonical-name
         (cond ((string? clr-type-name) clr-type-name)
               ((symbol? clr-type-name) (symbol->string clr-type-name))
               (else (error "Cannot find clr type " clr-type-name)))))

    (or (clr/%get-type canonical-name)
        (let ((assemblies (clr-app-domain/%get-assemblies (clr-app-domain/%current-domain clr/false)))
              (name-as-string (clr/%string->foreign canonical-name)))
          (if (not (%clr-array? assemblies))
              (error "Couldn't find-clr-type " canonical-name)
              (let loop ((idx 0)
                         (limit (clr-array/length assemblies)))
                (if (>= idx limit)
                    (error "Couldn't find-clr-type " canonical-name)
                    (let* ((this-assembly (clr/%foreign-aref assemblies idx))
                           (probe (clr-assembly/%get-type this-assembly name-as-string)))
                      (if (%clr-type? probe)
                          probe
                          (loop (+ idx 1) limit))))))))))

#||
(define (foreign? object) (%foreign object))
(define (ffi:isa? object type) (clr/%isa? object type))
(define (ffi:eq? left right) (or (eq? left right) (clr/%eq? left right)))

(define (foreign-null? object)
  (clr/%eq? object foreign-null))

(define (ffi:get-type name)
  (if (string? name)
      (clr/%get-type name)
      (error "get-type: expected a string, got " name)))

(define (ffi:get-method type name argtypes)
  (if (not (and (%foreign? type) (clr/%isa? type type%)))
      (error "ffi:get-method: expected type, got: " type))
  (if (not (string? name))
      (error "ffi:get-method: expected string, got: " name))
  (clr/%get-method type name (list->vector argtypes)))


(define (ffi:default-in-marshaler class-name)
  (cond
        ((string=? class-name "System.Byte")   ffi:foreign->int)
        ((string=? class-name "System.Char")   ffi:foreign->int)
        ((string=? class-name "System.Double") ffi:foreign->double)
        ((string=? class-name "System.Int16")  ffi:foreign->int)
        ((string=? class-name "System.Int32")  ffi:foreign->int)
        ((string=? class-name "System.Int64")  ffi:foreign->int)
        ((string=? class-name "System.SByte")  ffi:foreign->int)
        ((string=? class-name "System.Single") ffi:foreign->float)
        ((string=? class-name "System.String") ffi:foreign->symbol)
        ((string=? class-name "System.UInt16") ffi:foreign->int)
        ((string=? class-name "System.UInt32") ffi:foreign->int)
        ((string=? class-name "System.UInt64") ffi:foreign->int)
        (else #f)))

(define (ffi:datum->foreign conversion obj)
  (case conversion
    ((object) (syscall 34 2 0 obj))
    ((schemeobject) (syscall 34 2 1 obj))
    ((string) (syscall 34 2 2 obj))
    ((bytes) (syscall 34 2 4 obj))
    ((int) (syscall 34 2 5 obj))
    ((float) (syscall 34 2 6 obj))
    ((double) (syscall 34 2 7 obj))
    ((void)
     (error "ffi:datum->foreign: void conversion not allowed"))

    ((symbol)
     (if (symbol? obj)
         (ffi:datum->foreign 'string obj)
         (error "ffi:datum->foreign (symbol): expected symbol")))
    ((scheme)
     (if (%foreign? obj) obj (ffi:datum->foreign 'schemeobject obj)))
    ((bool) (if obj foreign-true foreign-false))
    (else
     (error "ffi:datum->foreign: unknown conversion: " conversion))))

(define (ffi:foreign->datum conversion obj)
  (case conversion
    ((object) (syscall 34 3 0 obj))
    ((schemeobject) (syscall 34 3 1 obj))
    ((string) (syscall 34 3 2 obj))
    ((bytes) (syscall 34 3 4 obj))
    ((int) (syscall 34 3 5 obj))
    ((float) (syscall 34 3 6 obj))
    ((double) (syscall 34 3 7 obj))
    ((void) (unspecified))

    ((symbol)
     (string->symbol (ffi:foreign->datum 'string obj)))
    ((scheme)
     (if (and (%foreign? obj) (clr/%isa? obj schemeobject%))
         (ffi:foreign->datum 'schemeobject obj)
         obj))
    ((bool) (ffi:eq? foreign-true obj))
    (else
     (error "ffi:foreign->datum: unknown conversion: " conversion))))

(define (ffi:invoke method obj . args)
  (clr/%invoke method obj (list->vector args)))

(define (ffi:get-constructor type argtypes)
  (clr/%get-constructor type (list->vector argtypes)))

(define (ffi:construct c args)
  (clr/%invoke-constructor c (list->vector args)))

(define (foreign-or-fixnum? x)
  (or (fixnum? x) (%foreign? x)))

(define (ffi:ensure-type t client)
  (cond ((string? t)
         (clr/%get-type t))
        ((and (%foreign? t) (clr/%isa? t type%))
         t)
        (else
         (error client " expected type, got: " t))))

(define ffi:foreign-array-length
  (let ((property (clr/%get-property array% "Length")))
    (lambda (foreign-array)
      (clr/%property-ref-int property foreign-array '#()))))

(define (map-foreign-array proc handle)
  (if (clr/%isa? handle array%)
      (let loop ((result '())
                 (idx     0)
                 (limit  (ffi:foreign-array-length handle)))
        (if (>= idx limit)
            (reverse! result)
            (loop (cons (proc (clr/%foreign-aref handle idx)) result)
                  (+ idx 1)
                  limit)))
      (error "map-foreign-array: not a foreign array" handle)))

;; ----

(define (clr-method->procedure type name argtypes)
  (define (ensure-type t) (ffi:ensure-type t 'clr-method->procedure))
  (let ((type (ensure-type type)))
    (cond ((not (string? name))
           (error "clr-method->procedure: expected string for method name, got: "
                  name))
          ((not (list? argtypes))
           (error "clr-method->procedure: expected list of argument types, got: "
                  argtypes))
          (else
           (let ((argtypes (map ensure-type argtypes)))
             (let ((mi (ffi:get-method type name argtypes)))
               (if (not mi)
                   (error "clr-method->procedure: no such method"))
               (lambda args
                 (apply ffi:invoke mi args))))))))

(define (clr-constructor->procedure type argtypes)
  (define (ensure-type t) (ffi:ensure-type t 'clr-constructor->procedure))
  (let ((type (ensure-type type)))
    (cond ((not (list? argtypes))
           (error
            "clr-constructor->procedure: expected list of argument types, got: "
            argtypes))
          (else
           (let ((argtypes (map ensure-type argtypes)))
             (let ((ci (ffi:get-constructor type argtypes)))
               (if (not ci)
                   (error "clr-constructor->procedure: no such constructor"))
               (lambda args
                 (ffi:construct ci args))))))))

(define (wrap-foreign-procedure p return-c arg-cs)
  (if (not (procedure? p))
      (error "wrap-foreign-procedure: expected procedure, given: " p))
  (lambda args
    (ffi:foreign->datum return-c
                        (apply p (map ffi:datum->foreign arg-cs args)))))

(define (static-method m)
  (lambda args
    (apply m #f args)))

(define (wrap-foreign-property ps obj-c newval-c)
  (lambda (obj . newval)
    (cond ((null? newval)
           (if (car ps)
               (ffi:foreign->datum newval-c
                                   ((car ps) (ffi:datum->foreign obj-c obj)))
               (error "foreign-property: not accessible")))
          (else
           (if (cdr ps)
               ((cdr ps) (ffi:datum->foreign obj-c obj)
                         (ffi:datum->foreign newval-c (car newval)))
               (error "foreign-property: not settable"))))))

(define (clr-field->procedures type name)
  (let ((fi (clr/%get-field type name)))
    (if (not fi)
        (error "clr-field->procedures: no such field"))
    (cons
     (lambda (obj) (clr/%field-ref fi obj))
     (lambda (obj newval) (clr/%field-set! fi obj newval)))))

(define (clr-property->procedures type name)
  (let ((pi (clr/%get-property type name)))
    (if (not pi)
        (error "clr-property->procedures: no such property"))
    (cons
     (lambda (obj . args) (clr/%property-ref pg obj (list->vector args)))
     (lambda (obj newval . args) (clr/%property-set! ps obj newval (list->vector args))))))

;; ----

(define object.to-string
  (wrap-foreign-procedure (clr-method->procedure object% "ToString" '())
                          'string '(scheme)))
(define object.equals
  (wrap-foreign-procedure
   (clr-method->procedure object% "Equals" (list object%))
   'bool '(scheme scheme)))
(define object.get-type
  (wrap-foreign-procedure
   (clr-method->procedure object% "GetType" '())
   'object '(scheme)))

(define string.length$$ (clr-property->procedures string% "Length"))
(define string.length
  (wrap-foreign-property string.length$$ 'string 'int))

(define pair.car$$
  (clr-field->procedures (ffi:get-type "Scheme.Rep.SPair") "first"))
(define pair.car
  (wrap-foreign-property pair.car$$ 'scheme 'scheme))

(define array.length
  (wrap-foreign-property
   (clr-property->procedures array% "Length")
   'object 'int))
(define array.get
  (wrap-foreign-procedure
   (clr-method->procedure array% "GetValue" (list int%))
   'scheme '(object int)))
(define array.set
  (wrap-foreign-procedure
   (clr-method->procedure array% "SetValue" (list object% int%))
   'void '(object object int)))

(define array.new
  (static-method
   (wrap-foreign-procedure
    (clr-method->procedure array% "CreateInstance" (list type% int%))
    'object '(scheme object int))))

(define type.base-type
  (wrap-foreign-property
   (clr-property->procedures type% "BaseType")
   'object 'object))

;; Assemblies

(define assembly% (ffi:get-type "System.Reflection.Assembly"))

(define assembly.get-type
  (wrap-foreign-procedure
   (clr-method->procedure assembly% "GetType" (list string%))
   'object '(object string)))

(define assembly.load
  (static-method
   (wrap-foreign-procedure
    (clr-method->procedure assembly% "Load" (list string%))
    'object '(scheme string))))

(define assembly.load/partial
  (static-method
   (wrap-foreign-procedure
    (clr-method->procedure assembly% "LoadWithPartialName" (list string%))
    'object '(scheme string))))

;; ----
;(define forms@ (assembly.load/partial "System.Windows.Forms"))
;(define control%
;  (assembly.get-type forms@ "System.Windows.Forms.Control"))
;(define form%
;  (assembly.get-type forms@ "System.Windows.Forms.Form"))
;
;(define form.new
;  (clr-constructor->procedure form% '()))
;(define form.show
;  (wrap-foreign-procedure
;   (clr-method->procedure form% "Show" '())
;   'void '(object)))
;(define control.left
;  (wrap-foreign-property
;   (clr-property->procedures control% "Left")
;   'object 'int))
||#
