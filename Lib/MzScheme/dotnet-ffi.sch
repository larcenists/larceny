;; -*-Mode: Scheme; coding: iso-8859-1 -*-

;;; This file uses the dotnet system calls to build a low-level
;;; interface to the reflection API of the .NET library.  It is used
;;; primarily by the JavaDot layer implemented in dotnet.sch, but the
;;; low-level window example code (in windows.sch) makes use of it
;;; to achieve better performance.

;;; We also use the NewGuid call to generate unique names for
;;; separately compiled macros.

;;; See the notes at the beginning of dotnet.sch

($$trace "dotnet-ffi")

;; Set this variable to an integer from 0 through 5 to trace execution
;; of the dotnet code.  0 is least verbose, 5 is very detailed.
(define *dotnet-noise-level* (make-parameter "*dotnet-noise-level*" #f))

(define (dotnet-message message-level text . objects)
  (if (and (number? (*dotnet-noise-level*))
           (>= (*dotnet-noise-level*) message-level))
      (begin
        (newline)
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
        (flush-output-port))))

;;; The syscalls that make this work.

(define-syntax define-syscall
  (syntax-rules ()
    ((define-syscall name code ...)
     (begin
     (define-syntax name
;       ;; Very slow, thoroughly traced version.
;       (syntax-rules ()
;         ((name)
;          (begin
;            (dotnet-message 5 "Syscall" 'name)
;            (syscall code ...)))

;         ((name arg1)
;          (let ((value1 arg1))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1)))

;         ((name arg1 arg2)
;          (let ((value1 arg1)
;                (value2 arg2))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2)))

;         ((name arg1 arg2 arg3)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3)))

;         ((name arg1 arg2 arg3 arg4)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3)
;                (value4 arg4))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3 value4)))

;         ((name arg1 arg2 arg3 arg4 arg5)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3)
;                (value4 arg4)
;                (value5 arg5))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3 value4 value5)))
;         )

       ;; Regular version.
       (syntax-rules ()
         ((name . args)
          (syscall code ... . args)))
       )
     (macro-expand
      '(define-syntax name
;       ;; Very slow, thoroughly traced version.
;       (syntax-rules ()
;         ((name)
;          (begin
;            (dotnet-message 5 "Syscall" 'name)
;            (syscall code ...)))

;         ((name arg1)
;          (let ((value1 arg1))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1)))

;         ((name arg1 arg2)
;          (let ((value1 arg1)
;                (value2 arg2))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2)))

;         ((name arg1 arg2 arg3)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3)))

;         ((name arg1 arg2 arg3 arg4)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3)
;                (value4 arg4))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3 value4)))

;         ((name arg1 arg2 arg3 arg4 arg5)
;          (let ((value1 arg1)
;                (value2 arg2)
;                (value3 arg3)
;                (value4 arg4)
;                (value5 arg5))
;            (dotnet-message 5 "Syscall" 'name value1)
;            (syscall code ... value1 value2 value3 value4 value5)))
;         )

       ;; Regular version.
       (syntax-rules ()
         ((name . args)
          (syscall code ... . args)))
       )
      usual-syntactic-environment)))))

(define-syscall clr/%clr-version        34  0)
(define-syscall clr/%ffi-version        34  1)
(define-syscall %foreign?               34  2)
(define-syscall clr/%to-string          34  3)
(define-syscall clr/%object-type        34  4)
(define-syscall clr/%isa?               34  5)
(define-syscall clr/%eq?                34  6)

(define-syscall clr/%get-type           34  7)
(define-syscall clr/%get-field          34  8)
(define-syscall clr/%get-constructor    34  9)
(define-syscall clr/%get-method         34 10)
(define-syscall clr/%get-property       34 11)

(define-syscall clr/%field-ref          34 12)
(define-syscall clr/%field-set!         34 13)
(define-syscall clr/%invoke-constructor 34 14)
(define-syscall clr/%invoke             34 15)
(define-syscall clr/%property-ref       34 16)
(define-syscall clr/%property-set!      34 17)
(define-syscall clr/%foreign-aref       34 18)

(define-syscall clr/%foreign-box            34 19 0)
(define-syscall clr/%string->foreign        34 19 1)
(define-syscall clr/%number->foreign-byte   34 19 2)
(define-syscall clr/%number->foreign-uint16 34 19 3)
(define-syscall clr/%number->foreign-uint32 34 19 4)
(define-syscall clr/%number->foreign-sbyte  34 19 5)
(define-syscall clr/%number->foreign-int16  34 19 6)
(define-syscall clr/%number->foreign-int32  34 19 7)
(define-syscall clr/%procedure->message-filter 34 19 8)
;(define-syscall clr/%void->foreign         34 19 9)

(define-syscall clr/%foreign->object       34 20 0)
(define-syscall clr/%foreign->schemeobject 34 20 1)
(define-syscall clr/%foreign->string       34 20 2)
;(define-syscall clr/%foreign->symbol      34 20 3)
(define-syscall clr/%foreign->bytes        34 20 4)
(define-syscall clr/%foreign->int          34 20 5)
(define-syscall clr/%foreign->flonum       34 20 6)
(define-syscall clr/%foreign->double       34 20 7)
(define-syscall clr/%foreign->void         34 20 8)

;; special for performance
(define-syscall clr/%property-ref-bool     34 21)
(define-syscall clr/%property-ref-int      34 22)
(define-syscall clr/%property-ref-window   34 23)
(define-syscall clr/%property-ref-intptr-int 34 24)

;; Hook this asap to make it easy to debug this file.
;; Using the syscalls directly means that we can turn on
;; the debug printing above but not have it interfere here.
(define print-foreign-object
  (let ((old-weird-printer (weird-printer)))
    (lambda (weird-object port slashify)
      (if (syscall 34 2 weird-object)
          (begin
            (display "#<Foreign " port)
            (display (syscall 34 3 weird-object) port)
            (display ">" port))
          (old-weird-printer weird-object port slashify)))))

;;; Install the new weird printer.
(weird-printer print-foreign-object)

;;; Basic types needed to boostrap the rest of the dotnet interface.
(define clr-type-handle/scheme-rt-ffi                   (clr/%get-type "Scheme.RT.FFI"))
(define clr-type-handle/system-appdomain                (clr/%get-type "System.AppDomain"))
(define clr-type-handle/system-array                    (clr/%get-type "System.Array"))
(define clr-type-handle/system-boolean                  (clr/%get-type "System.Boolean"))
(define clr-type-handle/system-byte                     (clr/%get-type "System.Byte"))
(define clr-type-handle/system-char                     (clr/%get-type "System.Char"))
(define clr-type-handle/system-convert                  (clr/%get-type "System.Convert"))
(define clr-type-handle/system-enum                     (clr/%get-type "System.Enum"))
(define clr-type-handle/system-guid                     (clr/%get-type "System.Guid"))
(define clr-type-handle/system-int16                    (clr/%get-type "System.Int16"))
(define clr-type-handle/system-int32                    (clr/%get-type "System.Int32"))
(define clr-type-handle/system-int64                    (clr/%get-type "System.Int64"))
(define clr-type-handle/system-object                   (clr/%get-type "System.Object"))
(define clr-type-handle/system-reflection-assembly      (clr/%get-type "System.Reflection.Assembly"))
(define clr-type-handle/system-reflection-bindingflags  (clr/%get-type "System.Reflection.BindingFlags"))
(define clr-type-handle/system-reflection-constructorinfo
  (clr/%get-type "System.Reflection.ConstructorInfo"))
(define clr-type-handle/system-reflection-emit-constructorbuilder
  (clr/%get-type "System.Reflection.Emit.ConstructorBuilder"))
(define clr-type-handle/system-reflection-emit-methodbuilder
  (clr/%get-type "System.Reflection.Emit.MethodBuilder"))
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

(macro-expand
 '(define-syntax clr/%null?
    (syntax-rules ()
      ((clr/%null? form)
       (clr/%eq? form clr/null))))
 usual-syntactic-environment)

(define (clr/null? object) (clr/%null? object))
(define (clr/foreign->schemeobject object) (clr/%foreign->schemeobject object))

(define (clr/bool->foreign   obj) (if obj clr/true clr/false))
(define (clr/double->foreign obj) (clr/%double->foreign obj))
(define (clr/int->foreign    obj) (clr/%number->foreign-int32 obj))
(define (clr/string->foreign obj) (clr/%string->foreign obj))
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
     (begin
     (define-syntax name
       (syntax-rules ()
           ((name object) (clr/%isa? object type-handle))))
       (macro-expand
         '(define-syntax name
            (syntax-rules ()
              ((name object) (clr/%isa? object type-handle))))
         usual-syntactic-environment)))))

(define-ffi-predicate %clr-array?      clr-type-handle/system-array)
(define-ffi-predicate %clr-enum?       clr-type-handle/system-enum)
(define-ffi-predicate %clr-int32?      clr-type-handle/system-int32)
(define-ffi-predicate %clr-string?     clr-type-handle/system-string)
(define-ffi-predicate %clr-type?       clr-type-handle/system-type)

(define-syntax define-clr-property
  (syntax-rules ()
    ((define-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name '#())))
         (if handle
             (lambda (foreign-object)
               (clr/%property-ref handle foreign-object '#()))
             (error (string-append "Property "property-name" not found."))))))))

(define-syntax define-boolean-clr-property
  (syntax-rules ()
    ((define-boolean-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name '#())))
         (if handle
             (lambda (foreign-object)
               (clr/%property-ref-bool handle foreign-object '#()))
             (error (string-append "Boolean property " property-name " not found."))))))

    ;; The fourth argument of #T allows us to refer to properties that
    ;; don't exist.  They will seem to always the value #F.  .NET version 2.0
    ;; adds new properties to types.
    ((define-boolean-clr-property name type-handle property-name #t)
     (define name
       (let ((handle (clr/%get-property type-handle property-name '#())))
         (if handle
             (lambda (foreign-object)
               (clr/%property-ref-bool handle foreign-object '#()))
             (lambda (foreign-object)
               #f)))))

    ;; By default, though, error if we can't find the property.
    ((define-boolean-clr-property name type-handle property-name #f)
     (define-boolean-clr-property name type-handle property-name))))

(define-syntax define-int-clr-property
  (syntax-rules ()
    ((define-int-clr-property name type-handle property-name)
     (define name
       (let ((handle (clr/%get-property type-handle property-name '#())))
         (if handle
             (lambda (foreign-object)
               (clr/%property-ref-int handle foreign-object '#()))
             (error (string-append "Integer property "property-name" not found."))))))))

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

(define (clr-memberinfo/name object)
  (clr/%foreign->string (clr-memberinfo/%name object)))

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
(define-boolean-clr-property clr-methodinfo/contains-generic-parameters?
  clr-type-handle/system-reflection-methodinfo "ContainsGenericParameters" #t)

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
(define-clr-property         clr-type/%assembly-qualified-name
  clr-type-handle/system-type "AssemblyQualifiedName")
(define-clr-property         clr-type/%base-type
  clr-type-handle/system-type "BaseType")
(define-clr-property         clr-type/%full-name
  clr-type-handle/system-type "FullName")
(define-boolean-clr-property clr-type/contains-generic-parameters?
  clr-type-handle/system-type "ContainsGenericParameters" #t)
(define-boolean-clr-property clr-type/is-enum?
  clr-type-handle/system-type "IsEnum")
(define-boolean-clr-property clr-type/is-generic?
  clr-type-handle/system-type "IsGenericTypeDefinition" #t)
(define-boolean-clr-property clr-type/is-special-name?
  clr-type-handle/system-type "IsSpecialName")

(define (clr-array->list handle)
  (if (%clr-array? handle)
      (let loop ((result '())
                 (idx     0)
                 (limit  (clr-array/length handle)))
        (if (>= idx limit)
            (reverse! result)
            (loop (cons (clr/%foreign-aref handle idx) result)
                  (+ idx 1)
                  limit)))
      (error "map-clr-array: not a foreign array" handle)))

(define (map-clr-array proc handle)
  (if (%clr-array? handle)
      (let loop ((result '())
                 (idx     0)
                 (limit  (clr-array/length handle)))
        (if (>= idx limit)
            (reverse! result)
            (loop (cons (proc (clr/%foreign-aref handle idx)) result)
                  (+ idx 1)
                  limit)))
      (error "map-clr-array: not a foreign array" handle)))

(define clr/%foreign-aset
  (let ((method-handle (clr/%get-method clr-type-handle/system-array "SetValue"
                                        (vector clr-type-handle/system-object
                                                clr-type-handle/system-int32))))
    (lambda (array idx value)
      (clr/%invoke method-handle array (vector value (clr/int->foreign idx))))))

(define allocate-clr-array
  (let* ((method-handle (clr/%get-method clr-type-handle/system-array "CreateInstance"
                                         (vector clr-type-handle/system-type
                                                 clr-type-handle/system-int32))))
    (lambda (type-handle length)
      (clr/%invoke method-handle #f (vector type-handle (clr/int->foreign length))))))

;;; Some bootstrap Enums
(define clr/parse-enum
  (let* ((method-handle (clr/%get-method clr-type-handle/system-enum "Parse"
                                         (vector clr-type-handle/system-type
                                                 clr-type-handle/system-string
                                                 clr-type-handle/system-boolean))))
    (lambda (enum-type name)
      ;; (dotnet-message 5 "Parse enum" name)
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
         (if method-handle
             (lambda (object)
               (dotnet-message 5 "Invoke method" method-name)
               (clr/%invoke method-handle object '#()))
             (error (string-append "Method "method-name" not found."))))))))

(define-clr-method (clr-app-domain/%get-assemblies)
  clr-type-handle/system-appdomain "GetAssemblies")

(define-clr-method (clr-guid/%new-guid)
  clr-type-handle/system-guid "NewGuid")

(define-clr-method (clr-methodbase/%get-parameters)
  clr-type-handle/system-reflection-methodbase "GetParameters")

(define-clr-method (clr-propertyinfo/%get-index-parameters)
  clr-type-handle/system-reflection-propertyinfo "GetIndexParameters")

(define-clr-method (clr-type/%get-element-type)
  clr-type-handle/system-type "GetElementType")

(define-clr-method (clr-type/%get-interfaces)
  clr-type-handle/system-type "GetInterfaces")

;;; Methods that have unusual calling sequences.

(define clr-assembly/%get-type
  (let ((method (clr/%get-method clr-type-handle/system-reflection-assembly "GetType"
                                 (vector clr-type-handle/system-string
                                         clr-type-handle/system-boolean
                                         clr-type-handle/system-boolean))))
    (lambda (assembly typename)
      ;; (dotnet-message 5 "clr-assembly/%get-type" typename)
      (clr/%invoke method assembly (vector typename clr/false clr/true)))))

(define clr-convert/%change-type
  (let ((method (clr/%get-method clr-type-handle/system-convert "ChangeType"
                                 (vector clr-type-handle/system-object
                                         clr-type-handle/system-type))))
    (lambda (object new-type)
      ;; (dotnet-message 5 "clr-convert/%change-type")
      (clr/%invoke method #f (vector object new-type)))))

(define clr-enum/%get-names
  (let ((method (clr/%get-method clr-type-handle/system-enum "GetNames"
                                 (vector clr-type-handle/system-type))))
    (lambda (object)
      ;; (dotnet-message 5 "clr-enum/%get-names")
      (clr/%invoke method #f (vector object)))))

(define (clr-enum/get-names enum)
  (map-clr-array clr/foreign->string (clr-enum/%get-names enum)))

(define clr-enum/%get-values
  (let ((method (clr/%get-method clr-type-handle/system-enum "GetValues"
                                 (vector clr-type-handle/system-type))))
    (lambda (object)
      ;; (dotnet-message 5 "clr-enum/%get-values")
      (clr/%invoke method #f (vector object)))))

(define (clr-enum/get-values enum)
  (map-clr-array clr/foreign->int (clr-enum/%get-values enum)))

;;; For marshaling integers to enums.
(define clr-enum/to-object
  (let ((method-handle (clr/%get-method clr-type-handle/system-enum "ToObject"
                                        (vector clr-type-handle/system-type
                                                clr-type-handle/system-int32))))
    (lambda (class-handle number)
      (dotnet-message 5 "clr-enum/to-object")
      (clr/%invoke method-handle #f (vector class-handle (clr/%number->foreign-int32 number))))))

(define clr-field-info/%get-value
  (let ((method (clr/%get-method clr-type-handle/system-reflection-fieldinfo "GetValue"
                                 (vector clr-type-handle/system-object))))
    (lambda (object)
      ;; (dotnet-message 5 "clr-field-info/%get-value")
      (clr/%invoke method object (vector clr/null)))))

;;; TRUE argument means fetch private name, false means public only.
(define clr-propertyinfo/%get-get-method
  (let ((method-handle (clr/%get-method clr-type-handle/system-reflection-propertyinfo "GetGetMethod"
                                        (vector clr-type-handle/system-boolean)))
        (private-arglist (vector clr/true))
        (public-arglist  (vector clr/false)))
    (lambda (property-info allow-private?)
      ;; (dotnet-message 5 "clr-propertyinfo/%get-get-method" allow-private?)
      (clr/%invoke method-handle property-info
                   (if allow-private? private-arglist public-arglist)))))

(define clr-type/%get-custom-attributes
  (let ((method (clr/%get-method clr-type-handle/system-type "GetCustomAttributes"
                                 (vector clr-type-handle/system-boolean)))
        (args (vector clr/true)))
    (lambda (object)
      ;; (dotnet-message 5 "clr-type/%get-custom-attributes")
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
      ;; (dotnet-message 5 "clr-type/%get-members" static? public?)
      (clr/%invoke method type (if static?
                                   (if public?
                                       arglist-static-public
                                       arglist-static-non-public)
                                   (if public?
                                       arglist-instance-public
                                       arglist-instance-non-public))))))

(define (clr/type-not-found canonical-name)
  (error "Couldn't find-clr-type " canonical-name))

(define (find-clr-type clr-type-name)
  (dotnet-message 5 "FIND-CLR-TYPE " clr-type-name)
  (let ((canonical-name
         (cond ((string? clr-type-name) clr-type-name)
               ((symbol? clr-type-name) (symbol->string clr-type-name))
               (else (error "Cannot find clr type " clr-type-name)))))

    (or (clr/%get-type canonical-name)
        (let ((assemblies (clr-app-domain/%get-assemblies (clr-app-domain/%current-domain clr/false)))
              (name-as-string (clr/%string->foreign canonical-name)))
          (if (not (%clr-array? assemblies))
              (clr/type-not-found canonical-name)
              (let loop ((idx 0)
                         (limit (clr-array/length assemblies)))
                (if (>= idx limit)
                    (clr/type-not-found canonical-name)
                    (let* ((this-assembly (clr/%foreign-aref assemblies idx))
                           (probe (clr-assembly/%get-type this-assembly name-as-string)))
                      (if (%clr-type? probe)
                          probe
                          (loop (+ idx 1) limit))))))))))

(define (clr/new-guid)
  (clr/%to-string (clr-guid/%new-guid #f)))
