;; -*-Scheme-*-
;; Port of dotnet.ss to larceny

;; NOTE:  This file requires that dotnet-ffi.sch has been loaded.

(define-syntax lookup-slot-info
  (syntax-rules ()
    ((lookup-slot-info class slot-name selector)
     (selector (or (assq slot-name
                         ;; no need to ground slot-ref any more! -- see below
                         ;; (if (eq? class <class>)
                         ;;   ;;* This grounds out the slot-ref tower
                         ;;   getters-n-setters-for-class
                         ;;   (%class-getters-n-setters class))
                         (%class-getters-n-setters class))
                   (error "slot-ref: no slot `~e' in ~e" slot-name class))))))

(define *dotnet-verbose* #t)

(define (dotnet-message text . objects)
  (if *dotnet-verbose*
      (begin
        (display "dotnet:  ")
        (display text)
        (for-each (lambda (object)
                    (display " ")
                    (display object))
                  objects)
        (newline))))

(define (describe-instance instance)
  (dotnet-message "Description of" instance)
  (for-each (lambda (slot)
              (dotnet-message "slot" (car slot) (slot-ref instance (car slot))))
            (class-slots (class-of instance)))
  (dotnet-message "End description"))

(define (void)
  (if #f #f))

(define (void? thing) (eq? thing (void)))
(define (flush-output) #f)

;;; End of miscellany



;;; Bootstrapping types
(define clr-type-handle/system-array                    (ffi:get-type "System.Array"))
(define clr-type-handle/system-com-object               (ffi:get-type "System.__ComObject"))
(define clr-type-handle/system-enum                     (ffi:get-type "System.Enum"))
(define clr-type-handle/system-int32                    (ffi:get-type "System.Int32"))
(define clr-type-handle/system-object                   (ffi:get-type "System.Object"))
(define clr-type-handle/system-reflection-fieldinfo     (ffi:get-type "System.Reflection.FieldInfo"))
(define clr-type-handle/system-reflection-memberinfo    (ffi:get-type "System.Reflection.MemberInfo"))
(define clr-type-handle/system-reflection-methodbase    (ffi:get-type "System.Reflection.MethodBase"))
(define clr-type-handle/system-reflection-methodinfo    (ffi:get-type "System.Reflection.MethodInfo"))
(define clr-type-handle/system-reflection-parameterinfo (ffi:get-type "System.Reflection.ParameterInfo"))
(define clr-type-handle/system-reflection-propertyinfo  (ffi:get-type "System.Reflection.PropertyInfo"))
(define clr-type-handle/system-string                   (ffi:get-type "System.String"))
(define clr-type-handle/system-type                     (ffi:get-type "System.Type"))

(define-syntax define-ffi-predicate
  (syntax-rules ()
    ((define-ffi-predicate name handle)
     (define-syntax name
       (syntax-rules ()
         ((name object) (ffi:%isa? object handle)))))))

(define-ffi-predicate clr-array?      clr-type-handle/system-array)
(define-ffi-predicate clr-com-object? clr-type-handle/system-com-object)
(define-ffi-predicate clr-enum?       clr-type-handle/system-enum)
(define-ffi-predicate clr-int32?      clr-type-handle/system-int32)
(define-ffi-predicate clr-string?     clr-type-handle/system-string)

;;; Bootstrapping methods

;; For performance, this is a macro
(define clr-method-handle/object.tostring  (ffi:get-method clr-type-handle/system-object "ToString" '()))
(define-syntax clr-object/to-string
  (syntax-rules ()
    ((clr-object/to-string object)
     (ffi:%foreign->string
      (ffi:%invoke clr-method-handle/object.tostring object '#())))))

;; as is this
(define clr-method-handle/object.type      (ffi:get-method clr-type-handle/system-object "GetType" '()))
(define-syntax clr-object/type
  (syntax-rules ()
    ((clr-object/type object)
     (ffi:%invoke clr-method-handle/object.type object '#()))))

;;; Zero-argument methods for now.
(define-syntax define-ffi-method
  (syntax-rules ()
    ((define-ffi-method name type-handle method-name)
     (define name
       (let ((method (ffi:get-method type-handle method-name '())))
         (lambda (object)
           (ffi:%invoke method object '#())))))
    ((define-ffi-method name type-handle method-name marshal-in)
     (define name
       (let ((method (ffi:get-method type-handle method-name '())))
         (lambda (object)
           (marshal-in
            (ffi:%invoke method object '#()))))))))

(define-ffi-method clr-object/to-symbol clr-type-handle/system-object "ToString" ffi:foreign->symbol)
(define-ffi-method clr-type/get-members clr-type-handle/system-type "GetMembers")
(define-ffi-method clr-methodbase/get-parameters
  clr-type-handle/system-reflection-methodbase "GetParameters" parse-clr-parameters)
(define-ffi-method clr-propertyinfo/get-get-method
  clr-type-handle/system-reflection-propertyinfo "GetGetMethod")
(define-ffi-method clr-propertyinfo/get-index-parameters
  clr-type-handle/system-reflection-propertyinfo "GetIndexParameters" parse-clr-parameters)

;;; Bootstrapping fields

;;; Bootstrapping properties
(define-syntax define-ffi-property
  (syntax-rules ()
    ((define-ffi-property name type-handle property-name)
     (define name
       (let ((handle (ffi:%get-property type-handle property-name)))
         (lambda (foreign-object)
           (ffi:%property-get handle foreign-object '#())))))
    ((define-ffi-property name type-handle property-name marshal-in)
     (define name
       (let ((handle (ffi:%get-property type-handle property-name)))
         (lambda (foreign-object)
           (marshal-in (ffi:%property-get handle foreign-object '#()))))))))

(define-syntax define-boolean-ffi-property
  (syntax-rules ()
    ((define-ffi-property name type-handle property-name)
     (define name
       (let ((handle (ffi:%get-property type-handle property-name)))
         (lambda (foreign-object)
           (ffi:%property-get-bool handle foreign-object '#())))))))

(define-syntax define-int-ffi-property
  (syntax-rules ()
    ((define-ffi-property name type-handle property-name)
     (define name
       (let ((handle (ffi:%get-property type-handle property-name)))
         (lambda (foreign-object)
           (ffi:%property-get-int handle foreign-object '#())))))))

(define-int-ffi-property clr-array/length   clr-type-handle/system-array "Length")
(define-ffi-property clr-type/base-type      clr-type-handle/system-type "BaseType")
(define-boolean-ffi-property clr-type/is-array?      clr-type-handle/system-type "IsArray")
(define-boolean-ffi-property clr-type/is-by-ref?     clr-type-handle/system-type "IsByRef")
(define-boolean-ffi-property clr-type/is-pointer?    clr-type-handle/system-type "IsPointer")
(define-boolean-ffi-property clr-type/is-enum?       clr-type-handle/system-type "IsEnum")
(define-boolean-ffi-property clr-type/is-interface?  clr-type-handle/system-type "IsInterface")
(define-boolean-ffi-property clr-type/is-value-type? clr-type-handle/system-type "IsValueType")

(define-boolean-ffi-property clr-fieldinfo/is-static?
  clr-type-handle/system-reflection-fieldinfo "IsStatic")
(define-ffi-property clr-fieldinfo/field-type
  clr-type-handle/system-reflection-fieldinfo "FieldType" clr-object->class)

(define-ffi-property clr-memberinfo/declaring-type
  clr-type-handle/system-reflection-memberinfo "DeclaringType" clr-object->class)
(define-ffi-property clr-memberinfo/name
  clr-type-handle/system-reflection-memberinfo "Name" ffi:%foreign->string)
(define-int-ffi-property clr-memberinfo/member-type
  clr-type-handle/system-reflection-memberinfo "MemberType")
(define-ffi-property clr-memberinfo/reflected-type
  clr-type-handle/system-reflection-memberinfo "ReflectedType" clr-object->class)

(define-boolean-ffi-property clr-methodbase/is-static?
  clr-type-handle/system-reflection-methodbase "IsStatic")

(define-ffi-property clr-methodinfo/return-type
  clr-type-handle/system-reflection-methodinfo "ReturnType" clr-object->class)

(define-boolean-ffi-property clr-parameterinfo/is-optional?
  clr-type-handle/system-reflection-parameterinfo "IsOptional")

(define-ffi-property clr-parameterinfo/parameter-type
  clr-type-handle/system-reflection-parameterinfo "ParameterType" clr-object->class)

(define-ffi-property clr-parameterinfo/default-value
  clr-type-handle/system-reflection-parameterinfo "DefaultValue")

(define-boolean-ffi-property clr-propertyinfo/can-read?
  clr-type-handle/system-reflection-propertyinfo "CanRead")
(define-boolean-ffi-property clr-propertyinfo/can-write?
  clr-type-handle/system-reflection-propertyinfo "CanWrite")
(define-ffi-property clr-propertyinfo/property-type
  clr-type-handle/system-reflection-propertyinfo "PropertyType" clr-object->class)




;;;; .NET Class hierarchy

;; The class hierarchy in Scheme will mirror the .NET class hierarchy.
;; This requires just a couple of bits of magic, that once performed,
;; will allow the rest of the .NET class structure to be generated on
;; the fly through the MOP.

;; In .NET, every .NET object is an instance of a System.Object
;; In the Scheme world, the CLASS-OF function is exactly analagous to
;; GetType.

;; So in Scheme, we want all .NET objects to be instances of a
;; System.Object class.  All Scheme System.Object instances have a
;; com-object slot that holds the COM wrapper to the .NET class.

;; Every .NET object has a type, and the GetType method on
;; System.Object returns that type.  Since a type object is *also* a
;; .NET object, it *also* has a type.  There is a distinguished type
;; that is the fixed-point of GetType and all .NET types inherit from
;; it.  Note that this implies that this distinguished type *is an
;; instance of itself* (so much for the single inheritance myth).

  ;; Don't think I need to export these names.
  ;; (provide System.RuntimeType System.Object)

  ;; Given a StudlyName string, return an appropriate key for the
  ;; various hash tables.
(define (StudlyName->key StudlyName)
;    (parameterize ((read-square-bracket-as-paren #f)
;                   (read-case-sensitive #f))
;      (read-from-string StudlyName)))
    (string->symbol (string-downcase StudlyName))
    )

(define class-key/system.array              (StudlyName->key "System.Array"))
(define class-key/system.boolean            (StudlyName->key "System.Boolean"))
(define class-key/system.byte               (StudlyName->key "System.Byte"))
(define class-key/system.__com-object       (StudlyName->key "System.__ComObject"))
(define class-key/system.enum               (StudlyName->key "System.Enum"))
(define class-key/system.int32              (StudlyName->key "System.Int32"))
(define class-key/system.typed-reference    (StudlyName->key "System.TypedReference"))
(define class-key/system.reflection.pointer (StudlyName->key "System.Reflection.Pointer"))
(define class-key/system.single             (StudlyName->key "System.Single"))
(define class-key/system.string             (StudlyName->key "System.String"))
(define class-key/system.uint32             (StudlyName->key "System.UInt32"))
(define class-key/system.value-type         (StudlyName->key "System.ValueType"))

  ;; A temporary scaffolding class for bootstrapping
  ;; the .NET class hierarchy.
;  (defclass <class-with-com-object-scaffold> (<class>)
;    (com-object :initarg :com-object))

(define <class-with-clr-object-scaffold>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-class-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <class>)
               :direct-slots (list (list 'clr-handle :initarg :clr-handle)
                                   (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
               :name '<class-with-clr-object-scaffold>)))
    (let ((<class-with-clr-object-scaffold>
           (rec-allocate-instance
            (*default-class-class*)
            (list :direct-default-initargs #f
                  :direct-supers (list <class>)
                  :direct-slots (list (list 'clr-handle :initarg :clr-handle)
                                      (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
                  :name '<class-with-clr-object-scaffold>))))
      (rec-initialize <class-with-clr-object-scaffold>
                      (list :direct-default-initargs #f
                            :direct-supers (list <class>)
                            :direct-slots (list (list 'clr-handle :initarg :clr-handle)
                                                (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
                            :name '<class-with-clr-object-scaffold>))
      <class-with-clr-object-scaffold>)))


  ;; System.RuntimeType will be the root of the metaclass hierarchy.
  ;; Every .NET type object will inherit from this class, including
  ;; the instance that represents this class!
;  (defclass System.RuntimeType (<class-with-clr-object-scaffold>)
;    (StudlyName :initarg :StudlyName :reader clr/StudlyName)
;    :metaclass <class-with-clr-object-scaffold>)

(define System.RuntimeType
  (begin
    (if (*make-safely*)
        (check-initargs
         <class-with-clr-object-scaffold>
         (list :direct-default-initargs #f
               :direct-supers (list <class-with-clr-object-scaffold>)
               :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
               :name 'System.RuntimeType)))
    (let ((System.RuntimeType
           (rec-allocate-instance
            <class-with-clr-object-scaffold>
            (list :direct-default-initargs #f
                  :direct-supers (list <class-with-clr-object-scaffold>)
                  :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
                  :name 'System.RuntimeType))))
      (rec-initialize
       System.RuntimeType
       (list :direct-default-initargs #f
             :direct-supers (list <class-with-clr-object-scaffold>)
             :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
             :name 'System.RuntimeType))
      System.RuntimeType)))

(define clr/StudlyName (make (*default-generic-class*) :name 'clr/StudlyName))

(add-method
 clr/StudlyName
 (make (*default-method-class*)
   :arity 1
   :specializers (list System.RuntimeType)
   :procedure (lambda (call-next-method x) (slot-ref x 'StudlyName))))

;; A hash table mapping symbols to the Swindle classes that
;; represent .NET classes.  The symbolic key will simply be the
;; name of the class as a case-folded symbol.
(define *clr-type-table* (make-hash-table 'symbol-eq?))

(define (register-dotnet-class! StudlyName class)
  (hash-table-put! *clr-type-table* (StudlyName->key StudlyName) class))

(define (map-dotnet-classes function)
  (hash-table-map *clr-type-table* (lambda (key value) (function value))))

;; This :after method ensures that any classes we create
;; will be installed in the *clr-type-table*
;; We specialize the ALLOCATE-INSTANCE method rather than the
;; INITIALIZE-INSTANCE method so that initialization will be allowed to refer
;; to the uninitialized class object.  (This is so methods created
;; at the time the time the class is created can refer to the type.)
;; Note use of :around method so we can get the return value of allocate-instance.
;; Note use of singleton is required for allocate instance because the
;; instance itself does not yet exist.

;  (defmethod :around (allocate-instance class initargs)
;    ;; Bug in Swindle prevents use of singleton class.
;    (if (eq? class System.RuntimeType)
;        (let ((instance (call-next-method))
;              (StudlyName (getarg initargs :StudlyName)))
;          ;; (dotnet-message "Class" StudlyName)
;          (register-dotnet-class! StudlyName instance)
;          instance)
;        (call-next-method)))

(add-method allocate-instance
  (make <method>
    :specializers (list <class> <top>)
    :arity 2
    :qualifier :around
    :procedure ((lambda ()
                  (define (allocate-instance call-next-method class initargs)
                    (if (eq? class System.RuntimeType)
                        (let ((instance (call-next-method))
                              (StudlyName (getarg initargs :StudlyName)))
                          (dotnet-message "Registering Class" StudlyName)
                          (register-dotnet-class! StudlyName instance)
                          instance)
                        (call-next-method)))
                  allocate-instance))))

(define <clr-reflected-method>
  (let ((<clr-reflected-method>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list
           :direct-default-initargs #f
           :direct-supers (list <method>)
           :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle)
                               (list 'max-arity :initarg :max-arity :reader 'max-arity))
           :name '<clr-reflected-method>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs #f
          :direct-supers (list <method>)
          :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle)
                              (list 'max-arity :initarg :max-arity :reader 'max-arity))
          :name '<clr-reflected-method>)))
    (rec-initialize
     <clr-reflected-method>
     (list
      :direct-default-initargs #f
      :direct-supers (list <method>)
      :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle)
                          (list 'max-arity :initarg :max-arity :reader 'max-arity))
      :name '<clr-reflected-method>))
    <clr-reflected-method>))

(define max-arity (make (*default-generic-class*) :name 'max-arity))

(add-method
  max-arity
  (make (*default-method-class*)
    :arity 1
    :specializers (list <clr-reflected-method>)
    :procedure (lambda (call-next-method x) (slot-ref x 'max-arity))))


(define clr-object/clr-handle (make (*default-generic-class*) :name 'clr-object/clr-handle))

(add-method
  clr-object/clr-handle
  (make (*default-method-class*)
    :arity 1
    :specializers (list <clr-reflected-method>)
    :procedure (lambda (call-next-method instance)
                 (slot-ref instance 'clr-handle))))

;; The CLR-TYPE-DESCRIPTOR is a .NET RuntimeType
;; We extract the name of the type and look it up in the *clr-type-table*
;; to get the Scheme class that represents the type.
;;
;; If the type isn't in the table yet, we create it on the fly by
;; recursively locating the parent type and creating a instance of a
;; <clr-class> to represent the new type (which will register it in the
;; table).  This bottoms out when we get to System.RuntimeType which
;; is manually created.
(define (clr-object->class clr-type-descriptor)
  (let* ((StudlyName    (clr-object/to-string clr-type-descriptor))
         (clr-type-name (StudlyName->key StudlyName)))
    ;; (dotnet-message "CLR-OBJECT->CLASS: " StudlyName)
    (hash-table-get
     *clr-type-table* clr-type-name
     (lambda ()
       ;;(dotnet-message "Instantiating class object for" StudlyName)
       ;; Not found?  Create one.
       (make System.RuntimeType
         :name clr-type-name
         :StudlyName StudlyName
         :clr-handle clr-type-descriptor
         :direct-supers
          ;; As it turns out, the "BaseType" property is *not* a reliable
          ;; means to figure out the base type.  This COND special cases the
          ;; known problems.
          (let* ((bt-property (clr-type/base-type clr-type-descriptor))
                 (base-class (if (or (not bt-property)
                                     (null? bt-property)
                                     (void? bt-property)
                                     (foreign-null? bt-property))
                                 (cond ((clr-type/is-array? clr-type-descriptor)
                                        ;;(error "Array Type?" clr-type-name)
                                        (dotnet-message "Using System.Array as base type for" StudlyName)
                                        (clr/find-class "System.Array"))

                                       ((clr-type/is-by-ref? clr-type-descriptor)
                                        (dotnet-message "Using System.TypedReference as base type for" StudlyName)
                                        (clr/find-class "System.TypedReference"))

                                       ((clr-type/is-pointer? clr-type-descriptor)
                                        (dotnet-message "Using System.Reflection.Pointer as base type for" StudlyName)
                                        (clr/find-class "System.Reflection.Pointer"))

                                       ;; Interfaces inherit from System.Object implicitly
                                       ;; should also check for `base interfaces'
                                       ((clr-type/is-enum? clr-type-descriptor)
                                        (dotnet-message "Using System.Enum as base type for" StudlyName)
                                        (clr/find-class "System.Enum"))

                                       ((clr-type/is-interface? clr-type-descriptor)
                                        (dotnet-message "Using System.Object as base type for" StudlyName)
                                        System.Object)

                                       ((clr-type/is-value-type? clr-type-descriptor)
                                        (dotnet-message "Using System.ValueType as base type for" StudlyName)
                                        (clr/find-class "System.ValueType"))

                                       ;; Special case CLR bug?
                                       ((eq? clr-type-name 'system.enum)
                                        (dotnet-message "Using System.ValueType as base type for" StudlyName)
                                        (clr/find-class "System.ValueType"))
                                       (else (error "Dunno what the base type is." clr-type-name)))
                                 (clr-object->class bt-property))))
            ;; If we have a method (or constructor), mix in the
            ;; reflected method class so we can invoke it.
            (if (string=? StudlyName "System.Reflection.MethodBase")
                (list <clr-reflected-method> base-class)
                (list base-class))))))))

  ;; System.Object will be the root of the class hierarchy.
  ;; Every .NET class will inherit from this class.
  ;; Every COM object that represents a .NET object will inherit from this one.
  ;; (Note: don't try to create a superclass for this, it won't work.
  ;;   -- the voice of experience)

(define System.Object
  (begin
    (if (*make-safely*)
        (check-initargs
         System.RuntimeType
         (list :StudlyName "System.Object"
               :direct-default-initargs #f
               :direct-supers (list <object>)
               :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle))
               :name 'system.object)))
    (let ((System.Object
           (rec-allocate-instance
            System.RuntimeType
            (list :StudlyName "System.Object"
                  :direct-default-initargs #f
                  :direct-supers (list <object>)
                  :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle))
                  :name 'system.object))))
      (rec-initialize
       System.Object
       (list :StudlyName "System.Object"
             :direct-default-initargs #f
             :direct-supers (list <object>)
             :direct-slots (list (list 'clr-handle :initarg :clr-handle :reader 'clr-object/clr-handle))
             :name 'system.object))
      System.Object)))

(add-method
  clr-object/clr-handle
  (make (*default-method-class*)
    :arity 1
    :specializers (list System.Object)
    :procedure (lambda (call-next-method instance)
                 (slot-ref instance 'clr-handle))))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list System.Object)
    :arity 3
    :procedure ((lambda ()
                  (define (print-object call-next-method object port slashify)
                    (let* ((clr-object  (clr-object/clr-handle object))
                           (type-name   (clr-object/to-string (clr-object/clr-handle (class-of object))))
                           (printed-rep (clr-object/to-string clr-object)))

                      (display "#<" port)
                      (display type-name port)
                      (if (not (string=? type-name printed-rep))
                          (begin
                            (display " " port)
                            (display printed-rep port)))
                      (if (assq 'arity (class-slots (class-of object)))
                          (begin
                            (display " " port)
                            (display (slot-ref object 'arity) port)))
                      (display ">" port)))
                  print-object))))

(define (bootstrap-clr-classes! bootstrap-clr-object)
  ;; The classes defined above are isomorphic to what we want, so we
  ;; simply need to kick out the supporting structure.

  ;; Get the type descriptor of the type class
  ;; by finding the fixed point of GetType.
  (let loop ((this bootstrap-clr-object)
             (previous-name #f))
    (let* ((this-type (clr-object/type this))
           (this-name (clr-object/to-symbol this-type)))
      ;; (dotnet-message "This name:  " this-name "Previous name:  " previous-name)
      (if (eq? this-name previous-name)
          ;; Got it.
          (begin
            ;; Set up the System.RuntimeType to be an instance
            ;; of itself (using some magic).  This *must* be done first
            ;; so that any clases created on demand while we initialize
            ;; will have the correct inheritance chain.
            ;; (dotnet-message "set-instance-class-to-self!")
            (set-instance-class-to-self! System.RuntimeType)

            ;; Set the clr-object slot and put this class in the
            ;; type table.
            ;; (dotnet-message "slot-set system.runtimetype 'clr-handle")
            (slot-set! System.RuntimeType 'clr-handle this-type)
            ;; (dotnet-message "slot-set system.runtimetype 'studlyname")
            (slot-set! System.RuntimeType 'StudlyName (symbol->string this-name))
            ;; (dotnet-message "register dotnet class")
            (register-dotnet-class! (symbol->string this-name) System.RuntimeType)
            ;; Reset the direct supers of the runtime class to be the correct object
            ;; and recompute the class precedence list and slots.  Once this is done,
            ;; we are bootstrapped.

            ;; Note that call to CLR-OBJECT->CLASS will cause other classes to be loaded.
            ;; This is ok because enough of System.RuntimeType is initialized to
            ;; make subsequent type creation work.
                                        ;(dotnet-message (clr-type/base-type this-type))
                                        ;(dotnet-message (clr-object->class (clr-type/base-type this-type)))

            (slot-set! System.RuntimeType 'direct-supers
                       (list (clr-object->class (clr-type/base-type this-type))
                             <class>))
            (slot-set! System.RuntimeType 'cpl (compute-cpl System.RuntimeType))
            (slot-set! System.RuntimeType 'slots (compute-slots System.RuntimeType))
            (add-method clr-object/clr-handle
              (make (*default-method-class*)
                :arity 1
                :specializers (list System.RuntimeType)
                :procedure (let ((getter (lookup-slot-info System.RuntimeType 'clr-handle cadr)))
                             (lambda (call-next-method instance)
                               (getter instance))))))

          (loop this-type this-name))))

  ;; Now we need to find the com object associated with System.Object.
  ;; This time we walk the type hierarchy in `BaseType' direction.
  (let loop ((this (slot-ref System.RuntimeType 'clr-handle)))
    (let ((this-name (clr-object/to-symbol this)))
      (if (eq? this-name '|System.Object|)
          ;; Should only be true when bootstrapping.
          ;; The type will be in the hash table after that.
          ;; The system object type is not initially
          (begin
            (slot-set! System.Object 'clr-handle this)
            (register-dotnet-class! (symbol->string this-name) System.Object))
          (loop (clr-type/base-type this))))))

;;; End of bootstrap code for .NET class hierarchy.

;; Given a CLR class, iterate over all `members'
;; This is used by the method discovery code below.
(define (clr-class/for-each-type-member function clr-class)
  ;; (dotnet-message "clr-class/for-each-type-member")
  (let* ((foreign-object (clr-object/clr-handle clr-class))
         (members        (clr-type/get-members foreign-object))
         (limit          (clr-array/length members)))
    (let loop ((idx 0))
      (if (< idx limit)
          (begin
            ;; (dotnet-message "clr-class/for-each-type-member" idx)
            (function (clr-object->clr-instance (ffi:%foreign-aref members idx)))
            (loop (+ idx 1)))))))

(define (clr/class-members clr-class)
  (let ((collection '()))
    (clr-class/for-each-type-member
     (lambda (member)
       (set! collection (cons member collection)))
     clr-class)
    collection))

(define (list-clr-classes)
  (map-dotnet-classes identity))

(define (find-clr-type clr-type-name)
  (or (ffi:get-type  (cond ((string? clr-type-name) clr-type-name)
                           ((symbol? clr-type-name) (symbol->string clr-type-name))
                           (else (error "Cannot find clr type " clr-type-name))))
      (error "Couldn't FIND-CLR-TYPE " clr-type-name)))

;; Given a symbol, find the CLR class associated with it,
;; fetching and instantiating it on the fly from the CLR
;; type object if necessary.
(define (clr/find-class StudlyName)
  (hash-table-get
   *clr-type-table* (cond ((string? StudlyName) (StudlyName->key StudlyName))
                          ((symbol? StudlyName) StudlyName)
                          (else (error "Cannot find class " StudlyName)))
   (lambda ()
     ;; (dotnet-message "clr/find-class failure")
     ;; Go fetch it from .NET
     ;; Problem here is that the underlying code is being case sensitive.
     (clr-object->class (find-clr-type StudlyName)))))

 ;; Create an instance of a CLR class to represent the .NET object
(define (wrap-clr-object class clr-object)
  ;; NOTE CAREFULLY
  ;;  When a CLR class is returned from .NET,
  ;;  we *don't* want to call the MAKE function on the metaclass
  ;;  (i.e., System.RuntimeType) because that will cause a non-equivalent
  ;;  instantiation of the CLR class.
  (cond ((eq? class System.RuntimeType)           (clr-object->class clr-object))
        ((subclass? class <clr-reflected-method>) (clr-object->method clr-object class))
        (else                                     (make class :clr-handle clr-object))))

(define (clr-object/potential-types clr-object)
  (let ((raw-object (clr-object/clr-handle clr-object))
        (potential-types '()))
    (map-dotnet-classes (lambda (class)
                          (if (ffi:%isa? raw-object (clr-object/clr-handle class))
                              (set! potential-types (cons class potential-types)))))
    potential-types))

(define (clr-dynamic-cast new-class clr-object)
  (let ((raw-object (clr-object/clr-handle clr-object))
        (raw-type   (clr-object/clr-handle new-class)))
    (if (ffi:%isa? raw-object raw-type)
        (wrap-clr-object new-class raw-object)
        (error "Cannot cast object to new type" clr-object new-class))))

(define (clr-object->clr-instance clr-object)
  (wrap-clr-object
   (hash-table-get *clr-type-table* (StudlyName->key (ffi:%type-as-string clr-object))
                   (lambda () (clr-object->class (clr-object/type clr-object))))
   clr-object))

(define (argument-specializer clr-class)
  (cond ((clr-type/is-array? (clr-object/clr-handle clr-class)) <vector>)
        ((clr-type/is-enum?  (clr-object/clr-handle clr-class)) <top>)
        ;; Widen system.object to include everything
        ;; so we can pass ints, strings, etc.
        ((eq? clr-class System.Object) <top>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "SchemeBridge.MzSchemeObject"))) <top>)
        ((eq? clr-class (clr/find-class "System.Char")) <char>)
        ((eq? clr-class (clr/find-class "System.String")) <symbol>)
        ((eq? clr-class (clr/find-class "System.Int32")) <integer>)
        ((eq? clr-class (clr/find-class "System.UInt32")) <integer>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "System.Array"))) <vector>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "SchemeBridge.BridgeArray"))) <vector>)
        ((eq? clr-class (clr/find-class "System.Boolean")) <boolean>)
        ((eq? clr-class (clr/find-class "System.Byte")) <integer>)
        ((eq? clr-class (clr/find-class "System.Single")) <integer>)
        ;;((eq? clr-class (clr/find-class (string->symbol "System.Drawing.Color")))
        ;; (clr/find-class (string->symbol "SchemeBridge.ColorClass")))
        (else clr-class)))

(define (argument-marshaler class)
  (cond ((eq? class (clr/find-class "System.Boolean")) ffi:bool->foreign)
        ((eq? class (clr/find-class "System.String")) ffi:symbol->foreign)
        (else clr-object/clr-handle)))

(define (clr/marshal-in object)
  (cond ((eq? object (unspecified)) object)
        ((clr-array? object) (let loop ((index 0)
                                        (limit (clr-array/length object))
                                        (result '()))
                               (if (>= index limit)
                                   (list->vector (reverse! result))
                                   (loop (+ index 1) limit (cons (clr/marshal-in (ffi:%foreign-aref object index)) result)))))
        ((clr-enum? object) (ffi:%foreign->int object))
        ((clr-int32? object) (ffi:%foreign->int object))
        ((clr-string? object) (ffi:foreign->symbol object))
        ((foreign-null? object) '())
        ((ffi:%eq? object foreign-true) #t)
        ((ffi:%eq? object foreign-false) #f)
        (else (clr-object->clr-instance object))))

(define (clr-array->vector object)
  (let loop ((index 0)
             (limit (clr-array/length object))
             (result '()))
    (if (>= index limit)
        (list->vector (reverse! result))
        (loop (+ index 1) limit (cons (clr/marshal-in (ffi:%foreign-aref object index)) result)))))

(define (return-marshaler class)
  (cond ((eq? class System.RuntimeType) clr-object->class)
        ((eq? class (clr/find-class "System.Boolean")) ffi:foreign->bool)
        ((eq? class (clr/find-class "System.Double"))  ffi:foreign->double)
        ((eq? class (clr/find-class "System.Int16"))   ffi:foreign->int)
        ((eq? class (clr/find-class "System.UInt16"))  ffi:foreign->int)
        ((eq? class (clr/find-class "System.Int32"))   ffi:foreign->int)
        ((eq? class (clr/find-class "System.UInt32"))  ffi:foreign->int)
        ((eq? class (clr/find-class "System.Int64"))   ffi:foreign->int)
        ((eq? class (clr/find-class "System.UInt64"))  ffi:foreign->int)
        ((eq? class (clr/find-class "System.String"))  ffi:foreign->symbol)
        ((eq? class (clr/find-class "System.Void"))    unspecified)
        ((clr-type/is-array? (clr-object/clr-handle class)) clr-array->vector)
        (else clr-object->clr-instance)))

(define (parse-clr-parameters raw-parameters)
  (let loop ((i 0)
             (limit (clr-array/length raw-parameters))
             (required-parameter-count 0)
             (optional-parameter-count 0)
             (default-values '())
             (specializers '())
             (parameter-marshalers '()))
    (if (>= i limit)
        (values required-parameter-count
                optional-parameter-count
                default-values
                (reverse! specializers)
                (reverse! parameter-marshalers))
        (let* ((raw-parameter  (ffi:%foreign-aref raw-parameters i))
               (parameter-type (clr-parameterinfo/parameter-type raw-parameter)))
          (if (clr-parameterinfo/is-optional? raw-parameter)
              (loop (+ i 1)
                    limit
                    required-parameter-count
                    (+ optional-parameter-count 1)
                    (cons (wrap-clr-object parameter-type (clr-parameterinfo/default-value raw-parameter))
                          default-values)
                    specializers
                    (cons (argument-marshaler parameter-type) parameter-marshalers))
              (loop (+ i 1)
                    limit
                    (+ required-parameter-count 1)
                    optional-parameter-count
                    default-values
                    (cons (argument-specializer parameter-type) specializers)
                    (cons (argument-marshaler parameter-type) parameter-marshalers)))))))

(define (marshal-out max-arity marshalers arguments default-values)
  (let ((result (make-vector max-arity)))
    (define (loop1 index marshalers arguments)
      (cond ((pair? arguments)
             (vector-set! result index ((car marshalers) (car arguments)))
             (loop1 (+ index 1) (cdr marshalers) (cdr arguments)))
            ((null? arguments) (loop2 max-arity index (reverse marshalers) default-values))
            (else (error "bad list of arguments"))))

    (define (loop2 index limit marshalers defaults)
      (let ((new-index (- index 1)))
        (if (< new-index limit)
            result
            (begin (vector-set! result new-index ((car marshalers) (car defaults)))
                   (loop2 new-index limit (cdr marshalers) (cdr defaults))))))

    (loop1 0 marshalers arguments)))

(define (clr-constructor-info->method info class)
  (call-with-values
   (lambda () (clr-methodbase/get-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let ((arity (if (= optional-parameter-count 0)
                      required-parameter-count
                      (make-arity-at-least required-parameter-count)))
           (max-arity (+ optional-parameter-count required-parameter-count 1))
           (in-marshaler (return-marshaler (clr-memberinfo/declaring-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers specializers
         :procedure (nary->fixed-arity
                     (lambda (call-next-method . args)
                       (in-marshaler
                        (ffi:%invoke-constructor info
                                                 (marshal-out (+ optional-parameter-count
                                                                 required-parameter-count)
                                                              out-marshalers args default-values))))
                     (arity-plus arity 1)))))))

(define (clr-method-info->static-method info class)
  (call-with-values
   (lambda () (clr-methodbase/get-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let ((arity (if (= optional-parameter-count 0)
                      required-parameter-count
                      (make-arity-at-least required-parameter-count)))
           (max-arity (+ optional-parameter-count required-parameter-count 1))
           (in-marshaler (return-marshaler (clr-methodinfo/return-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers specializers
         :procedure (nary->fixed-arity
                     (lambda (call-next-method . args)
                       (in-marshaler
                        (ffi:%invoke info
                                     #f
                                     (marshal-out (+ optional-parameter-count required-parameter-count)
                                                  out-marshalers args default-values))))
                     (arity-plus arity 1)))))))

(define (clr-method-info->method info class)
  (call-with-values
   (lambda () (clr-methodbase/get-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let* ((declaring-type (clr-memberinfo/declaring-type info))
            (instance-marshaler (argument-marshaler declaring-type))
            (arity (if (= optional-parameter-count 0)
                       (+ required-parameter-count 1)
                       (make-arity-at-least (+ required-parameter-count 1))))
            (max-arity (+ optional-parameter-count required-parameter-count 2))
            (in-marshaler (return-marshaler (clr-methodinfo/return-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers (cons (argument-specializer declaring-type) specializers)
         :procedure (nary->fixed-arity
                     (lambda (call-next-method instance . args)
                       (in-marshaler
                        (ffi:%invoke info
                                     (instance-marshaler instance)
                                     (marshal-out (+ optional-parameter-count required-parameter-count)
                                                  out-marshalers args default-values))))
                     (arity-plus arity 1)))))))

(define (clr-property-info->getter-method info class)
  (call-with-values
   (lambda () (clr-propertyinfo/get-index-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let* ((declaring-type (clr-memberinfo/declaring-type info))
            (instance-marshaler (argument-marshaler declaring-type))
            (arity (if (= optional-parameter-count 0)
                       (+ required-parameter-count 1)
                       (make-arity-at-least (+ required-parameter-count 1))))
            (max-arity (+ optional-parameter-count required-parameter-count 2))
            (in-marshaler (return-marshaler (clr-propertyinfo/property-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers (cons (argument-specializer declaring-type) specializers)
         :procedure (nary->fixed-arity
                     (lambda (call-next-method instance . args)
                       ;; (dotnet-message "Getting property" (clr-memberinfo/name info))
                       (in-marshaler
                        (ffi:%property-get info
                                           (instance-marshaler instance)
                                           (marshal-out (+ optional-parameter-count required-parameter-count)
                                                        out-marshalers args default-values))))
                     (arity-plus arity 1)))))))

(define (clr-property-info->static-getter-method info class)
  (call-with-values
   (lambda () (clr-propertyinfo/get-index-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let* ((declaring-type (clr-memberinfo/declaring-type info))
            (arity (if (= optional-parameter-count 0)
                       required-parameter-count
                       (make-arity-at-least required-parameter-count)))
            (max-arity (+ optional-parameter-count required-parameter-count 1))
            (in-marshaler (return-marshaler (clr-propertyinfo/property-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers specializers
         :procedure (nary->fixed-arity
                     (lambda (call-next-method . args)
                       ;;(dotnet-message "Getting static property" (clr-memberinfo/name info))
                       (in-marshaler
                        (ffi:%property-get info
                                           foreign-null
                                           (marshal-out (+ optional-parameter-count required-parameter-count)
                                                        out-marshalers args default-values))))
                     (arity-plus arity 1)))))))

(define (clr-property-info->setter-method info class)
  (call-with-values
   (lambda () (clr-propertyinfo/get-index-parameters info))
   (lambda (required-parameter-count
            optional-parameter-count
            default-values
            specializers
            out-marshalers)
     (let* ((declaring-type (clr-memberinfo/declaring-type info))
            (instance-marshaler (argument-marshaler declaring-type))
            (arity (if (= optional-parameter-count 0)
                       (+ required-parameter-count 2)
                       (make-arity-at-least (+ required-parameter-count 2))))
            (max-arity (+ optional-parameter-count required-parameter-count 3))
            (new-value-marshaler (argument-marshaler (clr-propertyinfo/property-type info))))
       (make class
         :arity arity
         :max-arity max-arity
         :clr-handle info
         :name (clr-memberinfo/name info)
         :specializers (list* (argument-specializer declaring-type)
                              (argument-specializer (clr-propertyinfo/property-type info))
                              specializers)
         :procedure (nary->fixed-arity
                     (lambda (call-next-method instance new-value . args)
                       (dotnet-message "Setting property" (clr-memberinfo/name info))
                       (ffi:%property-set info
                                          (instance-marshaler instance)
                                          (new-value-marshaler new-value)
                                          (marshal-out (+ optional-parameter-count required-parameter-count)
                                                       out-marshalers args default-values))
                       (unspecified))
                     (arity-plus arity 1)))))))

(define (clr-object->method object class)
  (let ((member-type/ctor   1)
        (member-type/method 8)
        (member-type (clr-memberinfo/member-type object)))
    (cond ((= member-type member-type/ctor)
           (clr-constructor-info->method object class))
          ((= member-type member-type/method)
           (if (clr-methodbase/is-static? object)
               (clr-method-info->static-method object class)
               (clr-method-info->method object class)))
          (else (error "Can't make a method from this" object)))))


(define <clr-enumerate>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-class-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <object>)
               :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName)
                                   (list 'enumeration :initarg :enumeration :reader 'enumerate/enumeration))
               :name '<clr-enumerate>)))
    (let ((<clr-enumerate>
           (rec-allocate-instance
            (*default-class-class*)
            (list :direct-default-initargs #f
                  :direct-supers (list <object>)
                  :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName)
                                      (list 'enumeration :initarg :enumeration :reader 'enumerate/enumeration))
                  :name '<clr-enumerate>))))
      (rec-initialize <clr-enumerate>
                      (list :direct-default-initargs #f
                            :direct-supers (list <object>)
                            :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName)
                                                (list 'enumeration :initarg :enumeration :reader 'enumerate/enumeration))
                            :name '<clr-enumerate>))
      <clr-enumerate>)))

(add-method
 clr/StudlyName
 (make (*default-method-class*)
   :arity 1
   :specializers (list <clr-enumerate>)
   :procedure (lambda (call-next-method x) (slot-ref x 'StudlyName))))

(define enumerate/enumeration (make (*default-generic-class*) :name 'enumerate/enumeration))

(add-method
 enumerate/enumeration
 (make (*default-method-class*)
   :arity 1
   :specializers (list <clr-enumerate>)
   :procedure (lambda (call-next-method x) (slot-ref x 'enumeration))))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-enumerate>)
    :arity 3
    :procedure ((lambda ()
                  (define (print-object call-next-method object port slashify)
                      (display "#<" port)
                      (display (clr/StudlyName (enumerate/enumeration object)) port)
                      (display " " port)
                      (display (clr/StudlyName object) port)
                      (display ">" port))
                  print-object))))

(define (clr/enumerate-union left right)
  (if (eq? (enumerate/enumeration left)
           (enumerate/enumeration right))
      ;; should test for flags attribute, but how?
      (make <clr-enumerate>
        :StudlyName (string-append (clr/StudlyName left) ", " (clr/StudlyName right))
        :enumeration (enumerate/enumeration left))
      (error "Mismatched enumeration." left right)))

;;;; Generic functions on CLR objects.
;;;
;;; Methods on CLR objects will map into methods on generic functions.
;;; These will be created dynamically.  Since dynamic importing of top-level
;;; names could be a problem, we'll translate top-level identifiers that
;;; have leading dots to something that looks up the appropriately named
;;; generic function in the *CLR-GENERICS* hash table.

;;; What we want to do is add an AFTER method to INITIALIZE-INSTANCE on
;;; System.RuntimeType so that whenever we create a new type we probe
;;; for the properties, fields, etc. and augment or construct the
;;; appropriate methods.  However, this would cause an infinite loop
;;; because the member descriptors refer to type objects that have not
;;; yet been loaded.  To solve this, we do the following:

;;;  1.  Iterate over all existing types probing for their members.
;;;      This will instantiate new types who's methods are unknown.
;;;
;;;  2.  Iterate over all new types just instantiated to get *their*
;;;      members.  This may instantiate even more types.
;;;
;;;  3.  Repeat step 2 until no new types are created.  At this point,
;;;      it should be possible to instantiate new methods without the
;;;      possibility of inifite loops.
;;;
;;;  4.  Install the AFTER method so that methods are instantiated right
;;;      after instantiating the type.

(define <clr-enum-constructor>
  (let ((<clr-enum-constructor>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots '()
                :name '<clr-enum-constructor>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots '()
               :name '<clr-enum-constructor>)))
                                        ;(display "; Initialize <clr-enum-constructor>")(newline)
    (rec-initialize
     <clr-enum-constructor>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots '()
           :name '<clr-enum-constructor>))
    <clr-enum-constructor>))

(define <clr-instance-field-getter>
  (let ((<clr-instance-field-getter>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots '()
                :name '<clr-instance-field-getter>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots '()
               :name '<clr-instance-field-getter>)))
    (rec-initialize
     <clr-instance-field-getter>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots '()
           :name '<clr-instance-field-getter>))
    <clr-instance-field-getter>))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-instance-field-getter>)
    :arity 3
    :procedure ((lambda ()
                  (define (print-object call-next-method object port slashify)
                    (let* ((clr-object  (clr-object/clr-handle object))
                           (decl-type   (clr-object/to-string
                                         (clr-object/clr-handle (clr-memberinfo/declaring-type clr-object))))
                           (printed-rep (clr-memberinfo/name clr-object)))

                      (display "#<CLR-FIELD-GETTER " port)
                      (display decl-type port)
                      (display "." port)
                      (display printed-rep port)
                      (display ">" port)))
                  print-object))))

(define <clr-static-field-getter>
  (let ((<clr-static-field-getter>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots '()
                :name '<clr-static-field-getter>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots '()
               :name '<clr-static-field-getter>)))
    (rec-initialize
     <clr-static-field-getter>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots '()
           :name '<clr-static-field-getter>))
    <clr-static-field-getter>))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-static-field-getter>)
    :arity 3
    :procedure ((lambda ()
                  (define (print-object call-next-method object port slashify)
                    (let* ((clr-object  (clr-object/clr-handle object))
                           (decl-type   (clr-object/to-string
                                         (clr-object/clr-handle (clr-memberinfo/declaring-type clr-object))))
                           (printed-rep (clr-memberinfo/name clr-object)))

                      (display "#<CLR-FIELD-GETTER " port)
                      (display decl-type port)
                      (display "." port)
                      (display printed-rep port)
                      (display ">" port)))
                  print-object))))

(define <clr-instance-field-setter>
  (let ((<clr-instance-field-setter>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots '()
                :name '<clr-instance-field-setter>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots '()
               :name '<clr-instance-field-setter>)))
    (rec-initialize
     <clr-instance-field-setter>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots '()
           :name '<clr-instance-field-setter>))
    <clr-instance-field-setter>))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-instance-field-setter>)
    :arity 3
    :procedure ((lambda ()
                  (define (print-object call-next-method object port slashify)
                    (let* ((clr-object  (clr-object/clr-handle object))
                           (decl-type   (clr-object/to-string
                                         (clr-object/clr-handle (clr-memberinfo/declaring-type clr-object))))
                           (printed-rep (clr-memberinfo/name clr-object)))

                      (display "#<CLR-FIELD-SETTER " port)
                      (display decl-type port)
                      (display "." port)
                      (display printed-rep port)
                      (display ">" port)))
                  print-object))))

(define *clr-generics* (make-hash-table 'symbol-eq?))

(define (clr/find-generic name)
  (hash-table-get
   *clr-generics* name
   (lambda ()
     (error "CLR-GENERIC (or overload) not found:" name))))

(define *clr-static-methods* (make-hash-table 'symbol-eq?))

(define (clr/find-static-method name)
  (hash-table-get
   *clr-static-methods* name
   (lambda ()
     (error "CLR-STATIC-METHOD not found:" name))))

(define *clr-constructors* (make-hash-table 'symbol-eq?))

(define (clr/find-constructor name)
  (hash-table-get
   *clr-constructors* name
   (lambda ()
     (error "CLR-CONSTRUCTOR not found:" name))))


(define *clr-static-fields* (make-hash-table 'symbol-eq?))

(define (clr/find-static-field name)
  (hash-table-get
   *clr-static-fields* name
   (lambda ()
     (error "CLR-STATIC-FIELD not found: " name))))

(define *clr-instance-field-getters* (make-hash-table 'symbol-eq?))

(define (clr/find-instance-field-getter name)
  (hash-table-get
   *clr-instance-field-getters* name
   (lambda ()
     (error "CLR-INSTANCE-FIELD-GETTER not found: " name))))

(define *clr-instance-field-setters* (make-hash-table 'symbol-eq?))

(define (clr/find-instance-field-setter name)
  (hash-table-get
   *clr-instance-field-setters* name
   (lambda ()
     (error "CLR-INSTANCE-FIELD-SETTER not found" name))))

(define <clr-generic>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs #f
          :direct-supers (list <generic>)
          :direct-slots (list
                              (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
          :name '<clr-generic>)))
    (let ((<clr-generic> (rec-allocate-instance
                          (*default-entityclass-class*)
                          (list
                           :direct-default-initargs #f
                           :direct-supers (list <generic>)
                           :direct-slots (list
                                               (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
                           :name '<clr-generic>))))
      (rec-initialize
       <clr-generic>
       (list
        :direct-default-initargs #f
        :direct-supers (list <generic>)
        :direct-slots (list
                            (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
        :name '<clr-generic>))
      <clr-generic>)))

(add-method clr/StudlyName
  (make (*default-method-class*)
    :specializers (list <clr-generic>)
    :arity 1
    :procedure (lambda (call-next-method x) (slot-ref x 'StudlyName))))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-generic>)
    :arity 3
    :procedure ((lambda ()
                 (define (print-object call-next-method object port slashify)
                     (display "#<CLR-GENERIC " port)
                     (display (generic-arity object) port)
                     (display " " port)
                     (display (clr/StudlyName object) port)
                     (display ">" port))
                 print-object))))

(define <clr-arity-overload>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs '() ;; (list :arity (make-arity-at-least 1))
          :direct-supers (list <clr-generic>)
          :direct-slots (list (list 'arity-vector :initarg :arity-vector))
          :name '<clr-arity-overload>)))

    (let ((<clr-arity-overload>
           (rec-allocate-instance
            (*default-entityclass-class*)
            (list
             :direct-default-initargs '() ;; (list :arity (make-arity-at-least 1))
             :direct-supers (list <clr-generic>)
             :direct-slots (list (list 'arity-vector :initarg :arity-vector))
             :name '<clr-arity-overload>))))
      (rec-initialize
       <clr-arity-overload>
       (list
        :direct-default-initargs '() ;; (list :arity (make-arity-at-least 1))
        :direct-supers (list <clr-generic>)
        :direct-slots (list (list 'arity-vector :initarg :arity-vector))
        :name '<clr-arity-overload>))
      <clr-arity-overload>)))

(define get-arity-vector
  (lookup-slot-info <clr-arity-overload> 'arity-vector cadr))

(add-method initialize-instance
  (make (*default-method-class*)
    :arity 2
    :specializers (list <clr-arity-overload> <top>)
    :procedure (lambda (call-next-method generic initargs)
                 ;;(dotnet-message "Initialize clr-arity-overload" initargs)
                 (add-method
                   generic
                   (make (*default-method-class*)
                     :arity (make-arity-at-least 0)
                     :specializers (list <top>)
                     :procedure (lambda (call-next-method . arguments)
                                  (let ((argcount (length arguments)))
                                    (let ((vector (get-arity-vector generic)))
                                      (if (not (< argcount (vector-length vector)))
                                          (error "Too many arguments to overloaded method."
                                                 generic
                                                 argcount)
                                          (let ((target (vector-ref vector argcount)))
                                            (if (instance-of? target <clr-generic>)
                                                (apply target arguments)
                                                (error "No overloaded method for "
                                                       arguments))))))))))
    :qualifier :after))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list <clr-arity-overload>)
    :arity 3
    :procedure ((lambda ()
                 (define (print-object call-next-method object port slashify)
                     (display "#<CLR-OVERLOAD " port)
                     (display (clr/StudlyName object) port)
                     (display ">" port))
                 print-object))))

(define (ensure-overload-vector-capacity overload desired-maximum-arity)
  (let* ((vector (get-arity-vector overload))
         (capacity (vector-length vector)))
    (if (<= capacity desired-maximum-arity)
        (begin
          ;; copy the elements
          (let loop ((new-vector (make-vector (+ desired-maximum-arity 1) #f))
                     (index 0))
            (if (= index capacity)
                (slot-set! overload 'arity-vector new-vector)
                (begin (vector-set! new-vector index
                                    (vector-ref vector index))
                       (loop new-vector (+ index 1)))))))))

(define (map-parameter-class clr-class)
  (cond ((clr-type/is-array? (clr-object/clr-handle clr-class)) <vector>)
        ((clr-type/is-enum? (clr-object/clr-handle clr-class)) <top>)
        ;; Widen system.object to include everything
        ;; so we can pass ints, strings, etc.
        ((eq? clr-class System.Object) <top>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "SchemeBridge.MzSchemeObject"))) <top>)
        ((eq? clr-class (clr/find-class "System.Char")) <char>)
        ((eq? clr-class (clr/find-class "System.String")) <symbol>)
        ((eq? clr-class (clr/find-class "System.Int32")) <integer>)
        ((eq? clr-class (clr/find-class "System.UInt32")) <integer>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "System.Array"))) <vector>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "SchemeBridge.BridgeArray"))) <vector>)
        ((eq? clr-class (clr/find-class "System.Boolean")) <boolean>)
        ((eq? clr-class (clr/find-class "System.Byte")) <integer>)
        ((eq? clr-class (clr/find-class "System.Single")) <integer>)
        ;;((eq? clr-class (clr/find-class (string->symbol "System.Drawing.Color")))
        ;; (clr/find-class (string->symbol "SchemeBridge.ColorClass")))
        (else clr-class)))

(define (find-or-create-generic table name arity min-arity)
  (define (create-generic)
    (make <clr-generic>
      :arity arity
      :name (StudlyName->key name)
      :StudlyName name))

  (let* ((key-name (StudlyName->key name))
         (generic  (hash-table-get
                    table key-name
                    (lambda ()
                      (let ((generic (create-generic)))
                        (hash-table-put! table key-name generic)
                        generic)))))

    (cond ((instance-of? generic <clr-arity-overload>)
           (ensure-overload-vector-capacity generic arity)
           (let ((arity-vector (get-arity-vector generic)))
             (or (vector-ref arity-vector arity)
                 (let ((generic (create-generic)))
                   (vector-set! arity-vector arity generic)
                   generic))))
          ((= (generic-arity generic) arity) generic)
          (else
           (let ((arity-vector (make-vector (+ (max arity (generic-arity generic)) 1)
                                            #f))
                 (new-generic (create-generic)))
             (vector-set! arity-vector (generic-arity generic) generic)
             (vector-set! arity-vector arity new-generic)
             (hash-table-put! table key-name (make <clr-arity-overload>
                                               :arity (make-arity-at-least min-arity)
                                               :arity-vector arity-vector
                                               :name key-name
                                               :StudlyName name))
             new-generic)))))

  ;; The CLR info is an instance of System.Reflection.MemberInfo
  ;; that describes a property, constructor, enum, method, field, etc.
  ;; We examine what it is and augment (or create) the appropriate generic
  ;; function to handle it.
(define (process-generic clr-info)
  (let* ((member-type/ctor 1)
         (member-type/module-resolve 2)
         (member-type/field 4)
         (member-type/method 8);; enums
         (member-type/property 16)
         (member-type/nested 128)
         (handle (clr-object/clr-handle clr-info))
         (name           (clr-memberinfo/name handle))
         (member-type    (clr-memberinfo/member-type handle))
         (declaring-type (clr-memberinfo/declaring-type handle)))

    (cond ((= member-type member-type/ctor)
           (let* ((constructor-name (clr/StudlyName declaring-type))
                  (min-arity (let ((arity (method-arity clr-info)))
                               (if (arity-at-least? arity)
                                   (arity-at-least-value arity)
                                   arity)))
                  (limit (max-arity clr-info)))
             (do ((argcount min-arity (+ argcount 1)))
                 ((>= argcount limit) (unspecified))
               (add-method (find-or-create-generic *clr-constructors* constructor-name argcount 0) clr-info))))

          ((= member-type member-type/nested)
           ;; (dotnet-message "Ignoring nested type" clr-info)
           (let ((full-name
                             (string-append (clr-object/to-string
                                             (clr-object/clr-handle
                                              declaring-type))
                                            "+"
                                            name)))
             (clr/find-class full-name)))

          ((= member-type member-type/module-resolve))

          ((= member-type member-type/field)
           (if (clr-fieldinfo/is-static? handle)
               (begin
                 ;; Check for a constructor for empty enumerations.
                 ;; Create it if it isn't there.
                 (let ((constructor-name (clr/StudlyName declaring-type)))
                   (hash-table-get *clr-constructors* (StudlyName->key constructor-name)
                                   (lambda ()
                                     (let ((generic (find-or-create-generic *clr-constructors*
                                                                            constructor-name 0 0))
                                           (empty-enumerate (make <clr-enumerate>
                                                              :enumeration declaring-type
                                                              :StudlyName "")))
                                       (if generic
                                           (add-method generic
                                                       (make <clr-enum-constructor>
                                                         :arity 0
                                                         :max-arity 0
                                                         :clr-handle (clr-object/clr-handle declaring-type)
                                                         :specializers (list)
                                                         :procedure (lambda (call-next-method . arguments)
                                                                      empty-enumerate))))))))

                 (let ((value (make <clr-enumerate>
                                :enumeration declaring-type
                                :StudlyName name))
                       (key (StudlyName->key
                             (string-append (clr/StudlyName declaring-type) "."  name))))
                   ;; (dotnet-message "Enum" key "=" value declaring-type)
                   (hash-table-put! *clr-static-fields* key value)))))

;           (if (and
;                (clr/get-property clr-info clr-property-handle/fieldinfo.is-static)
;;                  (let ((field-type (clr/get-property clr-info "FieldType")))
;;                    (not (member field-type
;;                                 (list (clr/find-class "System.Guid")
;;                                       (clr/find-class "System.Decimal")
;;                                       (clr/find-class "System.TimeSpan")
;;                                       (clr/find-class "System.Drawing.Color")
;;                                       (clr/find-class "System.Drawing.Point")
;;                                       (clr/find-class "System.Drawing.PointF")
;;                                       (clr/find-class "System.Drawing.Rectangle")
;;                                       (clr/find-class "System.Drawing.RectangleF")
;;                                       (clr/find-class "System.Drawing.Size")
;;                                       (clr/find-class "System.Drawing.SizeF")))))
;                )

          ((= member-type member-type/property)
           ;; (dotnet-message "Property" clr-info)
           (if (clr-propertyinfo/can-read? handle)
               (if (clr-methodbase/is-static? (clr-propertyinfo/get-get-method handle))
                   (let* ((reader-method (clr-property-info->static-getter-method handle <clr-static-field-getter>))
                          (min-arity (let ((arity (method-arity reader-method)))
                                       (if (arity-at-least? arity)
                                           (arity-at-least-value arity)
                                           arity)))
                          (limit (max-arity reader-method)))
                     (do ((argcount min-arity (+ argcount 1)))
                         ((>= argcount limit) (unspecified))
                       (add-method (find-or-create-generic *clr-static-fields*
                                                           (string-append (clr/StudlyName declaring-type) "."  name)
                                                           argcount
                                                           0)
                         reader-method)))

                   (let* ((reader-method (clr-property-info->getter-method handle
                                                                           <clr-instance-field-getter>))
                          (min-arity (let ((arity (method-arity reader-method)))
                                       (if (arity-at-least? arity)
                                           (arity-at-least-value arity)
                                           arity)))
                          (limit (max-arity reader-method)))
                     (do ((argcount min-arity (+ argcount 1)))
                         ((>= argcount limit) (unspecified))
                       (add-method (find-or-create-generic *clr-instance-field-getters* name
                                                           argcount
                                                           1)
                         reader-method)))))

           (if (clr-propertyinfo/can-write? handle)
               (let* ((writer-method (clr-property-info->setter-method handle
                                                                       <clr-instance-field-setter>))
                      (min-arity (let ((arity (method-arity writer-method)))
                                   (if (arity-at-least? arity)
                                       (arity-at-least-value arity)
                                       arity)))
                      (limit (max-arity writer-method)))
                 (do ((argcount min-arity (+ argcount 1)))
                     ((>= argcount limit) (unspecified))
                   (add-method  (find-or-create-generic *clr-instance-field-setters* name
                                                        argcount
                                                        2)
                     writer-method)))))

          ((= member-type member-type/method)

           (if (or (< (string-length name) 5)
                   (and (not (string=? (substring name 0 4) "get_"))
                        (not (string=? (substring name 0 4) "add_"))
                        (not (string=? (substring name 0 4) "set_"))))
               (let ((min-arity (let ((arity (method-arity clr-info)))
                                  (if (arity-at-least? arity)
                                      (arity-at-least-value arity)
                                      arity)))
                     (limit (max-arity clr-info)))
                 ;; (dotnet-message "Adding methods")
                 (do ((argcount min-arity (+ argcount 1)))
                     ((>= argcount limit) (unspecified))
                   (if (clr-methodbase/is-static? handle)
                       (let* ((static-name (string-append (clr/StudlyName declaring-type) "." name))
                              (generic (find-or-create-generic *clr-static-methods* static-name argcount 0)))
                         ;; (dotnet-message "Adding" clr-info "to static" generic)
                         (add-method generic clr-info))
                       (let ((generic (find-or-create-generic *clr-generics* name argcount 1)))
                         ;; (dotnet-message "Adding" clr-info "to" generic)
                         ;; (describe-instance clr-info)
                         ;; (describe-instance generic)
                         (add-method generic clr-info)))))
               ;; (dotnet-message "Skipping" clr-info)
               ))

          (else (error "Unknown member type" member-type name)))))

;; Since creating generics is going to load a bunch of classes,
;; and we can't create them until a lot of them are in place,
;; we go through and process everything in the class table.
;; When we are done, we loop over the class table and process anything
;; new.  Iterate until we have everything.
;;
;; At the point where everything exists, we can add an after method
;; to create CLR methods on the fly when a new CLR class is
;; instantiated.
(define (initialize-clr-generics!)
  (let loop ((classes-processed '())
             (classes-to-process (list System.RuntimeType)))
    (display ".")
    (flush-output)
    (cond ((pair? classes-to-process)
           (for-each (lambda (class)
                       ;; (dotnet-message "process generics for class" class)
                       (clr-class/for-each-type-member process-generic class))
                     classes-to-process)
           (let* ((processed (set-union classes-to-process classes-processed) )
                  (new (set-difference (list-clr-classes) processed)))
             (loop processed new)))
          ((null? classes-to-process) #f)
          (else (error "Bad list in initialize-clr-generics!"))))

  ;; Now add the after method to cause new class objects to initialize
  ;; the methods on the fly.
  (add-method initialize-instance
              (make (*default-method-class*)
                :arity 2
                :specializers (list System.RuntimeType <top>)
                :procedure (lambda (call-next-method class initargs)
                             ;; (dotnet-message "initialize-instance:after process-generic" class)
                             (clr-class/for-each-type-member process-generic class))
                :qualifier :after))

;    ;; Touch the bridge code to ensure we can manipulate it.
;    (for-each clr/find-class (list "SchemeBridge.Helper"
;                                   "SchemeBridge.FakeTypeBuilder"
;                                   "SchemeBridge.BridgeArray"
;                                   "SchemeBridge.FormProxy"
;                                   "SchemeBridge.ApplicationContext"))


  (dotnet-message "Bootstrapping complete. "
                  (length (list-clr-classes)) "classes,"
                  (length (hash-table-map *clr-constructors* (lambda (key value) #f))) "constructors,"
                  (length (hash-table-map *clr-static-fields* (lambda (key value) #f))) "static fields,"
                  (length (hash-table-map *clr-instance-field-getters* (lambda (key value) #f))) "instance fields getters,"
                  (length (hash-table-map *clr-instance-field-setters* (lambda (key value) #f))) "instance fields setters,"
                  (length (hash-table-map *clr-static-methods* (lambda (key value) #f))) "static methods, and"
                  (length (hash-table-map *clr-generics* (lambda (key value) #f))) "generics.")
  #t)

(define *dotnet-initialized* #f)

  ;; (provide initialize!)
(define (enable-dotnet!)
  (if *dotnet-initialized*
      #f
      (begin
        (newline)
        (display "Initializing dotnet...")
        ;; (flush-output)
        ;; Enable profiling of the CLR so we can go in the back door.
        ;; DONE IN BATCH FILE
        ;; (putenv "Cor_Enable_Profiling" "1")
        ;; (putenv "Cor_Profiler" "MysterX.DotnetProfiler")

        (let ((root-clr-object clr-type-handle/system-type))
          ;; Bootstrap the classes needed to represent CLR objects.
          ;; After this, we can use the marshaling routines.
          (dotnet-message "Bootstrap clr classes.")
          (bootstrap-clr-classes! root-clr-object)
          (dotnet-message "Initialize clr generics.")
          (initialize-clr-generics!)
          (recognize-javadot-symbols? #t)
          (display "done.")))))

;;;; Utilities
;;;
;;; Here's some trivial code that is used above.
(define (set-union left right)
  (cond ((pair? left) (set-union (cdr left)
                                 (if (member (car left) right)
                                     right
                                     (cons (car left) right))))
        ((null? left) right)
        (else (error "set-union: improper list" left))))

(define (set-difference left right)
  (cond ((pair? left) (if (member (car left) right)
                          (set-difference (cdr left) right)
                          (cons (car left) (set-difference (cdr left) right))))
        ((null? left) '())
        (else (error "set-union: improper list" left))))

  ;; Given a procedure of at least ARITY args,
  ;; return a procedure of exacty ARITY args.
(define (nary->fixed-arity procedure arity)
  ;; Why thirty-wonderful flavors?
  ;; The generic function arity is determined by the
  ;; arity of the enclosed methods.  If we used
  ;; a rest arg here, then we'd be creating an n-ary
  ;; generic which we don't want.
  ;;
  ;; Yes, there is a procedure in .NET that takes
  ;; 31 arguments.  I think they left one out.
  (cond ((number? arity)
         (case arity
           ((0) (lambda ()
                  (procedure)))
           ((1) (lambda (arg0)
                  (procedure arg0)))
           ((2) (lambda (arg0 arg1)
                  (procedure arg0 arg1)))
           ((3) (lambda (arg0 arg1 arg2)
                  (procedure arg0 arg1 arg2)))
           ((4) (lambda (arg0 arg1 arg2 arg3)
                  (procedure arg0 arg1 arg2 arg3)))
           ((5) (lambda (arg0 arg1 arg2 arg3 arg4)
                  (procedure arg0 arg1 arg2 arg3 arg4)))
           ((6) (lambda (arg0 arg1 arg2 arg3 arg4 arg5)
                  (procedure arg0 arg1 arg2 arg3 arg4 arg5)))
           ((7) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6)
                  (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)))
           ((8) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)
                  (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
           ((9) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
                  (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))
           ((10) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)))
           ((11) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10)))
           ((12) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11)))
           ((13) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12)))
           ((14) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13)))
           ((15) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14)))
           ((16) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15)))
           ((17) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16)))
           ((18) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17)))
           ((19) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18)))
           ((20) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19)))
           ((21) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20)))
           ((22) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21)))
           ((23) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22)))
           ((24) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23)))
           ((25) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24)))
           ((26) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25)))
           ((27) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26)))
           ((28) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27)))
           ((29) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28)))
           ((30) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29)))
           ((31) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                              arg30)))
           ((32) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30 arg31)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                              arg30 arg31)))
           ((33) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30 arg31 arg32)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                              arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                              arg30 arg31 arg32)))

           (else (error "Need more of nary->fixed arity" arity))))
        ((arity-at-least? arity)
         (case (arity-at-least-value arity)
           ((0) procedure)
           ((1) (lambda (arg0 . rest)
                  (apply procedure arg0 rest)))
           ((2) (lambda (arg0 arg1 . rest)
                  (apply procedure arg0 arg1 rest)))
           ((3) (lambda (arg0 arg1 arg2 . rest)
                  (apply procedure arg0 arg1 arg2 rest)))
           ((4) (lambda (arg0 arg1 arg2 arg3 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 rest)))
           ((5) (lambda (arg0 arg1 arg2 arg3 arg4 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 arg4 rest)))
           ((6) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 rest)))
           ((7) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 rest)))
           ((8) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 rest)))
           ((9) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 . rest)
                  (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 rest)))
           ((10) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 rest)))
           ((11) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 rest)))
           ((12) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 rest)))
           ((13) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 rest)))
           ((14) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 rest)))
           ((15) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 rest)))
           ((16) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 rest)))
           ((17) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 rest)))
           ((18) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 rest)))
           ((19) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 rest)))
           ((20) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 rest)))
           ((21) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 rest)))
           ((22) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 rest)))
           ((23) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 rest)))
           ((24) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 rest)))
           ((25) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 rest)))
           ((26) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 rest)))
           ((27) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 rest)))
           ((28) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 rest)))
           ((29) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 rest)))
           ((30) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29 rest)))
           ((31) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                          arg30 rest)))
           ((32) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30 arg31 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                          arg30 arg31 rest)))
           ((33) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                               arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                               arg30 arg31 arg32 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19
                          arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29
                          arg30 arg31 arg32 rest)))

           (else (error "Need more of nary->fixed arity" arity))))
        (else (error "nary->fixed-arity:  not an arity" arity))))

