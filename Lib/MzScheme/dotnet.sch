;; -*-Scheme-*-
;; Port of dotnet.ss to larceny

;; NOTE:  This file requires that dotnet-ffi.sch has been loaded.

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

(define (void)
  (if #f #f))

(define (void? thing) (eq? thing (void)))
(define (flush-output) #f)

;;;

(define *empty-vector* (list->vector '()))

;;; Bootstrapping types
(define clr-type-handle/system-array                    (ffi:get-type "System.Array"))
(define clr-type-handle/system-enum                     (ffi:get-type "System.Enum"))
(define clr-type-handle/system-int32                    (ffi:get-type "System.Int32"))
(define clr-type-handle/system-object                   (ffi:get-type "System.Object"))
(define clr-type-handle/system-reflection-fieldinfo     (ffi:get-type "System.Reflection.FieldInfo"))
(define clr-type-handle/system-reflection-memberinfo    (ffi:get-type "System.Reflection.MemberInfo"))
(define clr-type-handle/system-reflection-methodbase    (ffi:get-type "System.Reflection.MethodBase"))
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

(define-ffi-predicate clr-array?  clr-type-handle/system-array)
(define-ffi-predicate clr-enum?   clr-type-handle/system-enum)
(define-ffi-predicate clr-int32?  clr-type-handle/system-int32)
(define-ffi-predicate clr-string? clr-type-handle/system-string)

;;; Bootstrapping methods
(define clr-method-handle/array.getvalue
  (ffi:get-method clr-type-handle/system-array "GetValue" (list clr-type-handle/system-int32)))
(define clr-method-handle/object.tostring  (ffi:get-method clr-type-handle/system-object "ToString" '()))
(define clr-method-handle/object.type      (ffi:get-method clr-type-handle/system-object "GetType" '()))
(define clr-method-handle/type.get-members (ffi:get-method clr-type-handle/system-type "GetMembers" '()))
(define clr-method-handle/methodbase.get-parameters
  (ffi:get-method clr-type-handle/system-reflection-methodbase "GetParameters" '()))
(define clr-method-handle/propertyinfo.get-index-parameters
  (ffi:get-method clr-type-handle/system-reflection-propertyinfo "GetIndexParameters" '()))

;;; Bootstrapping fields

;;; Bootstrapping properties
(define clr-property-handle/array.length       (car (ffi:%get-property clr-type-handle/system-array "Length")))
(define clr-property-handle/type.base-type     (car (ffi:%get-property clr-type-handle/system-type "BaseType")))
(define clr-property-handle/type.is-array      (car (ffi:%get-property clr-type-handle/system-type "IsArray")))
(define clr-property-handle/type.is-by-ref     (car (ffi:%get-property clr-type-handle/system-type "IsByRef")))
(define clr-property-handle/type.is-enum       (car (ffi:%get-property clr-type-handle/system-type "IsEnum")))
(define clr-property-handle/type.is-interface  (car (ffi:%get-property clr-type-handle/system-type "IsInterface")))
(define clr-property-handle/type.is-pointer    (car (ffi:%get-property clr-type-handle/system-type "IsPointer")))
(define clr-property-handle/type.is-value-type (car (ffi:%get-property clr-type-handle/system-type "IsValueType")))

(define clr-property-handle/fieldinfo.field-type
  (car (ffi:%get-property clr-type-handle/system-reflection-fieldinfo "FieldType")))

(define clr-property-handle/fieldinfo.is-static
  (car (ffi:%get-property clr-type-handle/system-reflection-fieldinfo "IsStatic")))

(define clr-property-handle/memberinfo.declaring-type
  (car (ffi:%get-property clr-type-handle/system-reflection-memberinfo "DeclaringType")))
(define clr-property-handle/memberinfo.member-type
  (car (ffi:%get-property clr-type-handle/system-reflection-memberinfo "MemberType")))
(define clr-property-handle/memberinfo.name
  (car (ffi:%get-property clr-type-handle/system-reflection-memberinfo "Name")))
(define clr-property-handle/memberinfo.reflected-type
  (car (ffi:%get-property clr-type-handle/system-reflection-memberinfo "ReflectedType")))

(define clr-property-handle/methodbase.is-static
  (car (ffi:%get-property clr-type-handle/system-reflection-methodbase "IsStatic")))

(define clr-property-handle/parameterinfo.is-optional
  (car (ffi:%get-property clr-type-handle/system-reflection-parameterinfo "IsOptional")))
(define clr-property-handle/parameterinfo.parameter-type
  (car (ffi:%get-property clr-type-handle/system-reflection-parameterinfo "ParameterType")))

(define clr-property-handle/propertyinfo.can-read
  (car (ffi:%get-property clr-type-handle/system-reflection-propertyinfo "CanRead")))
(define clr-property-handle/propertyinfo.can-write
  (car (ffi:%get-property clr-type-handle/system-reflection-propertyinfo "CanWrite")))
(define clr-property-handle/propertyinfo.property-type
  (car (ffi:%get-property clr-type-handle/system-reflection-propertyinfo "PropertyType")))

;;; Base functions with default marshaling

(define (clr-array/length array)
  (ffi:%foreign->int (ffi:invoke clr-property-handle/array.length array)))

(define (clr-array/ref array index)
  (ffi:invoke clr-method-handle/array.getvalue array (ffi:%int32->foreign index)))

;; performance
(define-syntax clr-object/to-string
  (syntax-rules ()
    ((clr-object/to-string object)
     (ffi:%foreign->string (ffi:%invoke clr-method-handle/object.tostring object *empty-vector*)))))

(define (clr-object/to-symbol object)
  (ffi:foreign->symbol (ffi:invoke clr-method-handle/object.tostring object)))

;; performance
(define-syntax clr-object/type
  (syntax-rules ()
    ((clr-object/type object)
     (ffi:%invoke clr-method-handle/object.type object *empty-vector*))))

(define (clr-type/base-type type)
  (ffi:invoke clr-property-handle/type.base-type type))

(define (clr-type/get-members type)
  (ffi:invoke clr-method-handle/type.get-members type))

(define (clr-type/is-array? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-array type)))

(define (clr-type/is-by-ref? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-by-ref type)))

(define (clr-type/is-enum? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-enum type)))

(define (clr-type/is-interface? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-interface type)))

(define (clr-type/is-pointer? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-pointer type)))

(define (clr-type/is-value-type? type)
  (ffi:foreign->bool (ffi:invoke clr-property-handle/type.is-value-type type)))

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

  ;; A hash table mapping symbols to the Swindle classes that
  ;; represent .NET classes.  The symbolic key will simply be the
  ;; name of the class as a case-folded symbol.
(define *clr-type-table* (make-hash-table 'symbol-eq?))

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
(define class-key/system.enum               (StudlyName->key "System.Enum"))
(define class-key/system.int32              (StudlyName->key "System.Int32"))
(define class-key/system.typed-reference    (StudlyName->key "System.TypedReference"))
(define class-key/system.reflection.pointer (StudlyName->key "System.Reflection.Pointer"))
(define class-key/system.single             (StudlyName->key "System.Single"))
(define class-key/system.string             (StudlyName->key "System.String"))
(define class-key/system.uint32             (StudlyName->key "System.UInt32"))
(define class-key/system.value-type         (StudlyName->key "System.ValueType"))

(define (register-dotnet-class! StudlyName class)
  (hash-table-put! *clr-type-table* (StudlyName->key StudlyName) class))

(define (map-dotnet-classes function)
  (hash-table-map *clr-type-table* (lambda (key value) (function value))))

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

(define clr-object/clr-handle (make (*default-generic-class*) :name 'clr-object/clr-handle))

(add-method
 clr-object/clr-handle
 (make (*default-method-class*)
   :arity 1
   :specializers (list System.Object)
   :procedure (lambda (call-next-method x) (slot-ref x 'clr-handle))))

(add-method print-object
  (make (*default-method-class*)
    :specializers (list System.Object)
    :arity 3
    :procedure ((lambda ()
                 (define (print-object call-next-method object port slashify)
                   (let* ((clr-object  (clr-object/clr-handle object))
                          (type-name   (clr-object/to-string (clr-object/type clr-object)))
                          (printed-rep (clr-object/to-string clr-object)))

                     (display "#<" port)
                     (display type-name port)
                     (if (not (string=? type-name printed-rep))
                         (begin
                           (display " " port)
                           (display printed-rep port)))
                     (display ">" port)))
                 print-object))))

(define <clr-enumerate>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-class-class*)
         (list :direct-default-initargs #f
               :direct-supers (list)
               :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName)
                                   (list 'enumeration :initarg :enumeration :reader 'enumerate/enumeration))
               :name '<clr-enumerate>)))
    (let ((<clr-enumerate>
           (rec-allocate-instance
            (*default-class-class*)
            (list :direct-default-initargs #f
                  :direct-supers (list)
                  :direct-slots (list (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName)
                                      (list 'enumeration :initarg :enumeration :reader 'enumerate/enumeration))
                  :name '<clr-enumerate>))))
      (rec-initialize <clr-enumerate>
                      (list :direct-default-initargs #f
                            :direct-supers (list)
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

(define enumerate/enumeration (make (*default-generic-class*) :name 'clr-object/clr-handle))

(add-method
 enumerate/enumeration
 (make (*default-method-class*)
   :arity 1
   :specializers (list <clr-enumerate>)
   :procedure (lambda (call-next-method x) (slot-ref x 'enumeration))))

(define (clr/enumerate-union left right)
  (if (eq? (enumerate/enumeration left)
           (enumerate/enumeration right))
      ;; should test for flags attribute, but how?
      (make <clr-enumerate>
        :StudlyName (string-append (clr/StudlyName left) ", " (clr/StudlyName right))
        :enumeration (enumerate/enumeration left))
      (error "Mismatched enumeration." left right)))

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
            (set-instance-class-to-self! System.RuntimeType)

            ;; Set the clr-object slot and put this class in the
            ;; type table.
            (slot-set! System.RuntimeType 'clr-handle this-type)
            (slot-set! System.RuntimeType 'StudlyName (symbol->string this-name))
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
            (slot-set! System.RuntimeType 'slots (compute-slots System.RuntimeType)))
          (loop this-type this-name))))

  ;; Now we need to find the com object associated with System.Object.
  ;; This time we walk the type hierarchy in `BaseType' direction.
  (let loop ((this (clr-object/clr-handle System.RuntimeType)))
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

(define (find-clr-type clr-type-name)
  (or (ffi:get-type  clr-type-name)
      (error "Couldn't FIND-CLR-TYPE " clr-type-name)))

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
            (function (clr/marshal-in (clr-array/ref members idx)))
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

  ;; The CLR-TYPE-DESCRIPTOR is a COM interop proxy to a .NET RuntimeType
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
       ;(dotnet-message "Instantiating class object for" StudlyName)
       ;; Not found?  Create one.
       (make System.RuntimeType
         :name clr-type-name
         :StudlyName StudlyName
         :clr-handle clr-type-descriptor
         :direct-supers
         (list
          ;; As it turns out, the "BaseType" property is *not* a reliable
          ;; means to figure out the base type.  This COND special cases the
          ;; known problems.
          (let ((bt-property (clr-type/base-type clr-type-descriptor)))
            (if (or (not bt-property)
                    (null? bt-property)
                    (void? bt-property)
                    (foreign-null? bt-property))
                (begin
                  (dotnet-message "No base type returned for" StudlyName)
                  (cond ((clr-type/is-array? clr-type-descriptor)
                         ;;(error "Array Type?" clr-type-name)
                         (dotnet-message "Using System.Array")
                         (clr/find-class "System.Array"))

                        ((clr-type/is-by-ref? clr-type-descriptor)
                         (dotnet-message "Using System.TypedReference")
                         (clr/find-class "System.TypedReference"))

                        ((clr-type/is-pointer? clr-type-descriptor)
                         (dotnet-message "Using System.Reflection.Pointer")
                         (clr/find-class "System.Reflection.Pointer"))

                        ;; Interfaces inherit from System.Object implicitly
                        ;; should also check for `base interfaces'
                        ((clr-type/is-enum? clr-type-descriptor)
                         (dotnet-message "Using System.Enum")
                         (clr/find-class "System.Enum"))

                        ((clr-type/is-interface? clr-type-descriptor)
                         (dotnet-message "Using System.Object")
                         System.Object)

                        ((clr-type/is-value-type? clr-type-descriptor)
                         (dotnet-message "Using System.ValueType")
                         (clr/find-class "System.ValueType"))

                        ;; Special case CLR bug?
                        ((eq? clr-type-name 'system.enum)
                         (dotnet-message "Using System.ValueType")
                         (clr/find-class "System.ValueType"))
                        (else (error "Dunno what the base type is." clr-type-name))))
                (clr-object->class bt-property)))))))))

  ;; Create an instance of a CLR class to represent the com interop proxy.
(define (clr-object->clr-instance clr-object)
  ;; NOTE CAREFULLY
  ;;  When a CLR class is returned from the COM interop,
  ;;  we *don't* want to call the MAKE function on the metaclass
  ;;  (i.e., System.RuntimeType) because that will cause a non-equivalent
  ;;  instantiation of the CLR class.
  (let ((class (clr-object->class (clr-object/type clr-object))))
    (if (eq? class System.RuntimeType)
        (clr-object->class clr-object)
        (make class
          :clr-handle clr-object))))

  ;; We unwrap the CLR instance when we call out to foreign code.
(define clr/marshal-out (make (*default-generic-class*) :name 'clr/marshal-out))

(add-method
 clr/marshal-out
 (make (*default-method-class*)
   :arity 1
   :specializers (list <top>)
   :procedure (lambda (call-next-method x) x)))

(add-method
 clr/marshal-out
 (make (*default-method-class*)
   :arity 1
   :specializers (list System.Object)
   :procedure (lambda (call-next-method object) (clr-object/clr-handle object))))

(define (clr/marshal-in object)
  (cond ((clr-array? object) (let loop ((index 0)
                                        (limit (clr-array/length object))
                                        (result '()))
                               (if (>= index limit)
                                   (list->vector (reverse! result))
                                   (loop (+ index 1) limit (cons (clr/marshal-in (clr-array/ref object index)) result)))))
        ((clr-enum? object) (ffi:%foreign->int object))
        ((clr-int32? object) (ffi:%foreign->int object))
        ((clr-string? object) (ffi:foreign->symbol object))
        ((foreign-null? object) '())
        ((ffi:%eq? object foreign-true) #t)
        ((ffi:%eq? object foreign-false) #f)
        (else (clr-object->clr-instance object))))

  ;; The payoff.
  ;; Given a CLR object, a method, and some args, call the method.
  ;; (or get the property, etc.)
(define (clr/get-type object)
  (clr/marshal-in
   (clr-object/type
    (clr-object/clr-handle object))))

(define-syntax clr/invoke
  (syntax-rules ()
    ((clr/invoke object method)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object) *empty-vector*)))
    ((clr/invoke object method arg0)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector (clr/marshal-out arg0)))))
    ((clr/invoke object method arg0 arg1)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector (clr/marshal-out arg0)
                           (clr/marshal-out arg1)))))
    ((clr/invoke object method arg0 arg1 arg2)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector (clr/marshal-out arg0)
                           (clr/marshal-out arg1)
                           (clr/marshal-out arg2)))))
    ((clr/invoke object method arg0 arg1 arg2 arg3)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector (clr/marshal-out arg0)
                           (clr/marshal-out arg1)
                           (clr/marshal-out arg2)
                           (clr/marshal-out arg3)))))
    ((clr/invoke object method arg0 arg1 arg2 arg3 arg4)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector (clr/marshal-out arg0)
                           (clr/marshal-out arg1)
                           (clr/marshal-out arg2)
                           (clr/marshal-out arg3)
                           (clr/marshal-out arg4)))))
    ((clr/invoke object method arg0 arg1 arg2 arg3 arg4 arg5)
     (clr/marshal-in
      (ffi:%invoke method (clr-object/clr-handle object)
                   (vector
                    (clr/marshal-out arg0)
                    (clr/marshal-out arg1)
                    (clr/marshal-out arg2)
                    (clr/marshal-out arg3)
                    (clr/marshal-out arg4)
                    (clr/marshal-out arg5)))))))

(define-syntax clr/get-property
  (syntax-rules ()
    ((clr/get-property object property-handle)
     (clr/marshal-in
      (ffi:%invoke property-handle (clr-object/clr-handle object) *empty-vector*)))))

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

(define <clr-reflected-method>
  (let ((<clr-reflected-method>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list
           :direct-default-initargs #f
           :direct-supers (list <method>)
           :direct-slots (list (list 'clr-method :initarg :clr-method))
           :name '<clr-reflected-method>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs #f
          :direct-supers (list <method>)
          :direct-slots (list (list 'clr-method :initarg :clr-method))
          :name '<clr-reflected-method>)))
    (rec-initialize
     <clr-reflected-method>
     (list
      :direct-default-initargs #f
      :direct-supers (list <method>)
      :direct-slots (list (list 'clr-method :initarg :clr-method))
      :name '<clr-reflected-method>))
    <clr-reflected-method>))

(define <clr-constructor-method>
  (let ((<clr-constructor-method>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list)
                :name '<clr-constructor-method>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list)
                :name '<clr-constructor-method>)))
    (rec-initialize
     <clr-constructor-method>
     (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list)
                :name '<clr-constructor-method>))
    <clr-constructor-method>))

(define <clr-enum-constructor>
  (let ((<clr-enum-constructor>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list )
                :name '<clr-enum-constructor>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots (list )
               :name '<clr-enum-constructor>)))
                                        ;(display "; Initialize <clr-enum-constructor>")(newline)
    (rec-initialize
     <clr-enum-constructor>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots (list )
           :name '<clr-enum-constructor>))
    <clr-enum-constructor>))

(define <clr-instance-method>
  (let ((<clr-instance-method>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list )
                :name '<clr-instance-method>))))
                                        ;(dotnet-message "check clr-instance-method")
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots (list )
               :name '<clr-instance-method>)))
                                        ;(dotnet-message "initialize clr-instance-method")
    (rec-initialize
     <clr-instance-method>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots (list )
           :name '<clr-instance-method>))
                                        ;(dotnet-message "Made clr-instance-method")
    <clr-instance-method>))

(define <clr-instance-field-getter>
  (let ((<clr-instance-field-getter>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <method>)
                :direct-slots (list (list 'clr-field :initarg :clr-field))
                :name '<clr-instance-field-getter>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <method>)
               :direct-slots (list  (list 'clr-field :initarg :clr-field))
               :name '<clr-instance-field-getter>)))
    (rec-initialize
     <clr-instance-field-getter>
     (list :direct-default-initargs #f
           :direct-supers (list <method>)
           :direct-slots (list  (list 'clr-field :initarg :clr-field))
           :name '<clr-instance-field-getter>))
    <clr-instance-field-getter>))

(define <clr-instance-field-setter>
  (let ((<clr-instance-field-setter>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <method>)
                :direct-slots (list (list 'clr-field :initarg :clr-field))
                :name '<clr-instance-field-setter>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <method>)
               :direct-slots (list (list 'clr-field :initarg :clr-field))
               :name '<clr-instance-field-setter>)))
    (rec-initialize
     <clr-instance-field-setter>
     (list :direct-default-initargs #f
           :direct-supers (list <method>)
           :direct-slots (list (list 'clr-field :initarg :clr-field))
           :name '<clr-instance-field-setter>))
    <clr-instance-field-setter>))

(define <clr-static-method>
  (let ((<clr-static-method>
         (rec-allocate-instance
          (*default-entityclass-class*)
          (list :direct-default-initargs #f
                :direct-supers (list <clr-reflected-method>)
                :direct-slots (list )
                :name '<clr-static-method>))))
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list :direct-default-initargs #f
               :direct-supers (list <clr-reflected-method>)
               :direct-slots (list )
               :name '<clr-static-method>)))
    (rec-initialize
     <clr-static-method>
     (list :direct-default-initargs #f
           :direct-supers (list <clr-reflected-method>)
           :direct-slots (list )
           :name '<clr-static-method>))
    <clr-static-method>))

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

(define *clr-static-fields* (make-hash-table 'symbol-eq?))

(define *clr-instance-field-getters* (make-hash-table 'symbol-eq?))

(define *clr-instance-field-setters* (make-hash-table 'symbol-eq?))

(define <clr-generic>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs #f
          :direct-supers (list <generic>)
          :direct-slots (list (list 'my-arity :initarg :my-arity)
                              (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
          :name '<clr-generic>)))
    (let ((<clr-generic> (rec-allocate-instance
                          (*default-entityclass-class*)
                          (list
                           :direct-default-initargs #f
                           :direct-supers (list <generic>)
                           :direct-slots (list (list 'my-arity :initarg :my-arity)
                                               (list 'StudlyName :initarg :StudlyName :reader 'clr/StudlyName))
                           :name '<clr-generic>))))
      (rec-initialize
       <clr-generic>
       (list
        :direct-default-initargs #f
        :direct-supers (list <generic>)
        :direct-slots (list (list 'my-arity :initarg :my-arity)
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
                     (display (clr/StudlyName object) port)
                     (display ">" port))
                 print-object))))

(define <clr-arity-overload>
  (begin
    (if (*make-safely*)
        (check-initargs
         (*default-entityclass-class*)
         (list
          :direct-default-initargs #f
          :direct-supers (list <clr-generic>)
          :direct-slots (list (list 'arity-vector :initarg :arity-vector))
          :name '<clr-arity-overload>)))

    (let ((<clr-arity-overload>
           (rec-allocate-instance
            (*default-entityclass-class*)
            (list
             :direct-default-initargs #f
             :direct-supers (list <clr-generic>)
             :direct-slots (list (list 'arity-vector :initarg :arity-vector))
             :name '<clr-arity-overload>))))
      (rec-initialize
       <clr-arity-overload>
       (list
        :direct-default-initargs #f
        :direct-supers (list <clr-generic>)
        :direct-slots (list (list 'arity-vector :initarg :arity-vector))
        :name '<clr-arity-overload>))
      <clr-arity-overload>)))

(add-method initialize-instance
            (make (*default-method-class*)
              :arity 2
              :specializers (list <clr-arity-overload> <top>)
              :procedure (lambda (call-next-method generic initargs)
                           ;(dotnet-message "Initialize clr-arity-overload" initargs)
                           (add-method
                            generic
                            (make (*default-method-class*)
                              :arity (make-arity-at-least 1)
                              :specializers (list <top>)
                              :procedure (lambda (call-next-method . arguments)
                                           (let ((argcount (length arguments)))
                                             (let ((vector (slot-ref generic 'arity-vector)))
                                               (if (not (< argcount (vector-length vector)))
                                                   (error "Too many arguments to overloaded method."
                                                          generic
                                                          argcount)
                                                   (let ((target ( vector-ref vector argcount)))
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

(define (ensure-overload-vector-capacity overload desired-arity)
  (let* ((vector (slot-ref overload 'arity-vector))
         (capacity (vector-length vector)))
    (if (<= capacity desired-arity)
        (begin
          ;; copy the elements
          (let loop ((new-vector (make-vector (+ desired-arity 1) #f))
                     (index 0))
            (if (= index capacity)
                (slot-set! overload 'arity-vector new-vector)
                (begin (vector-set! new-vector index
                                    (vector-ref vector index))
                       (loop new-vector (+ index 1)))))))))

(define (map-parameter-class clr-class)
  (cond ((clr/get-property clr-class clr-property-handle/type.is-array) <vector>)
        ((clr/get-property clr-class clr-property-handle/type.is-enum) <top>)
        ;; Widen system.object to include everything
        ;; so we can pass ints, strings, etc.
        ((eq? clr-class System.Object) <top>)
        ;; ((eq? clr-class (clr/find-class (string->symbol "SchemeBridge.MzSchemeObject"))) <top>)
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

(define (find-or-create-generic table name arity)
  (define (create-generic)
    (make <clr-generic>
      :my-arity arity
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
           (let ((arity-vector (slot-ref generic 'arity-vector)))
             (or (vector-ref arity-vector arity)
                 (let ((generic (create-generic)))
                   (vector-set! arity-vector arity generic)
                   generic))))
          ((= (slot-ref generic 'my-arity) arity) generic)
          (else
           (let ((arity-vector (make-vector (+ (max arity
                                                       (slot-ref generic 'my-arity)) 1)
                                            #f))
                 (new-generic (create-generic)))
             (vector-set! arity-vector (slot-ref generic 'my-arity) generic)
             (vector-set! arity-vector arity new-generic)
             (hash-table-put! table key-name (make <clr-arity-overload>
                                               :arity-vector arity-vector
                                               :name key-name
                                               :StudlyName name))
             new-generic)))))

  ;; The CLR info is an instance of System.Reflection.MemberInfo
  ;; that describes a property, constructor, enum, method, field, etc.
  ;; We examine what it is and augment (or create) the appropriate generic
  ;; function to handle it.
(define (process-generic clr-info)
  ;; (dotnet-message "Process-generic" clr-info)
  (let ((member-type/ctor 1)
        (member-type/module-resolve 2)
        (member-type/field 4)
        (member-type/method 8);; enums
        (member-type/property 16)
        (member-type/nested 128)
        (name           (clr/get-property clr-info clr-property-handle/memberinfo.name))
        (member-type    (clr/get-property clr-info clr-property-handle/memberinfo.member-type))
        (declaring-type (clr/get-property clr-info clr-property-handle/memberinfo.declaring-type)))

    ;(dotnet-message "Processing" name "of declaring type" (clr/StudlyName declaring-type) member-type)

    (cond ((= member-type member-type/ctor)
           ;(dotnet-message "Constructor " declaring-type)
           (if (clr/get-property clr-info clr-property-handle/methodbase.is-static)
               (error "Constructor is Static")
               (let* ((constructor-name (clr/StudlyName declaring-type))
                      (parameter-list (map (lambda (parameter)
                                             (map-parameter-class
                                              (clr/get-property parameter clr-property-handle/parameterinfo.parameter-type)))
                                           (vector->list (clr/invoke clr-info clr-method-handle/methodbase.get-parameters))))
                      (generic (find-or-create-generic *clr-constructors* constructor-name
                                                       (length parameter-list))))
                 ;; (dotnet-message "Parameter list is" parameter-list)
                 (if generic
                     (add-method generic
                                 (make <clr-constructor-method>
                                   :clr-method clr-info
                                   :specializers parameter-list
                                   :procedure (lambda (call-next-method . arguments)
                                        ;(dotnet-message "I'm here...")
                                                (clr/invoke *dotnet-helper-object*
                                                            "ConstructorCall"
                                                            clr-info
                                                            (list->bridge-array arguments)))))))))

          ((= member-type member-type/nested)
           ;; (dotnet-message "Ignoring nested type" clr-info)
           (let ((full-name (string->symbol
                             (string-append (symbol->string (clr-base/object.to-string declaring-type))
                                            "+"
                                            (symbol->string name)))))
             (clr/find-class full-name)))

          ((= member-type member-type/module-resolve))

          ((= member-type member-type/field)
;           ;; (dotnet-message "Member-type/enum" name)
;           ;; (dotnet-message "static?" (clr/get-property clr-info "IsStatic"))
;           ;; (dotnet-message "FieldType" (clr/get-property clr-info "FieldType"))
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

;               (begin
;                 ;; Check for a constructor for empty enumerations.
;                 ;; Create it if it isn't there.
;                 (let ((constructor-name (clr/StudlyName declaring-type)))
;                   (hash-table-get *clr-constructors* constructor-name
;                                   (lambda ()
;                                     (let ((generic (find-or-create-generic *clr-constructors*
;                                                                            constructor-name 0))
;                                           (empty-enumerate (make <clr-enumerate>
;                                                              :enumeration declaring-type
;                                                              :StudlyName "")))
;                                       (if generic
;                                           (add-method generic
;                                                       (make <clr-enum-constructor>
;                                                         :clr-method declaring-type
;                                                         :specializers (list)
;                                                         :procedure (lambda (call-next-method &rest arguments)
;                                                                      empty-enumerate))))))))

;                 (let ((value (make <clr-enumerate>
;                                :enumeration declaring-type
;                                :StudlyName (symbol->string name)))
;                       (key (StudlyName->key
;                             (string-append (clr/StudlyName declaring-type) "."  (symbol->string name)))))
;                                        ;(dotnet-message "Enum" key "=" value declaring-type)
;                   (hash-table-put! *clr-static-fields* key value))))
           )

          ((= member-type member-type/property)
           ;(dotnet-message "Property" clr-info)
           (let* ((parameter-list (map (lambda (parameter)
                                         (map-parameter-class
                                          (clr/get-property parameter clr-property-handle/parameterinfo.parameter-type)))
                                       (vector->list (clr/invoke clr-info clr-method-handle/propertyinfo.get-index-parameters))))
                  (lenpl (length parameter-list))
                  (generic-getter
                   (if (clr/get-property clr-info clr-property-handle/propertyinfo.can-read)
                       (find-or-create-generic *clr-instance-field-getters* (symbol->string name)
                                               (+ lenpl 1))
                       #f))
                  (generic-setter
                   (if (clr/get-property clr-info clr-property-handle/propertyinfo.can-read)
                       (find-or-create-generic *clr-instance-field-setters* (symbol->string name)
                                               (+ lenpl 2))
                       #f)))
             (if generic-getter
                 (add-method generic-getter
                             (make <clr-instance-field-getter>
                               :clr-field clr-info
                               :specializers (cons declaring-type parameter-list)
                               :procedure (make-property-getter-call lenpl clr-info))))
             (if generic-setter
                 (let ((property-type (map-parameter-class (clr/get-property clr-info clr-property-handle/propertyinfo.property-type))))
                   (add-method generic-setter
                               (make <clr-instance-field-setter>
                                 :clr-field clr-info
                                 :specializers (cons declaring-type (append parameter-list (list property-type)))
                                 :procedure (make-property-setter-call lenpl clr-info)))))))


          ((= member-type member-type/method)

           (if (or (< (string-length (symbol->string name)) 5)
                   (and (not (string=? (substring (symbol->string name) 0 4) "get_"))
                        (not (string=? (substring (symbol->string name) 0 4) "add_"))
                        (not (string=? (substring (symbol->string name) 0 4) "set_"))))
               (begin
                 ;(dotnet-message "Method" clr-info)
                 (let* ((name (if (and (> (string-length (symbol->string name)) 4)
                                       (string=? (substring (symbol->string name) 0 3) "op_"))
                                  (string->symbol (substring (symbol->string name) 3 (string-length (symbol->string name))))
                                  name))
                        (raw-parameter-list (vector->list (clr/invoke clr-info clr-method-handle/methodbase.get-parameters)))
                        (parameter-types (map (lambda (parameter)
                                        ;(dotnet-message "parameter is" parameter)
                                        ;(dotnet-message "type is" (clr/get-property parameter clr-property-handle/parameterinfo.parameter-type))
                                                (map-parameter-class (clr/get-property parameter clr-property-handle/parameterinfo.parameter-type)))
                                              raw-parameter-list))
                                        ;(ignore1 (dotnet-message "Parameter types are" parameter-types))
                        (n-args (lambda (n)
                                  (if (clr/get-property clr-info clr-property-handle/methodbase.is-static)
                                      ;; Handle static methods
                                      (let* ((static-name (string-append (clr/StudlyName declaring-type) "." (symbol->string name)))
                                             (generic (find-or-create-generic *clr-static-methods* static-name n)))
                                        ;(dotnet-message "Static" name)
                                        ;(dotnet-message "Generic is " generic)
                                        (if generic
                                            (add-method generic
                                                        (make <clr-static-method>
                                                          :clr-method clr-info
                                                          :specializers (list-head parameter-types n)
                                                          :procedure (make-static-call n clr-info)))))

                                      ;; Dynamic methods have implicit THIS.
                                      (let ((generic (find-or-create-generic *clr-generics* (symbol->string name) (+ n 1))))

                                        ;(dotnet-message "Generic" name (cons declaring-type (list-head parameter-types n)))
                                        (if generic
                                            (add-method generic
                                                        (make <clr-instance-method>
                                                          :clr-method clr-info
                                                          :specializers (cons declaring-type
                                                                              (list-head parameter-types n))
                                                          :procedure (make-dynamic-call n clr-info)))))))))

                   (let loop ((n (length parameter-types)))
                     (n-args n)
                     (if (and (positive? n)
                              (begin
                                (clr/get-property (list-ref raw-parameter-list (- n 1)) clr-property-handle/parameterinfo.is-optional)))
                         (loop (- n 1))))))))

          (else (error "Unknown member type" member-type name)))))

(define (make-property-getter-call arity clr-info)
  (nary->fixed-arity (lambda (call-next-method clr-object . args)
                       (clr/invoke clr-object
                                   clr-info
                                   (list->vector args)))
                     (+ arity 2)))

(define (make-property-setter-call arity clr-info)
  (nary->fixed-arity (lambda (call-next-method clr-object new-value . args)
                       (clr/invoke clr-object
                                   clr-info
                                   new-value
                                   (list->vector args)))
                     (+ arity 3)))

(define (make-static-call arity clr-info)
  (nary->fixed-arity (lambda (call-next-method . args)
                       (clr/invoke clr-info (list->vector args)))
                     (+ arity 1)))

(define (make-dynamic-call arity clr-method)
  (nary->fixed-arity
   (lambda (call-next-method clr-object . args)
     ;(dotnet-message "Dynamic call " clr-object clr-method args)
     (clr/marshal-in
      (ffi:%invoke (clr-object/clr-handle clr-method)
                   (clr-object/clr-handle clr-object)
                   (list->vector (map clr/marshal-out args)))))
   (+ arity 2)))

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

(define *scheme-dynamic-assembly* #f)
(define *scheme-dynamic-module*   #f)
(define *scheme-bridging-object*  #f)

(define (initialize-dynamic-assembly! application-domain)
  (let ((assembly-name (clr/marshal-from-com (cci/progid "System.Reflection.AssemblyName"))))
    ;; No way to create it with the right name.  You have to create it
    ;; then bash the name.
    (clr/set-property! assembly-name "Name" "SchemeDynamicAssembly")
    (or (eq? (clr/get-property assembly-name "Name") '|SchemeDynamicAssembly|)
        (error "Could not assign AssemblyName.Name"))
    ;; Create the dynamic assembly.
    (set! *scheme-dynamic-assembly*
          (clr/invoke application-domain "DefineDynamicAssembly"
                      assembly-name
                      3                 ; 3 means that we want to run in memory and/or save to disk
                      ))))

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
          (bootstrap-clr-classes! root-clr-object)
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
