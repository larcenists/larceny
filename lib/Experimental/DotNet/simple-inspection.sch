;;; Useful procedures defined below:

;;; describe-type
;;; type->name type->full-name 
;;; type->superclass type->ancestry type->interfaces
;;; type->constructors 
;;; type->fields
;;; type->properties
;;; type->events
;;; type->methods

;;; prop-ref/name : Foreign String -> Foreign

(define type-type                (find-clr-type "System.Type"))

(define (make-prop-ref/symbolic type property-name)
  (let ((handle (clr/%get-property type property-name '#())))
    (lambda (obj-in-type)
      (string->symbol
       (clr/foreign->string (clr/%property-ref handle obj-in-type '#()))))))
(define type->name-property-ref
  (lambda (type)
    (make-prop-ref/symbolic type "Name")))

(define type->name (type->name-property-ref type-type))
(define type->full-name (make-prop-ref/symbolic type-type "FullName"))
(define (type->superclass t)
  (let ((bt (clr-type/%base-type t)))
    (if (clr/%null? bt)
        #f
        bt)))
(define (type->ancestry t)
  (let ((s (type->superclass t)))
    (if s (cons t (type->ancestry s)) (list t))))
(define (type->interfaces t)
  (clr-array->list (clr-type/%get-interfaces t)))

(define constructor-info-type
  (find-clr-type "System.Reflection.ConstructorInfo"))
(define constructor-info->parameters
  (let ((handle (clr/%get-method constructor-info-type "GetParameters" '#())))
    (lambda (ci)
      (clr-array->list (clr/%invoke handle ci '#())))))
(define type->constructors
  (let ((type-get-constructors-method
         (clr/%get-method type-type "GetConstructors" '#())))
    (lambda (t)
      (clr-array->list 
       (clr/%invoke type-get-constructors-method t '#())))))

(define field-info-type
  (find-clr-type "System.Reflection.FieldInfo"))
(define field-info->name (type->name-property-ref field-info-type))
(define field-info->type
  (let ((handle (clr/%get-property field-info-type "FieldType" '#())))
    (lambda (fi)
      (clr/%property-ref handle fi '#()))))
(define type->fields
  (let ((type-get-fields-method
         (clr/%get-method type-type "GetFields" '#())))
    (lambda (t)
      (clr-array->list (clr/%invoke type-get-fields-method t '#())))))

(define property-info-type
  (find-clr-type "System.Reflection.PropertyInfo"))
(define property-info->name (type->name-property-ref property-info-type))
(define property-info->type
  (let ((handle (clr/%get-property property-info-type "PropertyType" '#())))
    (lambda (pi)
      (clr/%property-ref handle pi '#()))))
(define type->properties
  (let ((type-get-properties-method
         (clr/%get-method type-type "GetProperties" '#())))
    (lambda (t)
      (clr-array->list (clr/%invoke type-get-properties-method t '#())))))

(define event-info-type 
  (find-clr-type "System.Reflection.EventInfo"))
(define event-info->name (type->name-property-ref event-info-type))
(define event-info->handler-type
  (let ((handle (clr/%get-property event-info-type "EventHandlerType" '#())))
    (lambda (ei)
      (clr/%property-ref handle ei '#()))))
(define type->events
  (let ((type-get-events-method
         (clr/%get-method type-type "GetEvents" '#())))
    (lambda (t)
      (clr-array->list (clr/%invoke type-get-events-method t '#())))))


(define method-info-type
  (find-clr-type "System.Reflection.MethodInfo"))
(define parameter-info-type
  (find-clr-type "System.Reflection.ParameterInfo"))
(define method-info->name (type->name-property-ref method-info-type))
(define method-info->parameters
  (let ((handle (clr/%get-method method-info-type "GetParameters" '#())))
    (lambda (mi)
      (clr-array->list (clr/%invoke handle mi '#())))))
(define method-info->return-parameter
  (let ((handle (clr/%get-property method-info-type "ReturnParameter" '#())))
    (lambda (mi)
      (clr/%property-ref handle mi '#()))))
(define parameter-info->type
  (let ((handle (clr/%get-property parameter-info-type "ParameterType" '#())))
    (lambda (pi)
      (clr/%property-ref handle pi '#()))))
(define parameter-info->name (type->name-property-ref property-info-type))
(define type->methods
  (let* ((type-get-methods-method
          (clr/%get-method type-type "GetMethods" '#()))
         (name-method 
          (clr/%get-property method-info-type "Name" '#()))
         (property-related? 
          (lambda (mi)
            (let* ((name (clr/foreign->string
                          (clr/%property-ref name-method mi '#()))))
              (and (>= (string-length name) 5)
                   (or (string=? (substring name 0 4) "get_")
                       (string=? (substring name 0 4) "set_"))))))
         (event-related?
          (lambda (mi)
            (let ((name (clr/foreign->string
                         (clr/%property-ref name-method mi '#()))))
              (or (and (>= (string-length name) 5)
                       (string=? (substring name 0 4) "add_"))
                  (and (>= (string-length name) 8)
                       (string=? (substring name 0 7) "remove_")))))))
    (lambda (t)
      (filter 
       (lambda (mi) (and (not (property-related? mi))
                         (not (event-related? mi))))
       (clr-array->list (clr/%invoke type-get-methods-method t '#()))))))

(define (describe-usual-type t)
  (let ((super-type  (let ((s (type->superclass t))) (if s (list(type->full-name s))'())))
        (field-names (map field-info->name    (type->fields t)))
        (prop-names  (map property-info->name (type->properties t)))
        (event-names (map event-info->name    (type->events t)))
        (meth-names  (map method-info->name   (type->methods t))))
    (define (maybe name values)
      (if (null? values) '() `((,name ,@values))))
    `(,@(maybe 'superclass: super-type)
      ,@(maybe 'field: field-names)
      ,@(maybe 'properties: prop-names)
      ,@(maybe 'events: event-names)
      ,@(maybe 'methods: meth-names))))
(define describe-enum-type
  (let* ((enum-type (find-clr-type "System.Enum"))
         (get-names-method
          (clr/%get-method enum-type "GetNames" (vector type-type))))
    (lambda (t)
      (let* ((array (clr/%invoke get-names-method #f (vector t)))
             (names (map clr/foreign->string (clr-array->list array)))
             (syms  (map string->symbol names)))
        (if (memq (string->symbol "System.FlagsAttribute")
                  (clr-type/get-custom-attributes t))
            `(enum-flags: ,@syms)
            `(enum-values: ,@syms))))))

(define subclass? 
  (let ((is-subclass-of-method (clr/%get-method type-type
                                                 "IsSubclassOf"
                                                 (vector type-type)))
        (equals-method (clr/%get-method type-type "Equals" (vector type-type))))
    (lambda (t s)
      (or 
       (clr/foreign->bool (clr/%invoke equals-method t (vector s)))
       (clr/foreign->bool (clr/%invoke is-subclass-of-method t (vector s)))))))

(define enum-type? 
  (let ((enum-type (find-clr-type "System.Enum")))
    (lambda (t)
      (subclass? t enum-type))))

(define (describe-type t)
  (if (enum-type? t)
      (describe-enum-type t)
      (describe-usual-type t)))

(define describe-type-extension
  (lambda (t)
    (define (difference lst1 lst2)
      (let loop ((l lst1))
        (cond ((not l) '())
              ((null? l) '())
              ((member (car l) lst2) (loop (cdr l)))
              (else (cons (car l) (loop (cdr l)))))))
    (define (maybe name values)
      (if (null? values) '() `((,name ,@values))))
    (let* ((s (type->superclass t))
           (super-type-name  (if s (list(type->full-name s))'()))
           (t-field-names (map field-info->name    (type->fields t)))
           (t-prop-names  (map property-info->name (type->properties t)))
           (t-event-names (map event-info->name    (type->events t)))
           (t-meth-names  (map method-info->name   (type->methods t)))
           (s-field-names (map field-info->name    (type->fields s)))
           (s-prop-names  (map property-info->name (type->properties s)))
           (s-event-names (map event-info->name    (type->events s)))
           (s-meth-names  (map method-info->name   (type->methods s)))
           (d-field-names (difference t-field-names s-field-names))
           (d-prop-names  (difference t-prop-names  s-prop-names))
           (d-event-names (difference t-event-names s-event-names))
           (d-meth-names  (difference t-meth-names  s-meth-names)))
      `(,@(maybe 'superclass: super-type-name)
        ,@(maybe 'field: d-field-names)
        ,@(maybe 'properties: d-prop-names)
        ,@(maybe 'events: d-event-names)
        ,@(maybe 'methods: d-meth-names)))))

(define (prop-ref/name obj propname)
  (let* ((type (clr/%object-type obj))
         (prop (clr/%get-property type propname '#())))
    (clr/%property-ref prop obj '#())))

(define o
  (lambda funcs
    (lambda (arg)
      (let loop ((fs funcs))
        (cond
         ((null? fs) arg)
         (else ((car fs) (loop (cdr fs)))))))))

