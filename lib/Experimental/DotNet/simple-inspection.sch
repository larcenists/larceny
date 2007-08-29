;;; Useful procedures defined below:

;;; describe-type
;;; type->name type->full-name 
;;; type->superclass type->ancestry type->interfaces
;;; type->constructors 
;;; type->fields
;;; type->properties
;;; type->events
;;; type->methods
;;; type->predicate

;;; prop-ref/name : Foreign String -> Foreign

;;; box :   SchemeObject or Foreign -> Foreign
;;; unbox : Foreign -> SchemeObject or Foreign

;;; type->nullary-constructor
;;; type*args&convert->constructor
;;;   int32-arg&convert single-arg&convert string-arg&convert

;;; make-static-method
;;; make-unary-method
;;; make-binary-method

;;; make-property-ref
;;; make-property-setter

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
        (if (enum-flags-type? t)
            `(enum-flags: ,@syms)
            `(enum-values: ,@syms))))))

(define enum-type->symbol->foreign
  (lambda (enum-type)
    (let* ((names (clr-enum/get-names enum-type))
           (vals  (clr-enum/get-values enum-type))
           ;; handles both 'Right and 'right for an enum named "Right"
           (lower-syms  (map string->symbol (map string-downcase names)))
           (cased-syms  (map string->symbol names))
           (lookup-table (append (map list cased-syms vals)
                                 (map list lower-syms vals)))
           (lookup (lambda (s) 
                     (let ((entry (assq s lookup-table)))
                       (if entry (cadr entry)
                           (error 'convert "" (type->name enum-type)
                                  "unknown name" s 
                                  "for possible enums " names))))))
      (if (enum-flags-type? enum-type)
          ;; If flags enum, then accept arbitrary # of args.
          (lambda args
            (clr-enum/to-object 
             enum-type
             (foldr fxlogior 0 (map lookup args))))
          (lambda (arg) ;; (strict subrelation of above)
            (clr-enum/to-object enum-type (lookup arg)))))))
(define enum-type->foreign->symbol
  (let ((get-name-method 
         (clr/%get-method clr-type-handle/system-enum "GetName" 
                          (vector clr-type-handle/system-type
                                  clr-type-handle/system-object)))
        (get-names-method
         (clr/%get-method clr-type-handle/system-enum "GetNames" 
                          (vector clr-type-handle/system-type)))
        (enum-parse-method
         (clr/%get-method clr-type-handle/system-enum "Parse"
                          (vector clr-type-handle/system-type 
                                  clr-type-handle/system-string)))
        (is-flag-val? ;; (powers of two)
         (lambda (x)
           (let loop ((i 1))
             (cond ((= i x) #t)
                   ((> i x) #f)
                   (else (loop (* i 2))))))))
    (lambda (enum-type)
      (if (enum-flags-type? enum-type)

          ;; Just because something is a flagsenum does not mean that all of 
          ;; its members are powers of two.  (See e.g. System.Windows.Forms.Keys)
          ;;
          ;; One (broken) strategy: 
          ;;
          ;; 1. Examine the type to find all of the flag values
          ;; 2. Check for the flags first, mask each one out in turn
          ;; 3. After masking out the flags, lookup residual value
          ;;
          ;; A problem with this is false positives on the flags in
          ;; step 1 (e.g. 32 is space character).  To work around
          ;; this, start at the largest power of two and count down
          ;; until we do not see any.  There are pathological cases
          ;; that this would also fail for, but hopefully they will
          ;; not arise in practice.

          (let* ((array (clr/%invoke get-names-method #f (vector enum-type)))
                 (foreign-names (clr-array->list array))
                 (name->value 
                  (lambda (foreign-name)
                    (clr/%invoke enum-parse-method #f (vector enum-type foreign-name))))
                 (values*names (map (lambda (x) 
                                      (let ((val (name->value x)))
                                        (list (clr/%foreign->int val)
                                              (string->symbol
                                               (string-downcase
                                                (clr/%foreign->string x)))
                                              )))
                                    foreign-names))
                 (all-values (map car values*names))
                 (max-value  (let ((v (apply max all-values))) 
                               (assert (is-flag-val? v)) v))
                 (flag-values*names
                  (let loop ((i max-value) (l '()))
                    (cond ((assv i values*names) =>
                           (lambda (v*n)
                             (loop (/ i 2) (cons v*n l))))
                          (else l)))))
            ;; (begin (display flag-values*names) (newline))
            (lambda (foreign-val)
              (let ((num (if (number? foreign-val) foreign-val
                             (clr/%foreign->int foreign-val))))
                ;; (display num) (newline)
                (let* ((flags*residue 
                        (let loop ((f flag-values*names) (flags '()) (num num))
                          (cond ((null? f) (list flags num))
                                ((not (zero? (fxlogand num (car (car f)))))
                                 (loop (cdr f) 
                                       (cons (cadr (car f)) flags) 
                                       (fxlogand num (fxlognot (car (car f))))))
                                (else
                                 (loop (cdr f) flags num)))))
                       (flags (car flags*residue))
                       (residue (cadr flags*residue)))
                  (cond 
                   ((and (zero? residue) (not (null? flags)))
                    (apply values flags))
                   ((assv residue values*names)
                    => (lambda (value*name)
                         (apply values (cons (cadr value*name) flags))))
                   (else
                    (apply values flags)))))))
          (let ()
            (lambda (foreign-val)
              (string->symbol 
               (string-downcase 
                (clr/foreign->string 
                 (clr/%invoke get-name-method
                              #f (vector enum-type foreign-val)))))))))))
  
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
(define enum-flags-type?
  (let ((flags-attribute (string->symbol "System.FlagsAttribute")))
    (lambda (t)
      (and (enum-type? t)
           (memq flags-attribute
                 (clr-type/get-custom-attributes t))))))

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

(define (box foreign-or-scheme-object)
  (let ((x foreign-or-scheme-object))
    (cond ((%foreign? x) x)
          ((and (number? x) (exact? x))   (clr/%number->foreign-int32 x))
          ((and (number? x) (inexact? x)) (clr/%flonum->foreign-double x))
          ((boolean? x) (clr/bool->foreign x))
          ((string? x)  (clr/%string->foreign x))
          (else (error 'box ": unknown argument type to convert " x)))))
(define (unbox foreign)
  (let ((x foreign))
    (cond ((or (clr/%isa? x clr-type-handle/system-int32)
               (clr/%isa? x clr-type-handle/system-uint32)
               (clr/%isa? x clr-type-handle/system-int64)
               (clr/%isa? x clr-type-handle/system-uint64))
           (clr/%foreign->int x))
          ((or (clr/%isa? x clr-type-handle/system-single))
           (clr/%foreign-single->flonum x))
          ((or (clr/%isa? x clr-type-handle/system-double))
           (clr/%foreign-double->flonum x))
          ((or (clr/%isa? x clr-type-handle/system-string))
           (clr/%foreign->string x))
          ((or (clr/%isa? x clr-type-handle/system-boolean))
           (clr/foreign->bool x))
          (else 
           x))))
(define type->nullary-constructor 
  (lambda (type)
    (let ((ctor (clr/%get-constructor type '#())))
      (lambda ()
        (clr/%invoke-constructor ctor '#())))))
(define (type*args&convert->constructor type . argtypes*converters)
  (let* ((argtypes (map car argtypes*converters))
         (converters (map cadr argtypes*converters))
         (tvec (list->vector argtypes))
         (ctor (clr/%get-constructor type tvec)))
    (lambda actuals
      (if (not (= (vector-length tvec) (length actuals)))
          (error 'constructor (format #t ": ~a requires argument types ~a" 
                                      type tvec))
          (clr/%invoke-constructor
           ctor (list->vector (map (lambda (f x) (f x)) 
                                   converters actuals)))))))
(define make-property-setter 
  (lambda (type property-name-string . maybe-convert)
    (let* ((convert (if (null? maybe-convert) box (car maybe-convert)))
           (prop (clr/%get-property type property-name-string '#())))
      (lambda (obj new-val)
        (clr/%property-set! prop obj (convert new-val) '#())))))
        
(define int32-arg&convert
  (list clr-type-handle/system-int32 clr/%number->foreign-int32))
(define single-arg&convert
  (list clr-type-handle/system-single clr/%flonum->foreign-single))
(define string-arg&convert
  (list clr-type-handle/system-string clr/string->foreign))

(define make-static-method
  (lambda (type method-name-string . arg-types)
    (let ((method (clr/%get-method type method-name-string (list->vector arg-types))))
      (lambda argl
        (cond ((not (= (length argl) (length arg-types)))
               (error (string->symbol method-name-string) 
                      ": argument count mismatch.")))
        (clr/%invoke method #f (list->vector argl))))))
(define make-unary-method
  (lambda (type method-name-string)
    (let ((method (clr/%get-method type method-name-string '#())))
      (lambda (obj)
        (unbox (clr/%invoke method obj '#()))))))
(define make-binary-method
  (lambda (type method-name-string arg-type)
    (let ((method (clr/%get-method type method-name-string (vector arg-type))))
      (lambda (obj arg)
        (unbox (clr/%invoke method obj (vector (box arg))))))))
(define make-property-ref
  (lambda (type property-name-string . maybe-convert)
    (let ((convert (if (null? maybe-convert) unbox (car maybe-convert)))
          (prop (clr/%get-property type property-name-string '#())))
      (lambda (obj)
        (convert (clr/%property-ref prop obj '#()))))))

(define type->predicate
  (lambda (type)
    (lambda (obj)
      (clr/%isa? obj type))))
(define type->name
  (make-property-ref (find-clr-type "System.Type") "Name" 
                     (lambda (x) (string->symbol (clr/foreign->string x)))))
