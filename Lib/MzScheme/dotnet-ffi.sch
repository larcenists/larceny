
(define (ffi:get-type name)
  (if (string? name)
      (syscall 34 0 name)
      (error "get-type: expected a string, got " name)))

(define (ffi:get-method type name argtypes)
  (if (not (and (foreign? type) (ffi:isa? type type%)))
      (error "ffi:get-method: expected type, got: " type))
  (if (not (string? name))
      (error "ffi:get-method: expected string, got: " name))
  (syscall 34 1 type name (list->vector argtypes)))

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
     (if (foreign? obj) obj (ffi:datum->foreign 'schemeobject obj)))
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
     (if (and (foreign? obj) (ffi:isa? obj schemeobject%))
         (ffi:foreign->datum 'schemeobject obj)
         obj))
    ((bool) (ffi:equal? foreign-true obj))
    (else 
     (error "ffi:foreign->datum: unknown conversion: " conversion))))

(define (ffi:invoke method obj . args)
  (syscall 34 4 method obj (list->vector args)))

(define (ffi:get-field type name)
  (syscall 34 5 type name))

(define (ffi:get-property type name)
  (syscall 34 6 type name))

(define (ffi:isa? obj type)
  (syscall 34 7 obj type))

(define (ffi:field-get f obj)
  (syscall 34 8 f obj))

(define (ffi:field-set f obj value)
  (syscall 34 9 f obj value))

(define (foreign? obj)
  (syscall 34 10 obj))

(define (ffi:get-constructor type argtypes)
  (syscall 34 11 type (list->vector argtypes)))

(define (ffi:construct c args)
  (syscall 34 12 c (list->vector args)))

(define (ffi:equal? a b)
  (syscall 34 13 a b))

(define (foreign-or-fixnum? x)
  (or (fixnum? x) (foreign? x)))

(define object% (ffi:get-type "System.Object"))
(define type% (ffi:get-type "System.Type"))
(define string% (ffi:get-type "System.String"))
(define int% (ffi:get-type "System.Int32"))
(define array% (ffi:get-type "System.Array"))
(define schemeobject% (ffi:get-type "Scheme.Rep.SObject"))
(define ffi% (ffi:get-type "Scheme.RT.FFI"))

(define (ffi:ensure-type t client)
  (cond ((string? t)
         (ffi:get-type t))
        ((and (foreign? t) (ffi:isa? t type%))
         t)
        (else
         (error client " expected type, got: " t))))

(define foreign-false
  (ffi:field-get (ffi:get-field ffi% "FALSE") #f))
(define foreign-true
  (ffi:field-get (ffi:get-field ffi% "TRUE") #f))
(define foreign-null
  (ffi:field-get (ffi:get-field ffi% "NULL") #f))

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
  (let ((fi (ffi:get-field type name)))
    (if (not fi)
        (error "clr-field->procedures: no such field"))
    (cons
     (lambda (obj) (ffi:field-get fi obj))
     (lambda (obj newval) (ffi:field-set fi obj newval)))))

(define (clr-property->procedures type name)
  (let ((pi (ffi:get-property type name)))
    (if (not pi)
        (error "clr-property->procedures: no such property"))
    (let ((pg (car pi))
          (ps (cdr pi)))
      (cons
       (and pg (lambda (obj) (ffi:invoke pg obj)))
       (and ps (lambda (obj newval) (ffi:invoke ps obj newval)))))))

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
