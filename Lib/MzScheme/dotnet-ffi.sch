;($$trace "dotnet-ffi")

(define-syntax define-ffi
  (syntax-rules ()
    ((define-ffi name code ...)
     (define-syntax name
       (syntax-rules ()
         ((name . args)
          (syscall code ... . args)))))))

(define-ffi ffi:%get-type          34 0)
(define-ffi ffi:%get-method        34 1)

(define-ffi ffi:%object->foreign       34 2 0)
(define-ffi ffi:%schemeobject->foreign 34 2 1)
(define-ffi ffi:%string->foreign       34 2 2)
;(define-ffi ffi:%symbol->foreign      34 2 3)
(define-ffi ffi:%bytes->foreign        34 2 4)
(define-ffi ffi:%int32->foreign        34 2 5)
(define-ffi ffi:%flonum->foreign       34 2 6)
(define-ffi ffi:%double->foreign       34 2 7)
;(define-ffi ffi:%void->foreign        34 2 8)

(define-ffi ffi:%foreign->object       34 3 0)
(define-ffi ffi:%foreign->schemeobject 34 3 1)
(define-ffi ffi:%foreign->string       34 3 2)
;(define-ffi ffi:%symbol->foreign      34 3 3)
(define-ffi ffi:%foreign->bytes        34 3 4)
(define-ffi ffi:%foreign->int          34 3 5)
(define-ffi ffi:%foreign->flonum       34 3 6)
(define-ffi ffi:%foreign->double       34 3 7)
(define-ffi ffi:%foreign->void         34 3 8)

(define-ffi ffi:%invoke                34 4)
(define-ffi ffi:%get-field             34 5)
(define-ffi ffi:%get-property          34 6)
(define-ffi ffi:%isa?                  34 7)
(define-ffi ffi:%field-get             34 8)
(define-ffi ffi:%field-set             34 9)
(define-ffi %foreign?                  34 10)
(define-ffi ffi:%get-constructor       34 11)
(define-ffi ffi:%invoke-constructor    34 12)
(define-ffi ffi:%eq?                   34 13)
(define-ffi ffi:%property-get          34 14)
(define-ffi ffi:%property-set          34 15)
(define-ffi ffi:%type-as-string        34 16)
(define-ffi ffi:%property-get-bool     34 17)
(define-ffi ffi:%property-get-int      34 18)
(define-ffi ffi:%foreign-aref          34 19)

(define array%        (ffi:%get-type "System.Array"))
(define enum%         (ffi:%get-type "System.Enum"))
(define ffi%          (ffi:%get-type "Scheme.RT.FFI"))
(define int%          (ffi:%get-type "System.Int32"))
(define object%       (ffi:%get-type "System.Object"))
(define schemeobject% (ffi:%get-type "Scheme.Rep.SObject"))
(define string%       (ffi:%get-type "System.String"))
(define type%         (ffi:%get-type "System.Type"))

(define foreign-false
  (ffi:%field-get (ffi:%get-field ffi% "FALSE") #f))
(define foreign-true
  (ffi:%field-get (ffi:%get-field ffi% "TRUE") #f))
(define foreign-null
  (ffi:%field-get (ffi:%get-field ffi% "NULL") #f))

(define (foreign? object) (%foreign object))
(define (ffi:isa? object type) (ffi:%isa? object type))
(define (ffi:eq? left right) (or (eq? left right) (ffi:%eq? left right)))

(define (foreign-null? object)
  (ffi:%eq? object foreign-null))

(define (ffi:get-type name)
  (if (string? name)
      (ffi:%get-type name)
      (error "get-type: expected a string, got " name)))

(define (ffi:get-method type name argtypes)
  (if (not (and (%foreign? type) (ffi:%isa? type type%)))
      (error "ffi:get-method: expected type, got: " type))
  (if (not (string? name))
      (error "ffi:get-method: expected string, got: " name))
  (ffi:%get-method type name (list->vector argtypes)))

(define (ffi:bool->foreign obj)   (if obj foreign-true foreign-false))
(define (ffi:symbol->foreign obj) (ffi:%string->foreign (symbol->string obj)))
(define (ffi:foreign->bool   obj) (not (ffi:%eq? obj foreign-false)))
(define (ffi:foreign->double obj) (ffi:%foreign->double obj))
(define (ffi:foreign->int    obj) (ffi:%foreign->int obj))
(define (ffi:foreign->symbol obj) (string->symbol (ffi:%foreign->string obj)))

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
     (if (and (%foreign? obj) (ffi:%isa? obj schemeobject%))
         (ffi:foreign->datum 'schemeobject obj)
         obj))
    ((bool) (ffi:eq? foreign-true obj))
    (else
     (error "ffi:foreign->datum: unknown conversion: " conversion))))

(define (ffi:invoke method obj . args)
  (ffi:%invoke method obj (list->vector args)))

(define (ffi:get-constructor type argtypes)
  (ffi:%get-constructor type (list->vector argtypes)))

(define (ffi:construct c args)
  (ffi:%invoke-constructor c (list->vector args)))

(define (foreign-or-fixnum? x)
  (or (fixnum? x) (%foreign? x)))

(define (ffi:ensure-type t client)
  (cond ((string? t)
         (ffi:%get-type t))
        ((and (%foreign? t) (ffi:%isa? t type%))
         t)
        (else
         (error client " expected type, got: " t))))

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
  (let ((fi (ffi:%get-field type name)))
    (if (not fi)
        (error "clr-field->procedures: no such field"))
    (cons
     (lambda (obj) (ffi:%field-get fi obj))
     (lambda (obj newval) (ffi:%field-set fi obj newval)))))

(define (clr-property->procedures type name)
  (let ((pi (ffi:%get-property type name)))
    (if (not pi)
        (error "clr-property->procedures: no such property"))
    (cons
     (lambda (obj . args) (ffi:%property-get pg obj (list->vector args)))
     (lambda (obj newval . args) (ffi:%property-set ps obj newval (list->vector args))))))

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
