
;; vector-struct defines a set of functions for dealing with struct-like 
;; vectors. A symbolic tag distinguishes different struct types (and aids
;; readability). Getters and setters are defined with the given names;
;; setters may be #f, in which case none is defined, but getters must be 
;; identifiers.

(define-syntax vector-struct
  (syntax-rules ()
    ((_ tag make pred (getter setter) ...)
     (vector-struct/h 'tag make pred 1 () ((getter setter) ...)))))

(define-syntax vector-struct/h
  (syntax-rules ()
    ((_ tag make pred next-index
        ((getter setter index) ...) ((getter0 setter0) (getterz setterz) ...))
     (vector-struct/h tag make pred (+ 1 next-index)
                      ((getter0 setter0 next-index) (getter setter index) ...)
                      ((getterz setterz) ...)))
    ((_ tag make pred next-index
        ((getter setter index) ...) ())
     (begin
       (define (make getter ...) (vector tag getter ...))
       (define (pred obj) (and (vector? obj) 
                               (> (vector-length obj) 0)
                               (eq? tag (vector-ref obj 0))))
       (define (getter obj)
         (if (pred obj)
             (vector-ref obj index)
             (error "vector-struct (" 'getter "): expected " tag ", given " obj)))
       ...
       (vector-struct/define-setter setter (obj val)
         (if (pred obj)
             (vector-set! obj index val)
             (error "vector-struct (" 'setter "): expected " tag ", given " obj))) 
       ...))))

(define-syntax vector-struct/define-setter
  (syntax-rules () 
    ((_ #f args body)
     (begin))
    ((_ setter args body)
     (define (setter . args) body))))

;;(vector-struct '$$foo make-foo foo? (foo.a foo.a!) (foo.b foo.b!))

;; Growable vector (from util.sch)
(vector-struct $$growable-vector make-growable-vector growable-vector?
               (growable-vector.elements growable-vector.elements!)
               (growable-vector.default growable-vector.default!))

;; (from pass5p2.sch)
(vector-struct $$user-data raw-make-user-data user-data?
               (user-data.il-namespace user-data.il-namespace!)
               (user-data.toplevel-counter user-data.toplevel-counter!)
               (user-data.proc-counter user-data.proc-counter!)
               (user-data.label-counter user-data.label-counter!)
               (user-data.label-map user-data.label-map!))

(vector-struct $$cvclass make-cvclass cvclass?
               (cvclass-il-namespace cvclass-il-namespace!)
               (cvclass-id cvclass-id!)
               (cvclass-instrs cvclass-instrs!)
               (cvclass-constants cvclass-constants!)
               (cvclass-label-count cvclass-label-count!))

;; (from il-gen.sch)
(vector-struct $$il raw:make-il il?
               (il.code #f)
               (il.args #f))
(vector-struct $$il-delay raw:make-il-delay il-delay?
               (il-delay.il #f))

(vector-struct $$il-class make-il-class il-class?
	       (il-class.assembly #f)
	       (il-class.namespaces #f)
	       (il-class.name #f))
(vector-struct $$il-primtype  make-il-primtype  il-primtype?
	       (il-primtype.string #f)
	       (il-primtype.class #f))
(vector-struct $$il-classtype make-il-classtype il-classtype?
	       (il-classtype.class #f))
(vector-struct $$il-arraytype make-il-arraytype il-arraytype?
	       (il-arraytype.basetype #f))
(vector-struct $$il-method make-il-method il-method?
	       (il-method.instance? #f)
	       (il-method.type #f)
	       (il-method.class #f)
	       (il-method.name #f)
	       (il-method.argtypes #f))
(vector-struct $$il-label make-il-label il-label?
	       (il-label.key #f))
(vector-struct $$il-field make-il-field il-field?
	       (il-field.type #f)
	       (il-field.class #f)
	       (il-field.name #f))

;; (from dumpheap-il.sch)
(vector-struct $$clr-class make-clr-class clr-class?
               (clr-class-name clr-class-name!)
               (clr-class-il-namespace clr-class-il-namespace!)
               (clr-class-super clr-class-super!)
               (clr-class-options clr-class-options!)
               (clr-class-members clr-class-members!))

(vector-struct $$clr-method make-clr-method clr-method?
               (clr-method-name clr-method-name!)
               (clr-method-type clr-method-type!)
               (clr-method-argtypes clr-method-argtypes!)
               (clr-method-options clr-method-options!)
               (clr-method-instrs clr-method-instrs!))

(vector-struct $$field make-field field?
               (field-name field-name!)
               (field-type field-type!)
               (field-options field-options!))
