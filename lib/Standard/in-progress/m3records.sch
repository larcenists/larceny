; Work-in-progress: single-receiver object system a la Modula-3 records
;
; (define-record-type name 
;   PARENT    parent-type
;   FIELDS    (field-name ...)
;   METHODS   (method-name ...)
;   OVERRIDES (method-name ...))
;
;  where PARENT, FIELDS, METHODS, and OVERRIDES are keywords
;  where parent-type is an expression that evaluates to another record type
;  where field-name is a symbol
;  where method-name is a symbol
;
; Creates a record type called NAME that extends parent-type with fields 
; field-name ... and the usual constructors, predicates, accessors, and 
; mutators, exactly as for DEFINE-RECORD, and in addition defines those
; generic functions in the method-name ... list of METHODS.
;
;
; (define-method generic-fn type proc)
;
;  where generic-fn is an expression evaluating to a generic function
;   defined by define-record-type
;  where type is an expression evaluating to a type s.t. generic-fn
;   dispatches on that type
;  where proc is an expression evaluating to a procedure
;
; Adds the procedure to the generic function when called with a first
; argument of the TYPE.
;
;
; It is an error to call a generic function with an argument that forces
; dispatch to be made to an as-yet-undefined method.
;
;
; eg  (define-record-type shape
;        METHODS (^boundingbox)))
;
;     (define-record-type circle
;        PARENT shape
;        FIELDS (center radius)
;        METHODS (^area)
;        OVERRIDES (^boundingbox))
;
;     (define-record-type point
;        FIELDS (x y))
;
;     (define-method ^area circle
;       (lambda (self) (* PI (expt (circle-radius self) 2))))
;
;     (define-method ^boundingbox circle
;       (lambda (self) 
;         (let ((r (circle-radius self))
;               (c (circle-center self)))
;           (cons (make-point (- (point-y c) r) (- (point-x c) r)))
;                 (make-point (+ (point-y c) r) (+ (point-x c) r))))))
;
;     (^area (make-circle (make-point 0 0) 5)) => 78.54
;     (^boundingbox (make-shape))              => error

(define-syntax define-record-type
  (syntax-rules (PARENT FIELDS METHODS OVERRIDES)
    ((define-record-type "*" ?name #f ?fields ?methods ?overrides
       PARENT ?parent ?clause ...)
     (define-record-type "*" ?name ?parent ?fields ?methods ?overrides
       ?clause ...))
    ((define-record-type "*" ?name ?parent () ?methods ?overrides
       FIELDS ?fields ?clause ...)
     (define-record-type "*" ?name ?parent ?fields ?methods ?overrides
       ?clause ...))
    ((define-record-type "*" ?name ?parent ?fields () ?overrides
       METHODS ?methods ?clause ...)
     (define-record-type "*" ?name ?parent ?fields ?methods ?overrides
       ?clause ...))
    ((define-record-type "*" ?name ?parent ?fields ?methods ()
       OVERRIDES ?overrides ?clause ...)
     (define-record-type "*" ?name ?parent ?fields ?methods ?overrides
       ?clause))
    ((define-record-type "*" ?name ?parent ?fields (?method ...) ?overrides)
     ; FIXME here
     (... 
      (let-syntax 
          ((define-record-methods 
             (syntax-rules ()
               ...))
           (define-generic
             (syntax-rules ()
               ...))
           (define-override
             (syntax-rules ()
               ...)))
        (begin (define ?name (make-record-type "metaclass" '(methods) ur-rtd))
               (define-record-methods ?name ?fields ?name)
               (define-generic ?method ?name) ...
               (define-override ?override ?name) ...))))
    ((define-record-type name clause ...)
     (define-record-type "*" name #f () () () clause ...))))

(define-syntax define-method
  (syntax-rules ()
    ...))
