;; This code depends on Auxlib/record.sch and Lib/MzScheme/inspector.sch

;; http://download.plt-scheme.org/doc/207/html/mzscheme/mzscheme-Z-H-4.html#node_chap_4

;; FIXME:
;;  - immutable-k-list is ignored.


;; These procedures are provided.
(define make-struct-type (undefined))
(define make-struct-type-property (undefined))
(define make-struct-field-accessor (undefined))
(define make-struct-field-mutator (undefined))

(define make-wrapped-waitable (undefined))
(define make-nack-guard-waitable (undefined))
(define make-poll-guard-waitable (undefined))

(define struct? (undefined))
(define struct-type? (undefined))
(define struct-type-property? (undefined))

(define struct-info (undefined))
(define struct-type-info (undefined))
(define struct->vector (undefined))

(define struct-mutator-procedure? (undefined))
(define struct-accessor-procedure? (undefined))
(define struct-predicate-procedure? (undefined))
(define struct-constructor-procedure? (undefined))

;; This shouldn't be visible to MzScheme programs, but the apply
;; code for structure-procedures needs it.
;; Consumes either a struct instance or a struct-type.
;; Produces a procedure if there is one, or (undefined)
(define $sys.struct-proc-spec (undefined))
(define $sys.struct-ref)

;; define-record is nowhere to be found.
(let* ((*rtd-type* (record-type-descriptor (make-record-type "" '())))
       (get-slots (record-accessor *rtd-type* 'slot-offsets))
       (get-printer (record-accessor *rtd-type* 'printer))
       (get-hier-vector (record-accessor *rtd-type* 'hierarchy-vector))
       (get-hier-depth (record-accessor *rtd-type* 'hierarchy-depth))
       (get-record-size (record-accessor *rtd-type* 'record-size))
       
       ;; The struct-type-descriptor type is a subtype of the
       ;; record-type-descriptor type.  (Say that five times fast!)
       (*std-type*
        (make-record-type
         'struct-type-descriptor
         '(auto-v prop-values inspector proc immutable-k-list)
         *rtd-type*))
       (*struct-field-offset* 1)
       
       (make-stype (record-constructor *std-type*))
        
       (stype-auto-v (record-accessor *std-type* 'auto-v))
       (stype-prop-values (record-accessor *std-type* 'prop-values))
       (stype-inspector (record-accessor *std-type* 'inspector))
       (stype-proc (record-accessor *std-type* 'proc))
       (stype-immutable-k-list (record-accessor *std-type* 'immutable-k-list))
       (stype? (record-predicate *std-type*)))
  
  ;; drop the first n elements of lst
  (define (drop n lst)
    (if (zero? n)
        lst
        (drop (- n 1) (cdr lst))))

  ;; generate (list 0 1 ... n-1)
  (define (nats-to n)
    (let loop ((c (- n 1))
               (l '()))
      (if (< c 0)
          l
          (loop (- c 1)
                (cons c l)))))
  
  ;; A structure type is a record consisting of
  ;; - a record type descriptor
  ;; - a list of (struct-type-property-descriptor . any)
  ;; - an inspector
  ;; - proc-spec
  ;; - immutable-k-list
  (define make-struct-type*
    (let ((offset->name
           (lambda (n) (string->symbol
                        (string-append "field-" (number->string n))))))
      (lambda (name super init-field-k auto-field-k . rest)
        ;; no opt-lambda, sorry...
        (let* ((defaults '(#f ;; auto-fill value
                           () ;; property value list
                           ;; go one up to get an opaque type
                           ($sys.inspector->superior (current-inspector))
                           #f ;; structure procedure
                           ())) ;; list of immutable field indices
               (opts (append rest
                             (drop (length rest) defaults)))
               (opts (list->vector opts)))
          
          (let ((auto-v (vector-ref opts 0))
                (prop-values (vector-ref opts 1))
                (inspector (vector-ref opts 2))
                (proc-spec (vector-ref opts 3))
                (immutable-k-list (vector-ref opts 4))
                
                (field-names
                 (map offset->name
                      (nats-to (+ init-field-k auto-field-k)))))
            
            ;; Make a record-type, and then use accessors to transfer
            ;; the data into a struct-type
            (let ((rtd (make-record-type (symbol->string name)
                                         field-names
                                         super)))
              (let ((hierarchy-vec (get-hier-vector rtd))
                    (hierarchy-depth (get-hier-depth rtd)))
                    
                (let ((st (make-stype
                           (record-type-name rtd)
                           (get-slots rtd)
                           (get-printer rtd)
                           (get-record-size rtd)
                           hierarchy-vec
                           hierarchy-depth
                           ;;
                           auto-v
                           prop-values
                           inspector
                           proc-spec
                           immutable-k-list)))
                  ;; Still need to invoke a bit of voodoo:
                  ;; make-record-type leaves the hierarchy-vector entry
                  ;; as a record-type-descriptor, but we want our
                  ;; shiny new struct-type-descriptor there instead.
                  (vector-set! hierarchy-vec hierarchy-depth st)

                  (let ((constructor (record-constructor st))
                        (predicate (record-predicate st))
                        (accessor (record-indexer st))
                        (mutator (record-mutator st)))
                
                    (values st
                            constructor
                            predicate
                            accessor
                            mutator))))))))))

  (define make-struct-type-property*
    (case-lambda
      ((name) (make-struct-type-property* name #f))
      ((name guard-proc)
       (let ((prop:p (make-record-type name '())))
         
         (define (p? x)
           (cond ((struct?* x) '...)
                 ((struct-type?* x) '...)
                 (else '...)))

         (define (p-ref x)
           (cond ((struct?* x) '...)
                 ((struct-type?* x) '...)
                 (else '...)))
           
         (values prop:p p? p-ref)
         ))))
  
  (define make-struct-field-accessor* (undefined))
  (define make-struct-field-mutator* (undefined))
  
  (define make-wrapped-waitable*  (undefined))
  (define make-nack-guard-waitable*  (undefined))
  (define make-poll-guard-waitable*  (undefined))

  ;; struct? is wrong.  see 4.8.
  (define struct?* record?)
  (define struct-type?*
    (lambda (t) (stype? t)))
  
  (define struct-type-property?*  (undefined))
  
  (define struct-info*  (undefined))
  (define struct-type-info*  (undefined))
  (define struct->vector*  (undefined))
  
  (define struct-mutator-procedure?*  (undefined))
  (define struct-accessor-procedure?*  (undefined))
  (define struct-predicate-procedure?*  (undefined))
  (define struct-constructor-procedure?* (undefined))

  ;; this isn't exported... different from struct?
  (define (struct-instance? obj)
    (and (record? obj)
         (struct-type? (record-type-descriptor obj))))

  ;; given an instance, return its type's proc-spec
  (define sys:struct-proc-spec
    (lambda (instance)
      (let ((type (record-type-descriptor instance)))
        (stype-proc-spec type))))

  (define sys:struct-ref
    (lambda (instance index)
      (vector-like-ref instance (+ index *struct-field-offset*))))
  
  ;; Hook up the implementation with the interface.
  (set! make-struct-type make-struct-type*)
  (set! make-struct-type-property make-struct-type-property*)
  (set! make-struct-field-accessor make-struct-field-accessor*)
  (set! make-struct-field-mutator make-struct-field-mutator*)
  
  (set! make-wrapped-waitable make-wrapped-waitable*)
  (set! make-nack-guard-waitable make-nack-guard-waitable*)
  (set! make-poll-guard-waitable make-poll-guard-waitable*)
  
  (set! struct? struct?*)
  (set! struct-type? struct-type?*)
  (set! struct-type-property? struct-type-property?*)
  
  (set! struct-info struct-info*)
  (set! struct-type-info struct-type-info*)
  (set! struct->vector struct->vector*)
  
  (set! struct-mutator-procedure? struct-mutator-procedure?*)
  (set! struct-accessor-procedure? struct-accessor-procedure?*)
  (set! struct-predicate-procedure? struct-predicate-procedure?*)
  (set! struct-constructor-procedure? struct-constructor-procedure?*)

  (set! $sys.struct-proc-spec sys:struct-proc-spec)
  (set! $sys.struct-ref sys:struct-ref)
  )
  
;; Quick and dirty test case
;; Larceny doesn't seem to have define-values
(define tup)
(define mk-tup)
(define tup?)
(define tup-ref)
(define tup-set!)
(let-values (((type cons pred ref set)
              (make-struct-type 'tup #f 2 0)))
  (set! tup type)
  (set! mk-tup cons)
  (set! tup? pred)
  (set! tup-ref ref)
  (set! tup-set! set))

(define triple)
(define mk-triple)
(define triple?)
(define triple-ref)
(define triple-set!)
(let-values (((type cons pred ref set)
              (make-struct-type 'triple tup 1 0)))
  (set! triple type)
  (set! mk-triple cons)
  (set! triple? pred)
  (set! triple-ref ref)
  (set! triple-set set))