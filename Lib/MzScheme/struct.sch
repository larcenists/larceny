;; This code depends on Auxlib/record.sch and Lib/MzScheme/inspector.sch

;; http://download.plt-scheme.org/doc/207/html/mzscheme/mzscheme-Z-H-4.html#node_chap_4

;; FIXME / TODO:
;;  - immutable-k-list is ignored.
;;  - struct-procedure fields should be immutable
;;  - auto-fields are broken
;;  - struct? isn't quite right.
;;  - make-struct-type should use struct-type-property guard proc
;;  - inherit things like inspectors, prop-value-lists, struct-procedures
;;  - handle error cases appropriately (relies on having Mz. exception system)


;; These procedures are provided.
(define make-struct-type)
(define make-struct-type-property)
(define make-struct-field-accessor)
(define make-struct-field-mutator)

(define make-wrapped-waitable)
(define make-nack-guard-waitable)
(define make-poll-guard-waitable)

(define struct?)
(define struct-type?)
(define struct-type-property?)

(define struct-info)
(define struct-type-info)
(define struct->vector)

(define struct-mutator-procedure?)
(define struct-accessor-procedure?)
(define struct-predicate-procedure?)
(define struct-constructor-procedure?)

;; define-record is nowhere to be found.
(let* ((*rtd-type* (record-type-descriptor (make-record-type "" '())))
      
       ;; The struct-type-descriptor type is a subtype of the
       ;; record-type-descriptor type.  (Say that five times fast!)
       (*std-type* (make-record-type
                    "struct-type-descriptor"
                    '(init-field-k
                      auto-field-k
                      auto-v
                      prop-values
                      inspector
                      proc
                      immutable-k-list)
                    *rtd-type*))
       (*stype-prop-type* (make-record-type
                           "struct-type-property-descriptor"
                           '(name guard-proc))))

  ;; Accessors for record type descriptors
  (define get-slots (record-accessor *rtd-type* 'slot-offsets))
  (define get-printer (record-accessor *rtd-type* 'printer))
  (define get-hier-vector (record-accessor *rtd-type* 'hierarchy-vector))
  (define get-hier-depth (record-accessor *rtd-type* 'hierarchy-depth))
  (define get-record-size (record-accessor *rtd-type* 'record-size))

  ;; Constructors / Accessors / Predicate for struct type descriptors
  (define make-stype (record-constructor *std-type*)) 
  (define stype-auto-v (record-accessor *std-type* 'auto-v))
  (define stype-prop-values (record-accessor *std-type* 'prop-values))
  (define stype-inspector (record-accessor *std-type* 'inspector))
  (define stype-proc (record-accessor *std-type* 'proc))
  (define stype-immutable-k-list
    (record-accessor *std-type* 'immutable-k-list))
  (define stype? (record-predicate *std-type*))

  ;; Constructors / Accessors / Predicate for struct-type-property
  ;; descriptors
  (define make-stype-prop (record-constructor *stype-prop-type*))
  (define stype-prop-name (record-accessor *stype-prop-type*
                                    'name))
  (define stype-prop-guard-proc (record-accessor *stype-prop-type*
                                          'guard-proc))
  (define stype-prop? (record-predicate *stype-prop-type*))
  
  ;; Tags for the struct-procs created by make-struct-type
  (define sys$tag.struct-constructor-procedure 'struct-constructor-procedure)
  (define sys$tag.struct-predicate-procedure 'struct-predicate-procedure)
  (define sys$tag.struct-accessor-procedure 'struct-accessor-procedure)
  (define sys$tag.struct-mutator-procedure 'struct-mutator-procedure)

  ;; : struct-type -> struct-constructor-procedure
  (define (struct-constructor stype)
    (let ((make/rec (record-constructor stype))
          (proc-spec (stype-proc stype))
          ;; FIXME: gonna have to handle auto-fields too
          )
      (let ((constructor
             (cond ((procedure? proc-spec)
                    (lambda field-vals
                      (make-struct-method proc-spec
                                          (apply make/rec field-vals))))
                   ;; The number will have been range-checked by
                   ;; make-struct-type
                   ((number? proc-spec)
                    (lambda field-vals
                      (let ((proc (list-ref field-vals proc-spec)))
                        ;; Technically this goes against the MzScheme manual.
                        ;; It says that, when (procedure? proc) is #f,
                        ;; the resulting instance should behave as a
                        ;; procedure that cannot be applied.
                        ;; It's more useful to signal an error in the
                        ;; constructor.
                        (if (procedure? proc)
                            (make-struct-proc proc
                                              (apply make/rec field-vals))
                            (raise-type-error (string->symbol
                                               (string-append
                                                "make-"
                                                (record-type-name stype)))
                                              "procedure"
                                              proc-spec
                                              field-vals)))))
                            
                   (else make/rec))))
                   
        (make-struct-proc constructor
                          sys$tag.struct-constructor-procedure))))
      
  (define (struct-constructor-procedure?* obj)
    (and (struct-proc? obj)
         (eq? (struct-proc-extra obj)
              sys$tag.struct-constructor-procedure)))

  ;; struct-type -> struct-predicate-procedure
  (define (struct-predicate stype)
    (let ((instance-of-stype? (record-predicate stype))
          (proc-spec (stype-proc stype)))

      (let ((predicate
             (if (or (procedure? proc-spec)
                     (number? proc-spec))
                 (lambda (obj)
                   (and (struct-proc? obj)
                        (instance-of-stype? (struct-proc-extra obj))))
                 instance-of-stype?)))
        (make-struct-proc predicate
                          sys$tag.struct-predicate-procedure))))
  
  (define (struct-predicate-procedure?* obj)
    (and (struct-proc? obj)
         (eq? (struct-proc-extra obj)
              sys$tag.struct-predicate-procedure)))

  ;; struct-type * struct-predicate-procedure -> struct-accessor-procedure
  (define (struct-accessor stype instance-of-stype?)
    (let ((rec-index (record-indexer stype))
          (proc-spec (stype-proc stype)))
      (let* ((throw-type-error (lambda (obj index)
                                 (raise-type-error
                                  (string->symbol
                                   (string-append
                                    (record-type-name stype) "-ref"))
                                  (record-type-name stype)
                                  0 ; 1st argument no good
                                  (list obj index))))
             (accessor
              (if (or (procedure? proc-spec)
                      (number? proc-spec))
                  (lambda (struct-proc index)
                    (if (instance-of-stype? struct-proc)
                        (rec-index (struct-proc-extra struct-proc)
                                   index)
                        (throw-type-error struct-proc index)))
                  (lambda (obj index)
                    (if (instance-of-stype? obj)
                        (rec-index obj)
                        (throw-type-error obj index))))))
        (make-struct-proc accessor
                          sys$tag.struct-accessor-procedure))))
  
  (define (struct-accessor-procedure?* obj)
    (and (struct-proc? obj)
         (eq? (struct-proc-extra obj)
              sys$tag.struct-accessor-procedure)))

  ;; struct-type * struct-predicate-procedure -> struct-mutator-procedure
  (define (struct-mutator stype instance-of-stype?)
    (let ((rec-set!  (record-mutator stype))
          (proc-spec (stype-proc stype)))
      (let* ((throw-type-error (lambda (obj index new-val)
                                 (raise-type-error
                                  (string->symbol
                                   (string-append
                                    (record-type-name stype) "-set!"))
                                  (record-type-name stype)
                                  0
                                  (list obj index new-val))))
             (mutator
              (if (or (procedure? proc-spec)
                      (number? proc-spec))
                  (lambda (obj index new-value)
                    (if (instance-of-stype? obj)
                        (let ((instance (struct-proc-extra obj)))
                          (rec-set! instance index new-value)
                          (set-struct-proc-extra! obj instance)
                          obj)
                        (throw-type-error obj index new-value)))
                  (lambda (obj index new-value)
                    (if (instance-of-stype? obj)
                        (rec-set! obj index new-value)
                        (throw-type-error obj index new-value))))))
      (make-struct-proc mutator
                        sys$tag.struct-mutator-procedure))))
  (define (struct-mutator-procedure?* obj)
    (and (struct-proc? obj)
         (eq? (struct-proc-extra obj)
              sys$tag.struct-mutator-procedure)))
  
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
                           init-field-k
                           auto-field-k
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
                  
                  (let ((predicate (struct-predicate st))
                        (constructor (struct-constructor st)))
                    (let ((accessor
                           (struct-accessor st predicate))
                          (mutator
                           (struct-mutator st predicate)))
                
                      (values st
                              constructor
                              predicate
                              accessor
                              mutator)))))))))))

  (define make-struct-type-property*
    (case-lambda
      ((name) (make-struct-type-property* name #f))
      ((name guard-proc)
       (let ((prop:p (make-record-type (symbol->string name)
                                       '()
                                       *stype-prop-type*)))
         (define (p? x)
           (cond ((struct-instance? x)
                  (p? (record-type-descriptor x)))
                 ((struct-type? x)
                  (let ((prop-vals (stype-prop-values x)))
                    (if (assq prop:p prop-vals)
                        #t
                        #f)))
                 (else #f)))

         (define (p-ref x)
           (cond ((struct-instance? x)
                  (p-ref (record-type-descriptor x)))
                 ((struct-type? x)
                  (let ((prop-vals (stype-prop-values x)))
                    (cond ((assq prop:p prop-vals) => cdr)
                          (else (p-ref 0))))) ;; trigger the error case
                 (else
                  (error "make-struct-type-property: exn:application:type"))))
           
         (values prop:p p? p-ref)
         ))))
  
  (define (make-struct-field-accessor* ref-proc field-index . rest)
    (let ((name (if (pair? rest)
                    (car rest)
                    (string-append "field"
                                   (number->string field-index)))))
      (if (struct-accessor-procedure? ref-proc)
          (make-struct-proc (lambda (obj) (ref-proc obj field-index)))
          (raise-type-error 'make-struct-field-accessor
                            "accessor procedure that requires a field index"
                            ref-proc))))
  
  (define (make-struct-field-mutator* mutator-proc field-index . rest)
    (let ((name (if (pair? rest)
                    (car rest)
                    (string-append "field"
                                   (number->string field-index)))))
      (if (struct-mutator-procedure? mutator-proc)
          (make-struct-proc (lambda (instance new-value)
                              (mutator-proc instance
                                            field-index
                                            new-value)))
          (raise-type-error 'make-struct-field-mutator
                            "mutator procedure"
                            mutator-proc))))
  
  (define make-wrapped-waitable* (undefined))
  (define make-nack-guard-waitable* (undefined))
  (define make-poll-guard-waitable* (undefined))

  ;; FIXME:  This isn't right.  struct? only yields true when
  ;; struct->vector would produce a vector with some field values exposed.
  ;; Weird.  See MzScheme manual 4.8.
  (define (struct?* obj)
    (or (struct-instance? obj)
        (struct-proc? obj)))

  ;; this is internal
  (define struct-instance?
    (lambda (obj) (and (record? obj)
                  (struct-type? (record-type-descriptor obj)))))
  
  (define struct-type?*
    (lambda (t) (stype? t)))
  
  (define struct-type-property?* stype-prop?)
  
  (define struct-info* (undefined))
  (define struct-type-info* (undefined))
  (define struct->vector* (undefined))
  
 
  ;; Random utilities that don't belong above.
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

  (define (false? v)
    (eq? v #f))

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
  
;; Quick test cases from the MzScheme manual

(define-values (struct:tup make-tup tup? tup-ref tup-set!)
  (make-struct-type 'tup #f 2 0))

(define-values (struct:triple make-triple triple? triple-ref triple-set!)
  (make-struct-type 'triple struct:tup 1 0))

(define-values (prop:p p? p-ref) (make-struct-type-property 'p))

(define-values (struct:a make-a a? a-ref a-set!) 
  (make-struct-type 'a #f 2 0 'uninitialized (list (cons prop:p 8))))

;;; Test struct-type properties
(p? struct:a) ; => #t 
(p? 13) ; => #f 
(define an-a (make-a 'x 'y)) 
(p? an-a) ; => #t
(p-ref an-a) ; => 8

(define-values (struct:b make-b b? b-ref b-set!) 
  (make-struct-type 'b #f 0 0 #f)) 
(p? struct:b) ; => #f


(define-values (struct:fish make-fish fish? fish-ref fish-set!) 
  (make-struct-type 'fish #f 2 0 #f '() #f 
                    (lambda (f n) (fish-set! f 0 (+ n (fish-ref f 0))))))
(define (fish-weight f) (fish-ref f 0))
(define (fish-color f) (fish-ref f 1))
(define wanda (make-fish 12 'red))
(fish? wanda) ; => #t
(procedure? wanda) ; => #t
(fish-weight wanda) ; => 12
(for-each wanda '(1 2 3))
(fish-weight wanda) ; => 18

(define-values (struct:ap make-annotated-proc annotated-proc? ap-ref ap-set!) 
  (make-struct-type 'anotated-proc #f 2 0 #f '() #f 0)) 
(define (proc-annotation p) (ap-ref p 1))
(define plus1 (make-annotated-proc
                (lambda (x) (+ x 1))
                "adds 1 to its argument"))
(procedure? plus1) ; => #t
(annotated-proc? plus1) ; => #t
(plus1 10) ; => 11
(proc-annotation plus1) ; => "adds 1 to its argument"