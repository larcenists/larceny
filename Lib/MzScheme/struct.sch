;; This code depends on Auxlib/record.sch and MzScheme/inspector.sch

;; http://download.plt-scheme.org/doc/207/html/mzscheme/mzscheme-Z-H-4.html#node_chap_4

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

(let ()

  ;; drop the first n elements of list
  (define (drop n lst)
    (if (zero? n)
        lst
        (drop (- n 1) (cdr lst))))
  
  ;; A structure type is a record consisting of
  ;; - a record type descriptor
  ;; - a list of (struct-type-property-descriptor . any)
  ;; - an inspector
  ;; - proc-spec
  ;; - immutable-k-list
  (define make-struct-type*
    (let ((nats-to  ;; generate a list from [0, n)
           (lambda (n) (let loop ((c 0)
                             (l '()))
                    (if (= c n)
                        l
                        (loop (+ 1 c)
                              (cons c l))))))
          (offset->name
           (lambda (n) (string->symbol
                   (string-append "field-" (number->string n)))))
          (defaults '(#f () (current-inspector) #f ())))
      (lambda (name super init-field-k auto-field-k . rest)
        ;; no opt-lambda, sorry...
        (let* ((opts (append rest
                             (drop (length rest) defaults)))
               (opts (list->vector opts)))
          
          (let ((auto-v (vector-ref opts 0))
                (prop-value-list (vector-ref opts 1))
                (inspector (vector-ref opts 2))
                (proc-spec (vector-ref opts 3))
                (immutable-k-list (vector-ref opts 4))

                (field-names
                 (map offset->name
                      (nats-to (+ init-field-k auto-field-k)))))
            (let ((type-descr (make-record-type name field-names super)))
              (let ((constructor (record-constructor type-descr))
                    (predicate (record-predicate type-descr))
                    (accessor (record-indexer type-descr))
                    (mutator (record-mutator type-descr)))
                (values type-descr
                        constructor
                        predicate
                        accessor
                        mutator))))))))

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
  
  (define struct?* record?)
  (define struct-type?*  record-type-descriptor?)
  (define struct-type-property?*  (undefined))
  
  (define struct-info*  (undefined))
  (define struct-type-info*  (undefined))
  (define struct->vector*  (undefined))
  
  (define struct-mutator-procedure?*  (undefined))
  (define struct-accessor-procedure?*  (undefined))
  (define struct-predicate-procedure?*  (undefined))
  (define struct-constructor-procedure?* (undefined))

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
  )
  

;; Larceny doesn't seem to have define-values
; (define tup)
; (define mk-tup)
; (define tup?)
; (define tup-ref)
; (define tup-set!)
; (let-values ((type cons pred ref set)
;              (make-struct-type 'tup #f 2 0))
;   (set! tup type)
;   (set! mk-tup cons)
;   (set! tup? pred)
;   (set! tup-ref ref)
;   (set! tup-set! set))