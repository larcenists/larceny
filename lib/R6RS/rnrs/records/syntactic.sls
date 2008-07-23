;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (rnrs records syntactic)
;
; This implementation extends the R6RS define-record-type as
; outlined by William D Clinger in his essay on language design
; that accompanied his vote against ratification of the R6RS:
; http://www.r6rs.org/ratification/results.html#X101
;
; FIXME:  That means this implementation is R6RS-compatible but
; not R6RS-conforming.  An R6RS-conforming implementation would
; have to reject some perfectly sensible record type definitions
; that this implementation accepts.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs records syntactic helper)

  (export preferred-cd preferred-cd-set!)

  (import (for (core primitives) run expand)
          (for (primitives make-r6rs-hashtable hashtable-ref hashtable-set!
                           symbol-hash record-type-name)
               run expand)
          (for (rnrs base) run expand)
          (rnrs records procedural)
          (err5rs records procedural))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; Preferred record-constructor descriptors.  Whoopee.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define preferred-cd-table
    (make-r6rs-hashtable (lambda (rtd)
                           (symbol-hash (record-type-name rtd)))
                         eqv?))

  (define (preferred-cd rtd)
    (let ((cd (hashtable-ref preferred-cd-table rtd #f)))
      (if cd
          cd
          (make-record-constructor-descriptor rtd #f #f))))

  (define (preferred-cd-set! rtd cd)
    (hashtable-set! preferred-cd-table rtd cd))
)


(library (rnrs records syntactic (6))

  ; FIXME: should this library define and export auxiliary keywords?

#;
  (export define-record-type fields mutable immutable
          parent protocol sealed opaque nongenerative parent-rtd
          record-type-descriptor record-constructor-descriptor)

  (export define-record-type
          record-type-descriptor record-constructor-descriptor)

  (import (for (core primitives) run expand)
          (for (rnrs base) run expand)
          (for (rnrs lists) run expand)
          (rnrs records procedural)
          (err5rs records procedural)
          (rnrs records syntactic helper)
          (for (primitives gensym) run expand))

  (define-syntax define-record-type
    (syntax-rules ()
     ((_ (rtd-name constructor-name predicate-name) clause ...)
      (define-record-type-helper0
       #t rtd-name constructor-name predicate-name clause ...))
     ((_ rtd-name clause ...)
      (define-record-type-helper0
       #f rtd-name #f #f clause ...))))

  (define-syntax define-record-type-helper0
    (lambda (x)

      (define (spanky-mode?) #f) ; FIXME

      (define (construct-record-type-definitions
               rtd-name constructor-name predicate-name      ; syntax objects
               type-name field-specs
               protocol sealed? opaque? uid
               parent parent-cd)
        (let ()
          (datum->syntax
           rtd-name
           `(,#'define-record-type-helper1
             ,rtd-name ,constructor-name ,predicate-name
             ,type-name ,field-specs
             ,protocol ,sealed? ,opaque? ,uid
             ,parent ,parent-cd))))

      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))

      (syntax-case x ()
       ((_ explicit? rtd-name constructor-name predicate-name clause ...)
        (let* ((type-name (syntax->datum #'rtd-name))
               (clauses (syntax->datum #'(clause ...)))
               (fields-clause (assq 'fields clauses))
               (parent-clause (assq 'parent clauses))
               (protocol-clause (assq 'protocol clauses))
               (sealed-clause (assq 'sealed clauses))
               (opaque-clause (assq 'opaque clauses))
               (nongenerative-clause (assq 'nongenerative clauses))
               (parent-rtd-clause (assq 'parent-rtd-clause clauses))
               (okay?
                (and (symbol? type-name)
                     (if (syntax->datum #'explicit?)
                         (and (symbol? (syntax->datum #'constructor-name))
                              (symbol? (syntax->datum #'predicate-name)))
                         #t)
                     (or (not fields-clause)
                         (and
                          (list? fields-clause)
                          (for-all (lambda (fspec)
                                     (or (symbol? fspec)
                                         (and (list? fspec)
                                              (>= (length fspec) 2)
                                              (memq (car fspec)
                                                    '(immutable mutable))
                                              (symbol? (cadr fspec))
                                              (case (length fspec)
                                               ((2) #t)
                                               ((3)
                                                (and (eq? (car fspec)
                                                          'immutable)
                                                     (symbol? (caddr fspec))))
                                               ((4)
                                                (and (eq? (car fspec) 'mutable)
                                                     (symbol? (caddr fspec))
                                                     (symbol? (cadddr fspec))))
                                               (else #f)))))
                                   (cdr fields-clause))))
                     (or (not parent-clause)
                         (and (list? parent-clause)
                              (= (length parent-clause) 2)
                              (if (spanky-mode?)
                                  (symbol? (cadr parent-clause))
                                  #t)))
                     (or (not protocol-clause)
                         (and (list? protocol-clause)
                              (= (length protocol-clause) 2)))
                     (or (not sealed-clause)
                         (and (list? sealed-clause)
                              (= (length sealed-clause) 2)
                              (boolean? (cadr sealed-clause))))
                     (or (not opaque-clause)
                         (and (list? opaque-clause)
                              (= (length opaque-clause) 2)
                              (boolean? (cadr opaque-clause))))
                     (or (not nongenerative-clause)
                         (and (list? nongenerative-clause)
                              (or (null? (cdr nongenerative-clause))
                                  (symbol? (cadr nongenerative-clause)))))
                     (or (not parent-rtd-clause)
                         (and (list? parent-rtd-clause)
                              (= (length parent-rtd-clause) 3)))))
               (type-name-string (symbol->string type-name))
               (cname
                (if (symbol? (syntax->datum #'constructor-name))
                    #'constructor-name
                    (datum->syntax
                     #'rtd-name
                     (string->symbol
                      (string-append "make-" type-name-string)))))
               (pname
                (if (symbol? (syntax->datum #'predicate-name))
                    #'predicate-name
                    (datum->syntax
                     #'rtd-name
                     (string->symbol
                      (string-append type-name-string "?")))))
               (make-accessor-name
                (lambda (field-name)
                  (string->symbol
                   (string-append type-name-string
                                  "-"
                                  (symbol->string field-name)))))
               (make-mutator-name
                (lambda (field-name)
                  (string->symbol
                   (string-append type-name-string
                                  "-"
                                  (symbol->string field-name)
                                  "-set!"))))
               (field-specs
                (map (lambda (fspec)
                       (let ((fspec (if (symbol? fspec)
                                        (list 'immutable fspec)
                                        fspec)))
                         (cond ((= (length fspec) 2)
                                (let ((accessor-name
                                       (make-accessor-name (cadr fspec))))
                                  (case (car fspec)
                                   ((immutable)
                                    (list 'immutable
                                          (cadr fspec)
                                          accessor-name
                                          #f))
                                   ((mutable)
                                    (list 'mutable
                                          (cadr fspec)
                                          accessor-name
                                          (make-mutator-name (cadr fspec)))))))
                               ((= (length fspec) 3)
                                (list (car fspec)
                                      (cadr fspec)
                                      (caddr fspec)
                                      #f))
                               (else fspec))))
                     (if fields-clause
                         (cdr fields-clause)
                         '()))))
          (if (not okay?)
              (complain))
          (construct-record-type-definitions
           #'rtd-name
           cname
           pname
           type-name
           field-specs
           (and protocol-clause (cadr protocol-clause))
           (and sealed-clause (cadr sealed-clause))
           (and opaque-clause (cadr opaque-clause))
           (cond ((eq? nongenerative-clause #f)
                  #f)
                 ((null? (cdr nongenerative-clause))
                  (gensym "uid"))
                 (else
                  (cadr nongenerative-clause)))
           (cond (parent-clause (cadr parent-clause))
                 (parent-rtd-clause (cadr parent-rtd-clause))
                 (else #f))
           (and parent-rtd-clause (caddr parent-rtd-clause))))))))

  (define-syntax define-record-type-helper1
    (syntax-rules ()
     ((_ rtd-name constructor-name predicate-name
         type-name ((mutable? field-name accessor mutator) ...)
         protocol sealed? opaque? uid
         parent parent-cd)
      (begin (def-rtd-name rtd-name type-name ((mutable? field-name) ...)
                           parent sealed? opaque? uid)
             (def-cd rtd-name type-name parent parent-cd protocol)
             (def-constructor rtd-name constructor-name)
             (def-predicate rtd-name predicate-name)
             (def-accessor rtd-name field-name accessor)
             ...
             (def-mutator rtd-name field-name mutator)
             ...))))

  ; FIXME: implements the sealed, opaque, and uid features
  ; using Larceny's extensions to make-rtd.

  (define-syntax def-rtd-name
    (syntax-rules ()

     ((_ rtd-name type-name (fieldspec ...) parent #f #f #f)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent)))

     ((_ rtd-name type-name (fieldspec ...) parent sealed? #f #f)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'sealed)))

     ((_ rtd-name type-name (fieldspec ...) parent #f opaque? #f)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'opaque)))

     ((_ rtd-name type-name (fieldspec ...) parent sealed? opaque? #f)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'sealed 'opaque)))

     ((_ rtd-name type-name (fieldspec ...) parent #f #f id)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'uid 'id)))

     ((_ rtd-name type-name (fieldspec ...) parent sealed? #f id)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'sealed 'uid 'id)))

     ((_ rtd-name type-name (fieldspec ...) parent #f opaque? id)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent 'opaque 'uid 'id)))

     ((_ rtd-name type-name (fieldspec ...) parent sealed? opaque? id)
      (define rtd-name
        (make-rtd 'type-name '#(fieldspec ...) parent
                  'sealed 'opaque 'uid 'id)))))

  (define-syntax def-cd
    (syntax-rules ()

     ((_ rtd-name type-name #f #f #f)
      (define ignored
        (preferred-cd-set!
         rtd-name
         (make-record-constructor-descriptor rtd-name #f #f))))

     ((_ rtd-name type-name #f #f protocol)
      (define ignored
        (preferred-cd-set!
         rtd-name
         (make-record-constructor-descriptor rtd-name #f protocol))))

     ; FIXME: the R6RS says the parent mustn't have a protocol clause
     ; in this case, but the following doesn't bother to check.

     ((_ rtd-name type-name parent #f #f)
      (define ignored
        (preferred-cd-set!
         rtd-name
         (make-record-constructor-descriptor rtd-name #f #f))))

     ((_ rtd-name type-name parent #f protocol)
      (define ignored
        (preferred-cd-set!
         rtd-name
         (make-record-constructor-descriptor
          rtd-name (preferred-cd parent) protocol))))

     ((_ rtd-name type-name parent parent-cd protocol)
      (define ignored
        (preferred-cd-set!
         rtd-name
         (make-record-constructor-descriptor rtd-name parent-cd protocol))))))

  (define-syntax def-constructor
    (syntax-rules ()

     ((_ rtd-name #f)
      (begin))

     ((_ rtd-name constructor-name)
      (define constructor-name (record-constructor (preferred-cd rtd-name))))))

  (define-syntax def-predicate
    (syntax-rules ()

     ((_ rtd-name #f)
      (begin))

     ((_ rtd-name predicate-name)
      (define predicate-name (rtd-predicate rtd-name)))))

  (define-syntax def-accessor
    (syntax-rules ()

     ((_ rtd-name field-name #f)
      (begin))

     ((_ rtd-name field-name accessor)
      (define accessor (rtd-accessor rtd-name 'field-name)))))

  (define-syntax def-mutator
    (syntax-rules ()

     ((_ rtd-name field-name #f)
      (begin))

     ((_ rtd-name field-name mutator)
      (define mutator (rtd-mutator rtd-name 'field-name)))))

  ; This is ridiculous.

  (define-syntax record-type-descriptor
    (syntax-rules ()
     ((_ rtd)
      rtd)))

  (define-syntax record-constructor-descriptor
    (syntax-rules ()
     ((_ rtd)
      (preferred-cd rtd))))

  ) ; rnrs records syntactic


