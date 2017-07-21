;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (rnrs records syntactic original)
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
          (for (except (rnrs base)
                       let-syntax
                       letrec-syntax)
               run expand)
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


(library (rnrs records syntactic original)

  ; FIXME: should this library define and export auxiliary keywords?

#;
  (export define-record-type fields mutable immutable
          parent protocol sealed opaque nongenerative parent-rtd
          record-type-descriptor record-constructor-descriptor)

  (export define-record-type
          record-type-descriptor record-constructor-descriptor)

  (import (for (core primitives) run expand)
          (for (except (rnrs base)
                       let-syntax
                       letrec-syntax)
               run expand)
          (for (rnrs lists) run expand)
          (for (only (rnrs syntax-case) quasisyntax unsyntax) run expand)
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

          (define (frob x)
            (cond ((identifier? x)
                   x)
                  ((pair? x)
                   (cons (frob (car x)) (frob (cdr x))))
                  (else
                   (datum->syntax rtd-name x))))

          #`(#,(frob #'define-record-type-helper1)
             #,(frob rtd-name)
             #,(frob constructor-name)
             #,(frob predicate-name)
             #,(frob type-name)
             #,(frob field-specs)
             #,(frob protocol)
             #,(frob sealed?)
             #,(frob opaque?)
             #,(frob uid)
             #,(frob parent)
             #,(frob parent-cd))))

      ; Searches for a clause beginning with the given symbol,
      ; returning the entire clause (as a syntax object) if found
      ; or #f if no such clause is found.

      (define (clauses-assq sym clauses)
        (syntax-case clauses ()
         (((x1 x2 ...) y ...)
          (if (and (identifier? #'x1)
                   (eq? sym (syntax->datum #'x1)))
              #'(x1 x2 ...)
              (clauses-assq sym #'(y ...))))
         ((y0 y1 y2 ...)
          (clauses-assq sym #'(y1 y2 ...)))
         (x
          #f)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object for its first element.

      (define (syntax-car x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'x0)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object obtained by omitting the first
      ; element of that list.

      (define (syntax-cdr x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'(x1 ...))))

      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))

      (syntax-case x ()
       ((_ explicit? rtd-name constructor-name predicate-name clause ...)
        (let* ((type-name (syntax->datum #'rtd-name))

;              (ignored (begin (display "got to here okay") (newline)))

               (clauses #'(clause ...))
               (fields-clause (clauses-assq 'fields clauses))
               (parent-clause (clauses-assq 'parent clauses))
               (protocol-clause (clauses-assq 'protocol clauses))
               (sealed-clause (clauses-assq 'sealed clauses))
               (opaque-clause (clauses-assq 'opaque clauses))
               (nongenerative-clause (clauses-assq 'nongenerative clauses))
               (parent-rtd-clause (clauses-assq 'parent-rtd clauses))

               (okay?
                (let (
                      (clauses (syntax->datum clauses))
                      (fields-clause (syntax->datum fields-clause))
                      (parent-clause (syntax->datum parent-clause))
                      (protocol-clause (syntax->datum protocol-clause))
                      (sealed-clause (syntax->datum sealed-clause))
                      (opaque-clause (syntax->datum opaque-clause))
                      (nongenerative-clause
                       (syntax->datum nongenerative-clause))
                      (parent-rtd-clause (syntax->datum parent-rtd-clause))
                     )

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
                                                       (symbol?
                                                        (caddr fspec))))
                                                 ((4)
                                                  (and (eq? (car fspec)
                                                            'mutable)
                                                       (symbol? (caddr fspec))
                                                       (symbol?
                                                        (cadddr fspec))))
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
                                (= (length parent-rtd-clause) 3))))))

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

                       (let ((fspec (if (identifier? fspec)
                                        #`(immutable #,fspec)
                                        fspec)))
                         (cond ((= (length fspec) 2)
                                (let ((accessor-name
                                       (make-accessor-name
                                        (syntax->datum (cadr fspec)))))
                                  (case (syntax->datum (car fspec))
                                   ((immutable)
                                    #`(immutable
                                       #,(cadr fspec)
                                       #,accessor-name
                                       #f))
                                   ((mutable)
                                    #`(mutable
                                       #,(cadr fspec)
                                       #,accessor-name
                                       #,(make-mutator-name
                                          (syntax->datum (cadr fspec))))))))
                               ((= (length fspec) 3)
                                #`(#,(car fspec)
                                   #,(cadr fspec)
                                   #,(caddr fspec)
                                   #f))
                               (else fspec))))
                     (if fields-clause
                         (syntax-cdr fields-clause)
                         '()))))
          (if (not okay?)
              (complain))
          (construct-record-type-definitions
           #'rtd-name
           cname
           pname
           type-name
           field-specs

           (and protocol-clause (cadr (syntax->datum protocol-clause)))
           (and sealed-clause (cadr (syntax->datum sealed-clause)))
           (and opaque-clause (cadr (syntax->datum opaque-clause)))

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

  (define (record-type-descriptor rtd)
    rtd)

  (define (record-constructor-descriptor rtd)
    (preferred-cd rtd))

  ) ; rnrs records syntactic original
