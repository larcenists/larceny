; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; 15 April 1999 / lth.
;
; Hygienic implementation of define-record syntax.
;
;    (define-record name (f1 ...))
; => (begin (define name (make-record-type 'name '(f1 ...)))
;           (define make-name (record-constructor name))
;           (define name? (record-predicate name))
;           (define name-f1 (record-accessor name 'f1)) ...
;           (define name-f1-set! (record-updater name 'f1)) ...
;    )
;
; Requires larceny's macro system (explicit renaming).
; Requires the record package (record.sch).
;
; The define-record syntax can be used at the top level only.  The 
; problem is that the define nest that results from macro expansion is
; not suitable for internal definitions: the first definition's value
; is used by subsequent definitions.  We can fix this by moving support
; for define-record into the macro expander, by introducing a sequencing
; form like MzScheme's DEFINITIONS, or by using a LET-RECORD form 
; for internal record definitions.
;
; FIXME:
;  - allow use of the inheritance feature
;  - allow use of the record printer feature
;  - define let-record or find another solution for internal records

(define-syntax define-record
  (transformer
   (lambda (exp rename compare)
     (if (not (and (list? exp)
                   (= (length exp) 3)
                   (symbol? (cadr exp))
                   (list? (caddr exp))
                   (every? symbol? (caddr exp))))
         (error "Invalid define-record form " exp)
         (let* ((name (symbol->string (cadr exp)))
                (rtd-name (string->symbol (string-append name)))
                (fields (caddr exp)))
           `(,(rename 'begin)
             ,(list (rename 'define)
                    rtd-name
                    `(,(rename 'make-record-type) ,name ',fields))
             ,(list (rename 'define) 
                    (string->symbol (string-append "make-" name))
                    `(,(rename 'record-constructor) ,rtd-name))
             ,(list (rename 'define)
                    (string->symbol (string-append name "?"))
                    `(,(rename 'record-predicate) ,rtd-name))
             ,@(map (lambda (fn)
                      (list (rename 'define)
                            (string->symbol
                             (string-append name "-" (symbol->string fn)))
                            `(,(rename 'record-accessor) ,rtd-name ',fn)))
                    fields)
             ,@(map (lambda (fn)
                      (list (rename 'define)
                            (string->symbol
                             (string-append name "-" (symbol->string fn)
                                            "-set!"))
                            `(,(rename 'record-updater) ,rtd-name ',fn)))
                    fields)))))))
; eof
