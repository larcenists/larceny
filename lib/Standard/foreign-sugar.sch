(define (foreign-name->string sym)
  (let* ((str (symbol->string sym))
         (lst (string->list str))
         (lst (apply append
                     (map (lambda (c) 
                            (case c
                              ((#\-) (list #\_))
                              ((#\!) (list))
                              (else  (list c))))
                          lst)))
         (str (list->string lst)))
    str))
    
(define-syntax define-foreign
  (syntax-rules ()
    ((define-foreign (NAME ARG-TYPES ...) RESULT-TYPE)
     (define NAME
       (foreign-procedure (foreign-name->string 'NAME) '(ARG-TYPES ...) 'RESULT-TYPE)))))
