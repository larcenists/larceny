(define (foreign-name->strings sym)
  (define (foo-bar-baz->foo_bar_baz lst)
    (apply append
           (map (lambda (c) 
                  (case c
                    ((#\-) (list #\_))
                    ((#\!) (list))
                    (else  (list c))))
                lst)))
  (define (foo-bar-baz->fooBarBaz lst)
    (call-with-current-continuation 
     (lambda (exit)
       (let loop ((lst lst))
         (cond 
          ((null? lst) '())
          (else
           (case (car lst)
             ((#\-) (if (null? (cdr lst))
                        (exit #f)
                        (cons (char-upcase (cadr lst))
                              (loop (cddr lst)))))
             ((#\!) (loop (cdr lst)))
             (else
              (cons (car lst) (loop (cdr lst)))))))))))
  
  (let* ((str (symbol->string sym))
         (lst (string->list str))  
         (lsts (map (lambda (f) (f lst)) (list foo-bar-baz->foo_bar_baz
                                               foo-bar-baz->fooBarBaz
                                               )))
         (lsts (let filter ((lsts lsts))
                 (cond ((null? lsts) '())
                       ((car lsts) (cons (car lsts) (filter (cdr lsts))))
                       (else (filter (cdr lsts))))))
         (strs (map list->string lsts)))
    strs))

(define (first-foreign-procedure all-names arg-desc result-desc)
  (let loop ((names all-names))
    (cond ((null? names)
           (error 'first-foreign-procedure 
                  " none of " all-names 
                  " is provided by the current set of ffi libraries."))
          ((foreign-procedure-provided? (car names))
           (foreign-procedure (car names) arg-desc result-desc))
          (else
           (loop (cdr names))))))

(define-syntax define-foreign
  (syntax-rules ()
    ((define-foreign (NAME ARG-TYPES ...) RESULT-TYPE)
     (define NAME
       (first-foreign-procedure (foreign-name->strings 'NAME) 
                                '(ARG-TYPES ...) 'RESULT-TYPE)))))
