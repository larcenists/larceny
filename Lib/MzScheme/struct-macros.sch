(export-syntax
 '(define-syntax define-struct
    (transformer
     (lambda (stx rename compare)
       (define (every p items)
         (if (null? items)
             #t
             (and (p (car items)) (every p (cdr items)))))
       (define (stx-error expected)
         (error "define-struct: bad-syntax (" expected ")"))
       (define args (cdr stx))
       (if (not (member (length args) '(2 3)))
           (stx-error "wrong number of arguments"))
       (let ((arg1 (car args))
             (arg2 (cadr args))
             (arg-rest (cddr args)))
         (define (get-name)
           (cond ((symbol? arg1) arg1)
                 ((and (pair? arg1) (symbol? (car arg1)))
                  (car arg1))
                 (else
                  (stx-error 'name))))
         (define (get-super)
           (cond ((symbol? arg1) #f)
                 ((and (pair? arg1) (pair? (cdr arg1)) (symbol? (cadr arg1)))
                  (cadr arg1))
                 (else
                  (stx-error 'super))))
         (define (get-fields)
           (cond ((or (not (list? arg2)) (not (every symbol? arg2)))
                  (stx-error 'fields))
                 (else arg2)))
         (define (get-inspector)
           (cond ((null? arg-rest) #f)
                 (else (car arg-rest))))
         (define (join . ids)
           (string->symbol (apply string-append (map symbol->string ids))))
         (let ((name (get-name))
               (super-exp (let ((s (get-super)))
                            (and s `(,s #t))))
               (fields (get-fields))
               (insp (get-inspector)))
           (let ((sinfo (join 'struct: name))
                 (mk (join 'make- name))
                 (s? (join name '?))
                 (accs (map (lambda (f) (join name '- f)) fields))
                 (muts (map (lambda (f) (join 'set- name '- f '!)) fields)))
             `(begin
                (define-values (,sinfo ,mk ,s? ,@accs ,@muts)
                  (let-values (((sinfo mk s? ref set)
                                (make-struct-type ',name ,super-exp ,(length fields) 0
                                                  #f '() ,insp)))
                    (values
                     sinfo
                     mk
                     s?
                     ,@(let loop ((i 0) (fields fields))
                         (if (null? fields)
                             '()
                             (cons `(make-struct-field-accessor ref ,i ',(car fields))
                                   (loop (+ 1 i) (cdr fields)))))
                     ,@(let loop ((i 0) (fields fields))
                         (if (null? fields)
                             '()
                             (cons `(make-struct-field-mutator set ,i ',(car fields))
                                   (loop (+ 1 i) (cdr fields))))))))
                (define-syntax ,name
                  (syntax-rules ()
                    ((_ #t) ,sinfo)
                    ((_ #f)
                     (list ,sinfo
                           ,mk
                           ,s?
                           (list ,@(reverse accs))
                           (list ,@(reverse muts))))))))))))))
