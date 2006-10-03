; Defmacro compatible with that of SLIB (I think)
; 2000-01-06 / lth
;
; (defmacro name args body ...)
; e.g.
;   (defmacro when (test . body)
;     `(if ,test (begin ,@body)))

; Definition 1: I think this is right but there's a macro expander bug
; in Larceny that prevents it from working.

'(define-syntax defmacro
  (transformer 
   (lambda (exp rename compare)

     (define (arglist? x)
       (or (symbol? x)
           (null? x)
           (and (pair? x)
                (symbol? (car x))
                (arglist? (cdr x)))))

     (if (not (and (list? exp)
                   (>= (length exp) 4)
                   (symbol? (cadr exp))
                   (arglist? (caddr exp))))
         (error "Bad macro definition: " exp))
     (let ((name (cadr exp))
           (args (caddr exp))
           (body (cdddr exp)))
       `(,(rename 'define-syntax) ,name
          (,(rename 'transformer)
           (,(rename 'lambda) (exp rename compare)
              (,(rename 'apply)
                (,(rename 'lambda)
                 ,args
                 ,@body)
                exp))))))))

; Definition 2: This is wrong but works roughly right on the top level,
; which is all we need.

(define-syntax defmacro
  (transformer 
   (lambda (exp rename compare)

     (define (arglist? x)
       (or (symbol? x)
           (null? x)
           (and (pair? x)
                (symbol? (car x))
                (arglist? (cdr x)))))

     (if (not (and (list? exp)
                   (>= (length exp) 4)
                   (symbol? (cadr exp))
                   (arglist? (caddr exp))))
         (error "Bad macro definition: " exp))
     (let ((name (cadr exp))
           (args (caddr exp))
           (body (cdddr exp)))
       `(define-syntax ,name
          (transformer
           (lambda (_defmacro_exp _defmacro_rename _defmacro_compare)
              (apply (lambda ,args ,@body) (cdr _defmacro_exp)))))))))

; eof
