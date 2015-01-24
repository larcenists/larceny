;;; From R7RS 7.3

(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values
         (lambda () expr)
         (lambda args #f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr) list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v))
       ...
       (define varn
         (let ((v (cadr var0))) (set! var0 (car var0)) v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr) list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v))
       ...
       (define varn
         (let ((v (cdr var0))) (set! var0 (car var0)) v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr) list)))))

;;; From src/Compiler/usual.sch

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((p1 e1) ...) b1 b2 ...)
     (letrec-syntax 
         ((parameterize-aux
           (... (syntax-rules ()
                  ((parameterize-aux (t ...) ((p0 e0) x ...) body1 body2 ...)
                   (let ((tempE e0)
                         (tempP p0))
                     (parameterize-aux ((tempE tempP) t ...) 
                                       (x ...) 
                                       body1 body2 ...)))
                  ((parameterize-aux ((tE tP) ...) () body1 body2 ...)
                   (let-syntax ((swap!
                                 (syntax-rules ()
                                   ((swap! var param)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (param tmp))))))
                     (dynamic-wind
                      (lambda ()
                        (swap! tE tP) ...)
                      (lambda ()
                        body1 body2 ...)
                      (lambda ()
                        (swap! tE tP) ...))))))))
       (parameterize-aux () ((p1 e1) ...) b1 b2 ...)))))

