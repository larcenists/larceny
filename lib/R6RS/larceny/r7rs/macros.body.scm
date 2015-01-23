;;; From src/Compiler/usual.sch

(define-syntax define-values
  (syntax-rules ()
    ((define-values (<name> ...) <body> ...)
     ; =>
     (define-values helper (<name> ...) () (<name> ...) <body> ...))
    ((define-values helper () (<temp> ...) (<name> ...) <body> ...)
     ; =>
     (begin
       (define <name> #f) 
       ...
       (define <ignored>
         (call-with-values
          (lambda () <body> ...)
          (lambda (<temp> ...)
            (set! <name> <temp> ) 
            ...
            )))
       ))
    ((define-values helper (<var1> <var2> ...) <temp>s
       (<name> ...) <body> ...)
     ; =>
     (define-values helper (<var2> ...) (<temp> . <temp>s)
       (<name> ...) <body> ...))))

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

