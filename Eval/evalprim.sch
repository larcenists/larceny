; Invoking primitives that take 1 argument.  The primitive procedures
; could usefully be defined using a macro.

($$trace "evalprim")

(define (eval/invoke-prim1:- a orig cell)
  (evaluator-primitive
   '- 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (- (a env))
	   (v (a env)))))))

(define (eval/invoke-prim1:car a orig cell)
  (evaluator-primitive
   'car 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (car (a env))
	   (v (a env)))))))

(define (eval/invoke-prim1:cdr a orig cell)
  (evaluator-primitive
   'cdr 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (cdr (a env))
	   (v (a env)))))))

; Invoking primitives that take 2 arguments.  The primitive procedures
; could usefully be generated using a macro.

(define (eval/invoke-prim2:+ a b orig cell)
  (evaluator-primitive
   '+ 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (+ (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:- a b orig cell)
  (evaluator-primitive
   '- 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (- (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:= a b orig cell)
  (evaluator-primitive
   '= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (= (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:< a b orig cell)
  (evaluator-primitive
   '< 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (< (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:> a b orig cell)
  (evaluator-primitive 
   '> 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (> (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:<= a b orig cell)
  (evaluator-primitive
   '<= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (<= (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:>= a b orig cell)
  (evaluator-primitive
   '>= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (>= (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:eq? a b orig cell)
  (evaluator-primitive
   'eq? 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (eq? (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:eqv? a b orig cell)
  (evaluator-primitive
   'eqv? 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (eqv? (a env) (b env))
	   (v (a env) (b env)))))))

(define (eval/invoke-prim2:cons a b orig cell)
  (evaluator-primitive
   'cons 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (cons (a env) (b env))
	   (v (a env) (b env)))))))

; Primitive tables and lookup functions.

(define eval/prim-table
  `((+  ,+  (2 . ,eval/invoke-prim2:+))
    (-  ,-  (1 . ,eval/invoke-prim1:-) (2 . ,eval/invoke-prim2:-))
    (=  ,=  (2 . ,eval/invoke-prim2:=))
    (<  ,<  (2 . ,eval/invoke-prim2:<))
    (>  ,>  (2 . ,eval/invoke-prim2:>))
    (<= ,<= (2 . ,eval/invoke-prim2:<=))
    (>= ,>= (2 . ,eval/invoke-prim2:>=))
    (eq? ,eq? (2 . ,eval/invoke-prim2:eq?))
    (eqv? ,eqv? (2 . ,eval/invoke-prim2:eqv?))
    (car ,car (1 . ,eval/invoke-prim1:car))
    (cdr ,cdr (1 . ,eval/invoke-prim1:cdr))
    (cons ,cons (2 . ,eval/invoke-prim2:cons))
    ))

(define (eval/primitive? name args)
  (let ((probe (assq name eval/prim-table)))
    (and probe
	 (assv args (cddr probe)))))

(define (eval/primitive name args)
  (cdr (assv args (cddr (assq name eval/prim-table)))))

(define (eval/prim-orig name)
  (cadr (assq name eval/prim-table)))

; eof
