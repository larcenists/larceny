; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny's interpreter: primitives.

; Invoking primitives that take 1 argument.  The primitive procedures
; could usefully be defined using a macro.

($$trace "evalprim")

(define (interpret/invoke-prim1:- a orig cell)
  (interpreted-primitive
   '- 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (- (a env))
	   (v (a env)))))))

(define (interpret/invoke-prim1:car a orig cell)
  (interpreted-primitive
   'car 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (car (a env))
	   (v (a env)))))))

(define (interpret/invoke-prim1:cdr a orig cell)
  (interpreted-primitive
   'cdr 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (cdr (a env))
	   (v (a env)))))))

; Invoking primitives that take 2 arguments.  The primitive procedures
; could usefully be generated using a macro.

(define (interpret/invoke-prim2:+ a b orig cell)
  (interpreted-primitive
   '+ 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (+ (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:- a b orig cell)
  (interpreted-primitive
   '- 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (- (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:= a b orig cell)
  (interpreted-primitive
   '= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (= (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:< a b orig cell)
  (interpreted-primitive
   '< 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (< (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:> a b orig cell)
  (interpreted-primitive 
   '> 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (> (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:<= a b orig cell)
  (interpreted-primitive
   '<= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (<= (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:>= a b orig cell)
  (interpreted-primitive
   '>= 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (>= (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:eq? a b orig cell)
  (interpreted-primitive
   'eq? 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (eq? (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:eqv? a b orig cell)
  (interpreted-primitive
   'eqv? 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (eqv? (a env) (b env))
	   (v (a env) (b env)))))))

(define (interpret/invoke-prim2:cons a b orig cell)
  (interpreted-primitive
   'cons 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
	   (cons (a env) (b env))
	   (v (a env) (b env)))))))

; Primitive tables and lookup functions.

(define interpret/prim-table
  `((+  ,+  (2 . ,interpret/invoke-prim2:+))
    (-  ,-  (1 . ,interpret/invoke-prim1:-) (2 . ,interpret/invoke-prim2:-))
    (=  ,=  (2 . ,interpret/invoke-prim2:=))
    (<  ,<  (2 . ,interpret/invoke-prim2:<))
    (>  ,>  (2 . ,interpret/invoke-prim2:>))
    (<= ,<= (2 . ,interpret/invoke-prim2:<=))
    (>= ,>= (2 . ,interpret/invoke-prim2:>=))
    (eq? ,eq? (2 . ,interpret/invoke-prim2:eq?))
    (eqv? ,eqv? (2 . ,interpret/invoke-prim2:eqv?))
    (car ,car (1 . ,interpret/invoke-prim1:car))
    (cdr ,cdr (1 . ,interpret/invoke-prim1:cdr))
    (cons ,cons (2 . ,interpret/invoke-prim2:cons))
    ))

(define (interpret/primitive? name args)
  (let ((probe (assq name interpret/prim-table)))
    (and probe
	 (assv args (cddr probe)))))

(define (interpret/primitive name args)
  (cdr (assv args (cddr (assq name interpret/prim-table)))))

(define (interpret/prim-orig name)
  (cadr (assq name interpret/prim-table)))

; eof
