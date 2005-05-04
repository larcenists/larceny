; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny's interpreter: primitives.

; Invoking primitives that take 1 argument.  The primitive procedures
; could usefully be defined using a macro.

($$trace "interp-prim")

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

(define (interpret/invoke-prim1:car:pair a orig cell)
  (interpreted-primitive
   'car:pair 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (car:pair (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:.car a orig cell)
  (interpreted-primitive
   '.car 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (.car (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:cdr a orig cell)
  (interpreted-primitive
   'cdr 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (cdr (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:cdr:pair a orig cell)
  (interpreted-primitive
   'cdr:pair 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (cdr:pair (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:.cdr a orig cell)
  (interpreted-primitive
   '.cdr 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (.cdr (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:not a orig cell)
  (interpreted-primitive
   'not 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (not (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:null? a orig cell)
  (interpreted-primitive
   'null? 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (null? (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:pair? a orig cell)
  (interpreted-primitive
   'pair? 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (pair? (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:vector? a orig cell)
  (interpreted-primitive
   'vector? 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (vector? (a env))
           (v (a env)))))))

(define (interpret/invoke-prim1:vector-length a orig cell)
  (interpreted-primitive
   'vector-length 1
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (vector-length (a env))
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

(define (interpret/invoke-prim2:apply a b orig cell)
  (interpreted-primitive
   'apply 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (apply (a env) (b env))
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

(define (interpret/invoke-prim2:.cons a b orig cell)
  (interpreted-primitive
   '.cons 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (.cons (a env) (b env))
           (v (a env) (b env)))))))

(define (interpret/invoke-prim2:vector-ref a b orig cell)
  (interpreted-primitive
   'vector-ref 2
   (lambda (env)
     (let ((v (car cell)))
       (if (eq? v orig)
           (vector-ref (a env) (b env))
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
    (apply ,apply (2 . ,interpret/invoke-prim2:apply))
    (eq? ,eq? (2 . ,interpret/invoke-prim2:eq?))
    (eqv? ,eqv? (2 . ,interpret/invoke-prim2:eqv?))
    (car ,car (1 . ,interpret/invoke-prim1:car))
    (car:pair ,car:pair (1 . ,interpret/invoke-prim1:car:pair))
    (.car ,.car (1 . ,interpret/invoke-prim1:.car))
    (cdr ,cdr (1 . ,interpret/invoke-prim1:cdr))
    (cdr:pair ,cdr:pair (1 . ,interpret/invoke-prim1:cdr:pair))
    (.cdr ,.cdr (1 . ,interpret/invoke-prim1:.cdr))
    (cons ,cons (2 . ,interpret/invoke-prim2:cons))
    (.cons ,.cons (2 . ,interpret/invoke-prim2:.cons))
    (not ,not (1 . ,interpret/invoke-prim1:not))
    (null? ,null? (1 . ,interpret/invoke-prim1:null?))
    (pair? ,pair? (1 . ,interpret/invoke-prim1:pair?))
    (vector? ,vector? (1 . ,interpret/invoke-prim1:vector?))
    (vector-length ,vector-length (1 . ,interpret/invoke-prim1:vector-length))
    (vector-ref ,vector-ref (2 . ,interpret/invoke-prim2:vector-ref))
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
