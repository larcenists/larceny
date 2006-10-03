; Copyright 1998 Lars T Hansen
;
; $Id$
;
; A collection of useful and common macros.  

(define-syntax when
  (syntax-rules ()
    ((when test e1 e2 ...)
     (if test
	 (begin e1 e2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test e1 e2 ...)
     (if (not test)
	 (begin e1 e2 ...)))))

; while test do e1 e2 ... end

(define-syntax while
  (syntax-rules ()
    ((while test e1 e2 ...)
     (letrec ((loop
	       (lambda ()
		 (if test
		     (begin e1 e2 ... (loop))))))
       (loop)))))

; (fluid-let ((v1 e1) ...) b1 b2 ...)
; sets the variables v1 ... to the values of e1 ... in the dynamic scope
; of b1 b2 ... .  usually, v1 ... are global, in which case they must
; already have a value.

(define-syntax fluid-let
  (syntax-rules ()
    ((_ ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e v) ...) () b1 b2 ...)
     (let-syntax ((swap!
		   (syntax-rules ()
		     ((swap! a b)
		      (let ((tmp a))
			(set! a b)
			(set! b tmp))))))
       (dynamic-wind
	(lambda ()
	  (swap! t v) ...)
	(lambda ()
	  b1 b2 ...)
	(lambda ()
	  (swap! t v) ...))))))

(define-syntax bound?
  (syntax-rules ()
    ((bound? x)
     (bound? x (interaction-environment)))
    ((bound? ?x ?env)
     (let ((env ?env)
           (name (quote ?x)))
       (or (environment-variable? env name)
           (environment-macro? env name))))))

(define-syntax time
  (syntax-rules ()
    ((time ?expr)
     (run-with-stats (lambda () ?expr)))))

(define-syntax include
  (transformer
   (lambda (exp rename compare)
     (if (and (list? exp)
              (= (length exp) 2))
         `(begin ,@(let ((fn (eval (cadr exp))))
                     (call-with-input-file fn
                       (lambda (in)
                         (do ((l (read in) (read in))
                              (es '() (cons l es)))
                             ((eof-object? l)
                              (reverse es)))))))
         (error "Bad INCLUDE: " exp)))))

(define-syntax begin0
  (syntax-rules ()
    ((begin0 e1 e2 ...)
     (let ((v e1)) e2 ... v))))

; This is really quite gross, but it seems to work.
;
; If the compiler can evaluate the length of a constant list, and 
; constant-folds the cases in the cond, then it's almost efficient.
; (Currently, it does the former but not the latter.)
;
; The interpreter loses...

; Needlessly messy because (?a1 ... . ?an) is not a legal pattern.

; Optimized for Larceny.

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let* ((l 0)
              (v1 (if (pair? args) (car args) #f))
              (t1 (if (pair? args) (cdr args) #f))
              (l  (if (pair? args) (+ l 1) l))
              (v2 (if (pair? t1) (car t1) #f))
              (t2 (if (pair? t1) (cdr t1) #f))
              (l  (if (pair? t1) (+ l 1) l))
              (v3 (if (pair? t2) (car t2) #f))
              (t3 (if (pair? t2) (cdr t2) #f))
              (l  (if (pair? t2)  (+ l 1) l))
              (l  (if (pair? t3) (+ l (length t3)) l)))
	 (case-lambda "CLAUSE" args l (v1 v2 v3 t1 t2 t3)
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ; The following alternative to the previous syntax-rule does not have as
    ; good performance, probably in part due to a bug in Twobit where
    ; it does not ignore some IGNORED parameters; see mail to larceny@ccs
    ; on 990906.
;    ((case-lambda 
;      (?a1 ?e1 ...) 
;      ?clause1 ...)
;     (lambda args
;       (define (f l v1 v2 v3 t1 t2 t3)
;	 (case-lambda "CLAUSE" args l (v1 v2 v3 t1 t2 t3)
;           (?a1 ?e1 ...)
;           ?clause1 ...))
;       (if (pair? args)
;           (let ((v1 (car args))
;                 (t1 (cdr args)))
;             (if (pair? t1)
;                 (let ((v2 (car t1))
;                       (t2 (cdr t1)))
;                   (if (pair? t2)
;                       (let ((v3 (car t2))
;                             (t3 (cdr t2)))
;                         (if (pair? t3)
;                             (f (+ 3 (length t3)) v1 v2 v3 t1 t2 t3)
;                             (f 3 v1 v2 v3 t1 t2 t3)))
;                       (f 2 v1 v2 #f t1 t2 #f)))
;                 (f 1 v1 #f #f t1 #f #f)))
;           (f 0 #f #f #f #f #f #f))))
    ((case-lambda "CLAUSE" ?args ?l ?xs
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (eq? ?l (length '(?a1 ...)))
         (case-lambda "APPLY" ?xs ((?a1 ...) ?e1 ...) ?args) 
         (case-lambda "CLAUSE" ?args ?l ?xs
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l ?xs
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 ?xs (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l ?xs
      (?a1 ?e1 ...)
      ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l ?xs)
     (error "Wrong number of arguments to CASE-LAMBDA."))
    ((case-lambda "IMPROPER" ?args ?l ?k ?xs ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?xs ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?xs ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (case-lambda "APPLY-IMPROPER" ?xs ?al (?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l ?xs
           ?clause1 ...)))
    ((case-lambda "APPLY" ?xs (() ?e1 ...) ?args)
     (begin ?e1 ...))
    ((case-lambda "APPLY" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3) ((?a1) ?e1 ...) ?args)
     (let ((?a1 ?v1))
       ?e1 ...))
    ((case-lambda "APPLY" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3) ((?a1 ?a2) ?e1 ...) ?args)
     (let ((?a1 ?v1)
           (?a2 ?v2))
       ?e1 ...))
    ((case-lambda "APPLY" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3) ((?a1 ?a2 ?a3) ?e1 ...) ?args)
     (let ((?a1 ?v1)
           (?a2 ?v2)
           (?a3 ?v3))
       ?e1 ...))
    ((case-lambda "APPLY" ?xs ((?a1 ...) ?e1 ...) ?args)
     (apply (lambda (?a1 ...) ?e1 ...) ?args))
    ((case-lambda "APPLY-IMPROPER" ?xs 
                  (?a1 ?a2 ?a3 ?a4 . ?ar) (?e1 ...) ?args)
     (apply (lambda (?a1 ?a2 ?a3 ?a4 . ?ar) ?e1 ...) ?args))
    ((case-lambda "APPLY-IMPROPER" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3) 
                  (?a1 ?a2 ?a3 . ?ar) (?e1 ...) ?args)
     (let ((?a1 ?v1)
           (?a2 ?v2)
           (?a3 ?v3)
           (?ar ?t3))
       ?e1 ...))
    ((case-lambda "APPLY-IMPROPER" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3)
                  (?a1 ?a2 . ?ar) (?e1 ...) ?args)
     (let ((?a1 ?v1)
           (?a2 ?v2)
           (?ar ?t2))
       ?e1 ...))
    ((case-lambda "APPLY-IMPROPER" (?v1 ?v2 ?v3 ?t1 ?t2 ?t3)
                  (?a1 . ?ar) (?e1 ...) ?args)
     (let ((?a1 ?v1)
           (?ar ?t1))
       ?e1 ...))))

; eof
