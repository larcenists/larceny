; copyright 1998 lars t hansen.
;
; $id: macros.sch,v 1.2 1998/11/24 16:52:05 lth exp $
;
; a collection of useful and common macros.

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

; repeat e1 e2 ... until test

(define-syntax repeat
  (syntax-rules ()
    ((repeat test e1 e2 ...)
     (letrec ((loop
	       (lambda ()
		 e1 e2 ... (if (not test) (loop)))))
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

; (parameterize ((p1 e1) ...) b1 b2 ...)
; where each p1 is the name of a parameter (a procedure of 0 or 1 args).

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((p1 e1) ...) b1 b2 ...)
     (parameterize "temps" () ((p1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((p1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (parameterize "temps" ((temp e1 p1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e p) ...) () b1 b2 ...)
     (let-syntax ((swap!
		   (syntax-rules ()
		     ((swap! var param)
		      (let ((tmp var))
			(set! var (param))
			(param tmp))))))
       (dynamic-wind
	(lambda ()
	  (swap! t p) ...)
	(lambda ()
	  b1 b2 ...)
	(lambda ()
	  (swap! t p) ...))))))

(define-syntax bound?
  (syntax-rules ()
    ((bound? x)
     (environment-gettable? (interaction-environment) (quote x)))
    ((bound? x env)
     (environment-gettable? env (quote x)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((i1 ...) e0) e1 e2 ...)
     (call-with-values
      (lambda ()
	e0)
      (lambda (i1 ...) e1 e2 ...)))))

; this is really quite gross, but it seems to work.
;
; if the compiler can evaluate the length of a constant list, and 
; constant-folds the cases in the cond, then it's almost efficient!
; (currently, it does the former but not the latter.)
; the interpreter loses...

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda ((a1 ...) e1 ...) ...)
     (lambda args
       (let ((l (length args)))
	 (case-lambda "*bind*" args l ((a1 ...) e1 ...) ...))))
    ((case-lambda "*bind*" args l) 
     (error "case-lambda: NO CASES!"))
    ((case-lambda "*bind*" args l ((a1 ...) e1 ...) ((a2 ...) e2 ...) ...)
     (if (= l (length '(a1 ...)))
	 (cond ((= (length '(a1 ...)) 0)
		((lambda (a1 ...) e1 ...)))
	       ((= (length '(a1 ...)) 1)
		((lambda (a1 ...) e1 ...) (car args)))
	       ((= (length '(a1 ...)) 2)
		((lambda (a1 ...) e1 ...) (car args) (cadr args)))
	       ((= (length '(a1 ...)) 3)
		((lambda (a1 ...) e1 ...) (car args) (cadr args)
					  (caddr args)))
	       ((= (length '(a1 ...)) 4)
		((lambda (a1 ...) e1 ...) (car args) (cadr args)
					  (caddr args) (cadddr args)))
	   (else (apply (lambda (a1 ...) e1 ...) args)))
	 (case-lambda "*bind*" args l ((a2 ...) e2 ...) ...)))))

; eof
