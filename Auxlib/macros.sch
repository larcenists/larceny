; Auxlib/macros.sch
; A collection of useful and common macros.
;
; $Id$

(define-syntax when
  (syntax-rules ()
    ((when test E1 E2 ...)
     (if test
	 (begin E1 E2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test E1 E2 ...)
     (if (not test)
	 (begin E1 E2 ...)))))

; while test do E1 E2 ... end

(define-syntax while
  (syntax-rules ()
    ((while test E1 E2 ...)
     (letrec ((loop
	       (lambda ()
		 (if test
		     (begin E1 E2 ... (loop))))))
       (loop)))))

; repeat E1 E2 ... until test

(define-syntax repeat
  (syntax-rules ()
    ((repeat test E1 E2 ...)
     (letrec ((loop
	       (lambda ()
		 E1 E2 ... (if (not test) (loop)))))
       (loop)))))

; (fluid-let ((v1 e1) ...) b1 b2 ...)
; Sets the variables v1 ... to the values of e1 ... in the dynamic scope
; of b1 b2 ... .  Usually, v1 ... are global, in which case they must
; already have a value.

(define-syntax fluid-let
  (syntax-rules ()
    ((fluid-let ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((fluid-let "temps" (t ...) ((v1 e1) (v2 e2) ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) ((v2 e2) ...) b1 b2 ...)))
    ((fluid-let "temps" ((t e v) ...) () b1 b2 ...)
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
     (environment-gettable? (interaction-environment) (quote x)))
    ((bound? x env)
     (environment-gettable? env (quote x)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((i1 ...) E0) E1 E2 ...)
     (call-with-values
      (lambda ()
	E0)
      (lambda (i1 ...) E1 E2 ...)))))

; This is really quite gross, but it seems to work.
; If the compiler can evaluate the length of a constant list, and 
; constant-folds the cases in the cond, then it's almost efficient!
; (Currently, it does the former but not the latter.)
; The interpreter loses...

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
