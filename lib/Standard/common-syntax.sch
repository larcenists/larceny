; Copyright 1998 Lars T Hansen
;
; $Id$
;
; A collection of useful and common macros.  

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

; (include <filename>)

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

; evaluates expressions in order, returning the value of the first

(define-syntax begin0
  (syntax-rules ()
    ((begin0 e1 e2 ...)
     (let ((v e1)) e2 ... v))))

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

; The following macros are already part of the standard heaps,
; but are duplicated here so they can be restored to a heap
; from which they have been eliminated.

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

; eof
