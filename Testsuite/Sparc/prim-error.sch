; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Testsuite/Sparc/prim-error.sch -- test that all primitives signal the 
; correct errors.

; Note: must load the error code file first.

(define (prim-error-tests)
  (let ((old-error-handler (error-handler))
	(token (list 'token)))
    (dynamic-wind 
     (lambda () #t)
     (lambda ()
       (do ((l test-inputs (cdr l)))
	   ((null? l))
	 (let* ((op (caar l))
		(x  (assq op test-operations))
		(proc (cadr x))
		(ev   (caddr x)))
	   (let ((v (call-with-current-continuation
		     (lambda (return)
		       (error-handler (lambda (a1 a2 a3 code)
					(return
					 (list token a1 a2 a3 code))))
		       (apply proc (cdar l))))))
	     (if (and (pair? v) (eq? (car v) token))
		 (let ((error-values (apply ev (cadr l))))
		   (if (not (results-ok? (cdr v) error-values))
		       (display "Failed test -- wrong values!")))
		 (display "Failed test -- did not fail!"))))))
     (lambda ()
       (error-handler old-error-handler)))))

(define test-operations
  `((car ,car ,(lambda (arg)
		 (list arg (unspecified) (unspecified) $ex.car)))
    (cdr ,cdr ,(lambda (arg)
		 (list arg (unspecified) (unspecified) $ex.cdr)))
    (set-car! ,set-car!
	      ,(lambda (a1 a2)
		   (list a1 a2 (unspecified) $ex.setcar)))
    (set-cdr! ,set-cdr!
	      ,(lambda (a1 a2)
		   (list a1 a2 (unspecified) $ex.setcdr)))))
    ...))

(define test-inputs
  `((car #f)
    (cdr #f)
    (set-car! #f #f)
    (set-cdr! #f #f)
    ...))
