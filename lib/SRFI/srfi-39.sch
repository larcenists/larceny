;;; SRFI-39: parameter objects
;;;
;;; In Larceny, make-parameter takes a name, a value, and a validity
;;; checker procedure.  The SRFI-39 signature is completely different.
;;;
;;; Larceny's parameterize macro is compatible with SRFI 39,
;;; so it doesn't have to be redefined here.

(define (make-parameter value . rest)
  (let ((converter (if (null? rest) (lambda (x) x) (car rest))))
    (set! value (converter value))
    (lambda args
      (cond ((null? args) 
	     value)
	    ((null? (cdr args))
	     (set! value (converter (car args))))
	    (else
	     (error "Too many arguments to parameter object"))))))
