;;; SRFI-39: parameter objects
;;;
;;; More or less compatible with Larceny's built-in parameter objects,
;;; in the absence of threads anyway: a parameter object is a
;;; procedure with local state.  PARAMETERIZE just manipulates the
;;; local state and uses DYNAMIC-WIND to ensure that it is restored on
;;; exit from the body.
;;;
;;; In Larceny, MAKE-PARAMETER takes a name, a value, and a validity
;;; checker procedure.  The SRFI-39 signature is completely different.

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

