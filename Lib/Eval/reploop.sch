;; Read-eval-print loop based on simple evaluator.

(define (main)
  (display "Larceny top level; evaluator version ")
  (display eval-version)
  (newline)
  (newline)
  (init-toplevel-environment)
  (call-with-current-continuation
   (lambda (k)
     (set! error-continuation k)))
  (rep-loop))

(define (rep-loop)
  (display "]> ")
  (flush-output-port)
  (let ((expr   (read)))
    (if (not (eof-object? expr))
	(let ((result (eval expr)))
	  (display result)
	  (newline)
	  (rep-loop))
	(begin (newline)
	       (exit)))))

(define error-continuation #f)

(define (error . args)
  (set! in-error #t)
  (display "Error:")
  (for-each (lambda (x) (display " ") (display x)) args)
  (newline)
  (if error-continuation
      (error-continuation #f)
      (begin 
	(display "No error continuation! Bye...") (newline)
	(exit))))

(define issue-warnings? #f)

(define (issue-warnings . args)
  (if (not (null? args))
      (set! issue-warnings? (car args)))
  issue-warnings?)

(define load-noise-level #f)

; not entirely robust, but ok for now.

(define (load filename)
  (let ((p (open-input-file filename)))
    (let loop ((expr (read p)))
      (if (eof-object? expr)
	  (begin (close-input-port p)
		 #t)
	  (let ((result (eval expr)))
	    (if load-noise-level
		(begin (display result)
		       (newline)))
	    (loop (read p)))))))

; supports rewritten native code

(define %list list)
(define %append append)
(define %list->vector list->vector)
(define %cons cons)

; eof
