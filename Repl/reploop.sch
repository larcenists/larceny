; -*- scheme -*-
;
; Larceny -- read-eval-print loop and error handler.
;
; lth@cs.uoregon.edu / 1992
; $Id$

(define *reset-continuation* #f)    ; to jump to for error or reset
(define *saved-continuation* #f)    ; saved on error

(define *argv*)

(define (command-line-arguments)
  *argv*)

(define (main argv)
  (display "; Evaluator version ")
  (display eval-version)
  (newline)
  (reset-handler new-reset-handler)
  (error-handler (new-error-handler))
  (init-toplevel-environment)
  (rep-loop0 argv))

(define (rep-loop0 argv)
  (set! *argv* argv)
  ; FIXME: Should reinitialize I/O system!!!!
  ; Try to load init file; on error signal error and abandon load.
  (if (not (call-with-current-continuation
	    (lambda (k)
	      (set! *reset-continuation* k)
	      (load-init-file)
	      #t)))
      (begin (display "Init file load failed.")
	     (newline)))
  (if (and (> (vector-length (command-line-arguments)) 0)
	   (file-exists? (vector-ref (command-line-arguments) 0)))
      (if (not (call-with-current-continuation
		(lambda (k)
		  (set! *reset-continuation* k)
		  (load (vector-ref (command-line-arguments) 0))
		  #t)))
	  (begin (display "Failed to load ")
		 (display (vector-ref (command-line-arguments) 0))
		 (newline))))
  (rep-loop))

(define (load-init-file)
  (let ((home (getenv "HOME")))
    (cond ((file-exists? ".larceny")
	   (display "; Loading .larceny")
	   (newline)
	   (load ".larceny"))
	  (home
	   (let ((fn (string-append home "/.larceny")))
	     (if (file-exists? fn)
		 (begin (display "; Loading ~/.larceny")
			(newline)
			(load fn))))))))

(define (rep-loop)

  (define (loop)
    (display "> ")
    (flush-output-port)
    (let ((expr   (read)))
      (if (not (eof-object? expr))
	  (let ((result (eval expr)))
	    (display result)
	    (newline)
	    (loop))
	  (begin (newline)
		 (exit)))))

  ; Setup the error continuation

  (call-with-current-continuation
   (lambda (k)
     (set! *reset-continuation* k)))
  (newline)
  (loop))

(define (new-error-handler)
  (let ((old (error-handler)))
    (lambda args
      (call-with-current-continuation 
       (lambda (k) 
	 (set! *saved-continuation* k)
	 (apply old args))))))

(define (error-continuation)
  *saved-continuation*)

(define (backtrace)
  (print-continuation (error-continuation)))

(define (new-reset-handler)
  (if *reset-continuation*
      (*reset-continuation* #f)
      (begin (display "No reset continuation! Exiting.")
	     (newline)
	     (exit))))

(define (dump-interactive-heap filename)
  (dump-heap filename rep-loop0))


; eof
