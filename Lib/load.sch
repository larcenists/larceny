; -*- scheme -*-
;
; Larceny -- the loader.
;
; Not entirely robust, but ok for now.
;
; FIXME: Loader should install reader macros for #^G, #^B, #^P so that 
; the reader would not need to be aware of these extensions.

(define *load-noise-level* #f)

; The garbage collection causes the icache to be flushed if appropriate;
; alternatively this could be done when the bytevector is loaded.

(define (load filename . rest)
  (let* ((env (cond ((null? rest)
		     (interaction-environment))
		    ((and (null? (cdr rest))
			  (environment? (car rest)))
		     (car rest))
		    (else
		     (error "load: too many extra arguments: " rest))))
	  (old-resolver (global-name-resolver))
	  (new-resolver (lambda (sym)
			  (environment-lookup-binding env sym))))
    (dynamic-wind 
     (lambda ()
       (global-name-resolver new-resolver))
     (lambda ()
       (let ((p (open-input-file filename)))
	 (let loop ((expr (read p)))
	   (if (eof-object? expr)
	       (begin (close-input-port p)
		      (collect 'ephemeral)
		      #t)
	       (let ((result (eval expr env)))
		 (if *load-noise-level*
		     (begin (display result)
			    (newline)))
		 (loop (read p)))))))
     (lambda ()
       (global-name-resolver old-resolver)))))

(define (load-noisily . args)
  (let ((noise-level *load-noise-level*))
    (set! *load-noise-level* #t)
    (if (not (null? args))
	(begin (load (car args))
	       (set! *load-noise-level* noise-level)))))

(define (load-quietly . args)
  (let ((noise-level *load-noise-level*))
    (set! *load-noise-level* #f)
    (if (not (null? args))
	(begin (load (car args))
	       (set! *load-noise-level* noise-level)))))

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
	  p
	  (begin (procedure-set! p i (car l))
		 (loop (cdr l) (+ i 1)))))))

; eof
