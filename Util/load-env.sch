; Util/load-env.sch
; Larceny compilation environment -- compilation system loader file
;
; $Id: load-env.sch,v 1.2 1997/09/23 20:07:36 lth Exp lth $
;
; Usage: (load-environment module-file)
;        (load-environment module-file 'verbose)

; See the file modules.list for an example of the input.

(define (load-environment module-file . rest)

  (define envs '())
  (define verbose #f)

  (define (loadf files env)
    (for-each (lambda (fn)
		(if verbose
		    (format #t "~a...~%" fn))
		(load fn env))
	      files))

  (define (get name item failure list?)
    (let ((probe (assq name item)))
      (cond (probe
	     (if list?
		 (cdr probe)
		 (cadr probe)))
	    ((eq? failure 'fail)
	     (error "Can't get " name " in " item))
	    (else
	     failure))))

  (define (find-env name)
    (if (eq? name #f)
	envs
	(let ((probe (assq name envs)))
	  (if probe
	      (cadr probe)
	      (error "Environment " name " not found.")))))

  (define (remember-env name env imports)
    (set! envs (cons (list name env imports) envs)))

  (define (process-parent name)
    (if (eq? name '@interaction-environment@)
	(interaction-environment)
	(find-env name)))

  (define (process-item item)
    (let ((name    (get 'environment item 'fail #f))
	  (files   (get 'files item '() #t))
	  (parent  (process-parent (get 'parent item 'fail #f)))
	  (imports (get 'imports item '() #t)))
      (let ((env (make-environment (symbol->string name) parent)))
	(loadf files env)
	(remember-env name env imports))))

  (define (perform-imports)
    (for-each
     (lambda (entry)
       (let ((name (car entry))
	     (env  (cadr entry))
	     (imports (caddr entry)))
	 (for-each
	  (lambda (imp)
	    (let ((from (car imp))
		  (impname (cadr imp)))
	      (let ((from-env (find-env from)))
		;		(format #t "resolving ~a in ~a~%" impname from)
		(environment-set! env impname
				  (environment-get from-env impname)))))
	  imports)))
     envs))

  (define (lambda-expr? x)
    (and (pair? x) (eq? (car x) 'lambda)))

  (if (memq 'verbose rest)
      (set! verbose #t))

  (if (or (file-exists? ".larceny")
	  (file-exists? (string-append (getenv "HOME") "/" ".larceny")))
      (format #t "Warning: init file found; heap may not be clean.~%"))

  (call-with-input-file module-file
    (lambda (input)
      (do ((item (read input) (read input)))
	  ((eof-object? item) (perform-imports))
	(cond ((lambda-expr? item)
	       ((eval item) find-env))
	      (else
	       (process-item item))))))

  ; Become the new toplevel environment.

  (interaction-environment (find-env '@toplevel@))
  #!unspecified)

; eof
