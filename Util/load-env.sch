; Util/load-env.sch
; Compilation system loader file.
;
; $Id: load-env.sch,v 1.1.1.1 1998/11/19 21:51:54 lth Exp $
;
; Usage: (load-environment module-file &opt 'verbose &opt basedir)

; See the file modules.list for an example of the input.

(define (load-environment module-file . rest)

  (define envs '())
  (define verbose #f)
  (define basedir "")

  (define (loadf files env)
    (for-each (lambda (fn)
		(let ((fn (string-append basedir fn)))
		  (if verbose
		      (begin (display fn)
			     (newline)))
		  (load fn env)))
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
    (let* ((name    (get 'environment item 'fail #f))
	   (files   (get 'files item '() #t))
	   (parent  (if (eq? name '@interaction-environment@)
			#t
			(process-parent (get 'parent item 'fail #f))))
	   (imports (get 'imports item '() #t)))
      (let ((env (if (eq? name '@interaction-environment@)
		     (interaction-environment)
		     (make-environment (symbol->string name) parent))))
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

  ; Parse arguments

  (do ((rest rest (cdr rest)))
      ((null? rest))
    (cond ((eq? (car rest) 'verbose)
	   (set! verbose #t))
	  ((string? (car rest))
	   (set! basedir (car rest)))))

  ; Make sure base directory path is OK

  (if (and (not (= (string-length basedir) 0))
	   (not (char=? (string-ref basedir (- (string-length basedir) 1))
			#\/)))
      (set! basedir (string-append basedir "/")))

  ; Warn if init file exists and may have been loaded

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

  ; If there is a toplevel environment, become it.

  (let ((probe (assq '@toplevel@ envs)))
    (if probe
	(interaction-environment (cadr probe))))

  ; Check for undefined variables

  (for-each (lambda (e)
	      (let ((name (car e))
		    (env  (cadr e)))
		(for-each (lambda (v)
			    (if (not (environment-gettable? env v))
				(format #t "Undefined variable ~a in ~a~%"
					v name)))
			  (environment-variables env))))
	    envs)

  (unspecified))

; eof
