; Auxlib/load.sch
; Larceny auxiliary library -- a smart-but-not-intelligent 'load' procedure.
;
; $Id: load.sch,v 1.1 1997/02/27 16:48:16 lth Exp $
;
; This version of 'load' extends the normal 'load' in several ways.
;   * There is a load path.  If the filename to load is not absolute, then
;     directories in the load path are prepended to the given file name,
;     and the resulting files are tried, in order.  Generally, you'd want
;     to have `.' in the load path.
;   * There is a list of alternate extensions.  If the named file is not
;     found anywhere in the load path, then another search is made with
;     the alternate extensions, in the order of the load path.  If a file
;     with an alternate extension is found, then it is loaded.  Ties are
;     resolved by examining file modification times: the last modified
;     is chosen.
;   * If *load-prefer-requested?* is #f, then the extension search is
;     done without searching for the requested name first.  The default 
;     is #t.
;   * If *load-print-filename?* is #t, then the name of the file loaded
;     is printed.

; This code is not super robust w.r.t. all different forms of path 
; names; for that we need a more general pathname package (which I'm 
; not interested in spending time on right now).

; Load parameters

(define *load-print-filename?* #t)
(define *load-prefer-requested?* #t)
(define *load-path* (list *current-directory-designator*))
(define *load-extensions* '("sch" "scm" "fasl"))
(define *load-noise-level* #f)

(define load
  (let ((load load))
    (lambda (fn . args)
      (let* ((evaluator
	      (load-evaluator))
	     (new-ev
	      (lambda (expr env)
		(call-with-values
		 (lambda ()
		   (evaluator expr env))
		 (lambda results
		   (if *load-noise-level*
		       (cond ((null? results)
			      (display "; No values.") (newline))
			     ((null? (cdr results))
			      ((repl-printer) (car results)))
			     (else
			      (display "; ")
			      (display (length results))
			      (display " values") (newline)
			      (do ((results results (cdr results)))
				  ((null? results))
				((repl-printer) (car results)))))))))))
	(dynamic-wind
	 (lambda ()
	   (load-evaluator new-ev))
	 (lambda ()
	   (%%new-load load fn args))
	 (lambda ()
	   (let ((e (load-evaluator)))
	     (if (eq? e new-ev)
		 (load-evaluator evaluator)))))))))

(define load-noisily)
(define load-quietly)

(let ((load-with-noise 
       (lambda (new-noise)
	 (lambda args
	   (if (null? args)
	       (set! *load-noise-level* new-noise)
	       (let ((noise *load-noise-level*))
		 (dynamic-wind
		  (lambda () (set! *load-noise-level* new-noise))
		  (lambda () (apply load args))
		  (lambda () (set! *load-noise-level* noise)))))
	   (unspecified)))))
       
  (set! load-noisily (load-with-noise #t))
  (set! load-quietly (load-with-noise #f)))

(define (%%new-load load fn args)

  (define (load-file filename rest)
    (if *load-print-filename?*
	(begin (display "; Loading ")
	       (display filename)
	       (newline)))
    (apply load filename rest))

  (define (append-directory dir fn)
    (cond ((string=? dir "") fn)
	  ((memv (string-ref dir (- (string-length dir) 1))
		 *directory-separators*)
	   (string-append dir fn))
	  (else
	   (string-append dir (string (car *directory-separators*)) fn))))

  (define (replace-extension fn ext)
    (let ((x (string-search-reverse fn (cons #\. *directory-separators*))))
      (cond ((not x)
	     (string-append fn "." ext))
	    ((char=? (string-ref fn x) #\.)
	     (string-append (substring fn 0 (+ x 1)) ext))
	    (else
	     (string-append fn "." ext)))))

  (define (preference fn extensions)
    (let ((candidates
	   (filter file-exists?
		   (cons fn (map (lambda (e)
				   (replace-extension fn e))
				 extensions)))))
      (if (null? candidates)
	  #f
	  (smallest file-newer? candidates))))

  (define (relative-preference fn extensions dirs)
    (cond ((null? dirs) #f)
	  ((preference (append-directory (car dirs) fn) extensions))
	  (else (relative-preference fn extensions (cdr dirs)))))

  (define (find-relative-file)
    (or (and *load-prefer-requested?*
	     (relative-preference fn '() *load-path*))
	(relative-preference fn *load-extensions* *load-path*)))

  (define (find-absolute-file)
    (or (and *load-prefer-requested?*
	     (preference fn '()))
	(preference fn *load-extensions*)))
  
  (cond ((if (relative-filename? fn)
	     (find-relative-file)
	     (find-absolute-file))
	 =>
	 (lambda (fn)
	   (load-file fn args)))
	(else
	 (error "load: Could not find file " fn)))
  (unspecified))

; eof
