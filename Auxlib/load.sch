; Auxlib/load.sch
; Larceny auxiliary library -- a smart-but-not-intelligent 'load' procedure.
;
; $Id: load.sch,v 1.1.1.1 1998/11/19 21:52:17 lth Exp $
;
; Exported procedures:
;   (load filename)
;   (load-noisily filename)
;   (load-quietly filename)
;
; This version of 'load' extends the normal 'load' in several ways.
;
;   * There is a *load-path* variable, which is a list of directories.
;     If the filename to load is not absolute, then directories in the
;     load path are prepended to the given file name, and the resulting
;     files are tried, in order.   (The current working directory isn't
;     tried unless it's also in the path.)  The default path contains 
;     the current directory only.
;
;   * There is a *load-extensions* variable, which is a list of alternate
;     file extensions to try.  If the requested file is not found anywhere
;     in the load path, then another search is made with the alternate
;     extensions, in the order of the load path.  If a file with an alternate
;     extension is found, then it is loaded.  Ties in a given directory
;     are resolved by examining file modification times: the file that was
;     modified most recently is chosen.  The default extensions are .sch,
;     .scm, and .fasl.
;
;   * If the variable *load-prefer-requested?* is #f, then the extension
;     search is done without searching for the requested name first.  The
;     default is #t.
;
;   * If the variable *load-print-filename?* is #t, then the name of the
;     file loaded is printed.  The default is #t.
;
; The code relies on os-dependent code for most file name manipulations.
; See the files Auxlib/osdep-*.sch.
;
; BUGS
;   The file name manipulation code is not particularly general but should
;   work for Unix, MS-DOS, and MacOS.

; Load parameters

(define *load-print-filename?* #t)
(define *load-prefer-requested?* #t)
(define *load-path* (list *current-directory-designator*))
(define *load-extensions* '("sch" "scm" "fasl"))

; Private parameters

(define *load-noise-level* #f)

(define (%%new-load load-file fn)

  (define (string-search-reverse s chars)
    (let loop ((i (- (string-length s) 1)))
      (cond ((< i 0) #f)
	    ((memq (string-ref s i) chars) i)
	    (else (loop (- i 1))))))

  (define (load-file filename)
    (if *load-print-filename?*
	(begin (display "; Loading ")
	       (display filename)
	       (newline)))
    (load-file filename))

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
	  (least file-newer? candidates))))

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
  
  (cond ((if (relative-pathname? fn)
	     (find-relative-file)
	     (find-absolute-file))
	 =>
	 (lambda (fn)
	   (load-file fn)))
	(else
	 (error "load: Could not find file " fn)))
  (unspecified))

(define load
  (let ((load       load)		; Original load procedure
	(%%new-load %%new-load))	; New load procedure
    (lambda (fn . load-args)
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
	   (%%new-load (lambda (fn)
			 (apply load fn load-args))
		       fn))
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

; eof
