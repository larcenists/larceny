;;; Bundle system for Larceny
;;; 2002-10-29 / lth
;;;
;;; What is this?
;;;   A "bundle" is a package of FASL files that make up a Scheme
;;;   program.  It is useful on systems where heap dumping is not
;;;   possible, because it does not require moving entire directory
;;;   trees around.
;;;
;;;   BUILD-BUNDLE creates a bundle in a directory by evaluating a
;;;   Scheme command; whenever the command loads a FASL file, that file
;;;   is copied into the bundle.  Later, LOAD-BUNDLE can be used
;;;   to reexecute the command, and whenever it loads a FASL file the
;;;   file will be loaded from the bundle if it exists there.
;;;
;;;   The key for a file in the bundle is the exact path name that is
;;;   passed to LOAD when the bundle is created, so the bundle must be
;;;   loaded from a context comparable to where it was saved (except
;;;   that the FASL files need not exist any more).  The bundle can be
;;;   moved, however.
;;;   
;;; Implementation
;;;   A bundle is represented as a directory with a bunch of files and a
;;;   special file called BUNDLE, which contains a Scheme datum: the
;;;   table of contents for the bundle.
;;;
;;;   This implementation is for Unix-like systems.

;;; FLUID-LET copied from Larceny lib / common-syntax.sch for the sake of self-containedness.

(define-syntax fluid-let
  (syntax-rules ()
    ((_ ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e v) ...) () b1 b2 ...)
     (let-syntax ((swap!
		   (syntax-rules ()
		     ((swap! a b)
		      (let ((tmp a))
			(set! a b)
			(set! b tmp))))))
       (dynamic-wind
	(lambda () (swap! t v) ...)
	(lambda () b1 b2 ...)
	(lambda () (swap! t v) ...))))))


;;; BUILD-BUNDLE
;;;
;;; Create a bundle in directory "dir" by evaluating the Scheme
;;; command "cmd".

(define (build-bundle dir cmd)

  (define b '())
  (define n 0)
  (define toc (string-append dir "/BUNDLE"))

  (define (make-bundle)
    (system (string-append "mkdir -p " dir))
    (delete-file toc))

  (define (dump-to-bundle fn)
    (let* ((basename (string-append "f" (number->string n) ".fasl"))
	   (newname (string-append dir "/" basename)))
      (set! n (+ n 1))
      (system (string-append "cp " fn " " newname))
      (set! b (cons (cons fn basename) b))))

  (define (write-bundle)
    (call-with-output-file toc
      (lambda (f)
	(write (reverse b) f))))

  (make-bundle)
  (let ((old-load load))
    (fluid-let ((load (lambda (fn)
			(if (and (> (string-length fn) 5)
				 (string-ci=? (substring fn (- (string-length fn) 5) (string-length fn)) ".fasl"))
			    (dump-to-bundle fn))
			(old-load fn))))
      (cmd)))
  (write-bundle))


;;; LOAD-BUNDLE
;;;
;;; Load a bundle from the directory "dir" by evaluating the Scheme
;;; command "cmd".  (The command is usually the same that was used to
;;; create the bundle.)

(define (load-bundle dir cmd)

  (define b '())
  (define toc (string-append dir "/BUNDLE"))

  (define (read-bundle)
    (call-with-input-file toc
      (lambda (f)
	(set! b (read f)))))

  (define (file-in-bundle fn)
    (cond ((assoc fn b) => (lambda (x) (string-append dir "/" (cdr x))))
	  (else #f)))

  (read-bundle)
  (let ((old-load load)
	(old-file-exists? file-exists?)
	(old-file-modification-time file-modification-time))
    (fluid-let ((load
		 (lambda (fn)
		   (old-load (cond ((and (> (string-length fn) 5)
					 (string-ci=? (substring fn (- (string-length fn) 5) (string-length fn)) ".fasl")
					 (file-in-bundle fn)))
				   (else fn)))))
		(file-exists?
		 (lambda (fn)
		   (cond ((old-file-exists? fn))
			 ((file-in-bundle fn) #t)
			 (else #f))))
		(file-modification-time
		 (lambda (fn)
		   (cond ((old-file-exists? fn)
			  (old-file-modification-time fn))
			 ((file-in-bundle fn) => old-file-modification-time)
			 (else
			  #f)))))
      (cmd))))

;;; eof
