; Larceny auxiliary library -- a more intelligent 'load' procedure.
;
; $Id$
;
; This version of 'load' extends the normal 'load' in several ways:
; - multiple arguments
; - implicit extensions are tried if the name does not exists
; - fasl files are loaded if they exist and are newer than the scheme
;   file
; - supports a load search path
; - supports multiple implicit source extensions

(define load-path       '("."))      ; list of strings
(define src-extensions  '("sch"))    ; list of strings

(define load
  (let ((old-load load))

    ; Note: with multiple source (and fasl) extensions, we must find
    ; all matches and their modification dates, and take the newest from
    ; each class.

    (define (try-load path fn)
      (let ((file (string-append path "/" fn)))
	(if (file-exists? file)
	    (old-load file)
	    (let ((sname (string-append file ".sch"))
		  (fname (string-append file ".fasl")))
	      (cond ((and (file-exists? sname)
			  (file-exists? fname))
		     (if (file-newer? fname sname)
			 (old-load fname)
			 (old-load sname))
		     #t)
		    ((file-exists? fname)
		     (old-load fname)
		     #t)
		    ((file-exists? sname)
		     (old-load sname)
		     #t)
		    (else
		     #f))))))

    (define (file-newer? fna fnb)
      ...)

    (define (do-load fn)
      (if (some? (lambda (path)
		   (try-load path fn))
		 load-path)
	  #t
	  (error "Unable to load file " fn)))

    (define (load . rest)
      (for-each do-load rest))

    load))

; eof
