; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Nbuild parameters for SPARC Larceny.

(define (make-nbuild-parameter dir source? verbose? hostdir hostname)
  (let ((parameters 
	 `((compiler       . ,(pathname-append dir "Compiler"))
	   (util           . ,(pathname-append dir "Util"))
	   (build          . ,(pathname-append dir "Rts" "Build"))
	   (source         . ,(pathname-append dir "Lib"))
           (common-source  . ,(pathname-append dir "Lib" "Common"))
           (repl-source    . ,(pathname-append dir "Repl"))
           (interp-source  . ,(pathname-append dir "Eval"))
           (machine-source . ,(pathname-append dir "Lib" "Sparc"))
	   (common-asm     . ,(pathname-append dir "Asm" "Common"))
	   (sparc-asm      . ,(pathname-append dir "Asm" "Sparc"))
	   (target-machine . SPARC)
	   (endianness     . big)
	   (word-size      . 32)
	   (always-source? . ,source?)
	   (verbose-load?  . ,verbose?)
	   (compatibility  . ,(pathname-append dir "Compat" hostdir))
	   (host-system    . ,hostname)
	   )))
    (lambda (key)
      (let ((probe (assq key parameters)))
	(if probe 
	    (cdr probe)
	    #f)))))

(define nbuild-parameter
  (make-nbuild-parameter "" #f #f "Larceny" "Larceny"))

; eof
