; Util/nbuild-params.sch
; The nbuild-parameter procedure, for SPARC.
;
; $Id$

(define nbuild-parameter)

(define (setup-nbuild-parameters target-machine)
  (let ((parameters `((compiler       . "Compiler/")
		      (util           . "Util/")
		      (build          . "Build/")
		      (source         . "Lib/")
		      (common-asm     . "Asm/Common/")
		      (sparc-old      . "Asm/Sparc-old/")
		      (sparc-asm      . "Asm/Sparc/")
		      (standard-C-asm . "Asm/C/")
		      (target-machine . ,target-machine)
		      (new-assembler? . #t)
		      (always-source? . #f)
		      (verbose-load?  . #f)
		      (compatibility  . "Larceny/")
		      (host-system    . "Larceny")
		      )))
    (set! nbuild-parameter 
	  (lambda (key)
	    (let ((probe (assq key parameters)))
	      (if probe 
		  (cdr probe)
		  (error "Bad nbuild parameter key " path-ident)))))))

; eof
