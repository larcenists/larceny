; Util/load-twobit.sch
; 
; $Id$
;
; If you load this file into Larceny, you get almost the same setup
; that nbuild gives you.  The only difference is that the path names
; here are relative, while nbuild gives you absolute path names.

(define nbuild-parameter
  (let ((parameters `((compiler       . "Compiler/")
		      (util           . "Util/")
		      (build          . "Build/")
		      (source         . "Lib/")
		      (common-asm     . "Asm/Common/")
		      (sparc-old      . "Asm/Sparc-old/")
		      (sparc-asm      . "Asm/Sparc/")
		      (standard-C-asm . "Asm/C/")
		      (target-machine . SPARC)	           ; FIXME
		      (new-assembler? . #t)
		      (always-source? . #f)                ; FIXME
		      (verbose-load?  . #f)                ; FIXME
		      (compatibility  . "Larceny/")
		      (host-system    . "Larceny")
		      )))
    (lambda (key)
      (let ((probe (assq key parameters)))
	(if probe 
	    (cdr probe)
	    (error "Bad nbuild parameter key " path-ident))))))

(load "Larceny/compat.sch")
(compat:initialize)
(load "Util/nbuild.sch")

; eof
