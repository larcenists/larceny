; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Nbuild parameters for Petit Larceny, big-endian, 32-bit.

(define (make-nbuild-parameter dir source? verbose? hostdir hostname)
  (let ((parameters `((compiler       . ,(string-append dir "Compiler/"))
		      (util           . ,(string-append dir "Util/"))
		      (build          . ,(string-append dir "Rts/Build/"))
		      (source         . ,(string-append dir "Lib/"))
                      (common-source  . ,(string-append dir "Lib/Common/"))
                      (repl-source    . ,(string-append dir "Repl/"))
                      (interp-source  . ,(string-append dir "Eval/"))
                      (machine-source . ,(string-append dir "Lib/Standard-C/"))
		      (common-asm     . ,(string-append dir "Asm/Common/"))
		      (standard-C-asm . ,(string-append dir "Asm/Standard-C/"))
		      (target-machine . Standard-C)
		      (endianness     . big)
		      (word-size      . 32)
		      (always-source? . ,source?)
		      (verbose-load?  . ,verbose?)
		      (compatibility  . ,(string-append dir 
							"Compat/" hostdir "/"))
		      (host-system    . ,hostname)
		      )))
    (lambda (key)
      (let ((probe (assq key parameters)))
	(if probe 
	    (cdr probe)
	    #f)))))

(define nbuild-parameter
  (make-nbuild-parameter "./" (getenv "SOURCE_ONLY") #t "Larceny" "Larceny"))

; eof
