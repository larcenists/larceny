; Util/load-twobit-C-be.sch
; Load Twobit with the standard-C back-end, big-endian.
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
		      (standard-C-asm . "Asm/Standard-C/")
		      (target-machine . Standard-C)
		      (endianness     . big)
		      (word-size      . 32)
		      (always-source? . #t)
		      (verbose-load?  . #t)
		      (compatibility  . "Compat/Larceny/")
		      (host-system    . "Larceny")
		      )))
    (lambda (key)
      (let ((probe (assq key parameters)))
	(if probe 
	    (cdr probe)
	    (error "Bad nbuild parameter key " key))))))

(load "Compat/Larceny/compat.sch")
(compat:initialize)
(load "Util/nbuild.sch")

; eof
