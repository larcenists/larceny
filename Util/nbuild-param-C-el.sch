; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Nbuild parameters for Petit Larceny, endian-little, 32-bit.

(define (make-nbuild-parameter dir source? verbose? hostdir hostname)
  (let ((parameters `((compiler       . ,(string-append dir "Compiler/"))
		      (util           . ,(string-append dir "Util/"))
		      (build          . ,(string-append dir "Build/"))
		      (source         . ,(string-append dir "Lib/"))
		      (common-asm     . ,(string-append dir "Asm/Common/"))
		      (standard-C-asm . ,(string-append dir "Asm/Standard-C/"))
		      (target-machine . Standard-C)
		      (endianness     . little)
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
  (make-nbuild-parameter "./" #f #f "Larceny" "Larceny"))

; eof
