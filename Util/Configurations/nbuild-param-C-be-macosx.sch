; Copyright 1998 Lars T Hansen    -*- mode: scheme; indent-tabs-mode: nil -*-
;
; $Id$
;
; NBUILD parameters for Petit Larceny, big-endian, 32-bit, MacOS X.

(define (make-nbuild-parameter dir source? verbose? development? hostdir hostname)
  (let ((parameters 
         `((compiler       . ,(pathname-append dir "Twobit"))
           (util           . ,(pathname-append dir "Util"))
           (build          . ,(pathname-append dir "Rts" "Build"))
           (source         . ,(pathname-append dir "Lib"))
           (common-source  . ,(pathname-append dir "Lib" "Common"))
           (repl-source    . ,(pathname-append dir "Repl"))
           (interp-source  . ,(pathname-append dir "Interpreter"))
           (machine-source . ,(pathname-append dir "Lib" "Standard-C"))
           (common-asm     . ,(pathname-append dir "Asm" "Common"))
           (standard-C-asm . ,(pathname-append dir "Asm" "Standard-C"))
           (always-source? . ,source?)
           (verbose-load?  . ,verbose?)
	   (development?   . ,development?)
           (compatibility  . ,(pathname-append dir "Compat" hostdir))
           (auxiliary      . ,(pathname-append dir "Auxlib"))
           (root           . ,dir)
           (host-system    . ,hostname)
           (target-machine . Standard-C)
           (target-os      . macosx)
           (host-os        . macosx)
           (endianness     . big)
           (target-endianness . big)  ; same as "endianness"
           (host-endianness . big)
           (word-size      . 32)
           )))
    (lambda (key)
      (let ((probe (assq key parameters)))
        (if probe 
            (cdr probe)
            #f)))))

; eof
