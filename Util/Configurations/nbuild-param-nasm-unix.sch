; Copyright 1998 Lars T Hansen    -*- mode: scheme; indent-tabs-mode: nil -*-
;
; $Id$
;
; NBUILD parameters for Petit Larceny, x86-nasm/Unix.

(define (make-nbuild-parameter dir source? verbose? development?
                               hostdir hostname)
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
           (x86-nasm-asm   . ,(pathname-append dir "Asm" "Intel"))
           (always-source? . ,source?)
           (verbose-load?  . ,verbose?)
	   (development?   . ,development?)
           (compatibility  . ,(pathname-append dir "Compat" hostdir))
           (auxiliary      . ,(pathname-append dir "Auxlib"))
           (root           . ,dir)
           (host-system    . ,hostname)
           (target-machine . x86-nasm)
           (target-os      . unix)
           (host-os        . unix)
           (endianness     . little)
           (target-endianness . little)  ; same as "endianness"
           (host-endianness . little)
           (word-size      . 32)
           )))
    (lambda (key)
      (let ((probe (assq key parameters)))
        (if probe 
            (cdr probe)
            #f)))))

; eof
