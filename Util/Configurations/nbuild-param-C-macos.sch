; Copyright 1998 Lars T Hansen    -*- mode: scheme; indent-tabs-mode: nil -*-
;
; $Id$
;
; Nbuild parameters for Petit Larceny, MacOS (big-endian, 32-bit)

(define (make-nbuild-parameter dir source? verbose? hostdir hostname)
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
           (compatibility  . ,(pathname-append dir "Compat" hostdir))
           (auxiliary      . ,(pathname-append dir "Auxlib"))
           (root           . ,dir)
           (host-system    . ,hostname)
           (target-machine . Standard-C)
           (target-os      . macos)
           (endianness     . big)
           (word-size      . 32)
           )))
    (lambda (key)
      (let ((probe (assq key parameters)))
        (if probe 
            (cdr probe)
            #f)))))

; eof
