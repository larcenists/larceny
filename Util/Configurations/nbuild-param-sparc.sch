; Copyright 1998 Lars T Hansen    -*- mode: scheme; indent-tabs-mode: nil -*-
;
; $Id$
;
; NBUILD parameters for SPARC Larceny, 32-bit, Unix OS.

(define (make-nbuild-parameter dir source? verbose? development? hostdir hostname)
  (let ((parameters 
         `((compiler       . ,(pathname-append dir "Twobit"))
           (util           . ,(pathname-append dir "Util"))
           (build          . ,(pathname-append dir "Rts" "Build"))
           (source         . ,(pathname-append dir "Lib"))
           (common-source  . ,(pathname-append dir "Lib" "Common"))
           (repl-source    . ,(pathname-append dir "Repl"))
           (interp-source  . ,(pathname-append dir "Interpreter"))
           (machine-source . ,(pathname-append dir "Lib" "Sparc"))
           (common-asm     . ,(pathname-append dir "Asm" "Common"))
           (sparc-asm      . ,(pathname-append dir "Asm" "Sparc"))
           (always-source? . ,source?)
           (verbose-load?  . ,verbose?)
           (development?   . ,development?)
           (compatibility  . ,(pathname-append dir "Compat" hostdir))
           (auxiliary      . ,(pathname-append dir "Auxlib"))
           (root           . ,dir)
           (host-system    . ,hostname)
           (host-os        . unix)
           (target-machine . SPARC)
           (target-os      . unix)
           (endianness     . big)
           (target-endianness . big)  ; same as "endianness"
           (host-endianness . big)
           (word-size      . 32)
           )))
    (lambda (key . rest)
      (let ((probe (assq key parameters)))
        (if (not probe)
            (error "nbuild-parameter: Unknown key: " key))
        (cond ((null? rest)
               (cdr probe))
              ((null? (cdr rest))
               (set-cdr! probe (car rest)))
              (else
               (error "Too many parameters to nbuild-parameter")))))))

; eof
