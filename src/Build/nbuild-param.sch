; Copyright 1998 Lars T Hansen    -*- mode: scheme; indent-tabs-mode: nil -*-
;
; $Id$
;
; MAKE-NBUILD-PARAMETER

(define (make-nbuild-parameter . args)
  (let ((parameters (nbuild-parameter-template)))

    ; Setup

    (let loop ((as args))
      (cond ((null? as))
            ((null? (cdr as))
             (error "make-nbuild-parameter: Odd number of arguments: " args))
            ((assq (car as) parameters)
             => (lambda (cell)
                  (set-cdr! cell (cadr as))
                  (loop (cddr as))))
            (else
             (error "make-nbuild-parameter: Unknown property name: "
                    (car as)))))

    ; Apply defaults
    (nbuild-param-defaults parameters)

    ; Test that everything is defined
    (let loop ((ps parameters))
      (cond ((null? ps))
            ((eq? 'unknown (cdar ps))
             (error "make-nbuild-parameter: Missing value for parameter: "
                    (caar ps)))
            (else
             (loop (cdr ps)))))

    ; Return an accessor/updater function
    (lambda (key . rest)
      (let ((probe (assq key parameters)))
        (if (not probe)
            (error "nbuild-parameter: Unknown key: " key))
        (cond ((null? rest)
               (cdr probe))
              ((null? (cdr rest))
               (set-cdr! probe (car rest)))
              (else
               (error "nbuild-parameter: Too many arguments: "
                      (cons key rest))))))))

(define (nbuild-param-defaults parameters)

  (define (apply-parameter-value name v)
    (let ((cell (assq name parameters)))
      (if (eq? 'unknown (cdr cell))
          (set-cdr! cell v))))

  (define (parameter-value name)
    (let ((v (cdr (assq name parameters))))
      (if (eq? 'unknown v)
          (error "nbuild-param-default: reading unknown value: " name)
          v)))

  (apply-parameter-value 'root "")

  (let ((dir (parameter-value 'root)))

    ; Abbreviation to shorten the lines.

    (define (apv name v) (apply-parameter-value name v))

    (apv 'compiler              (pathname-append dir "src" "Compiler"))
    (apv 'util                  (pathname-append dir "src" "Build"))
    (apv 'rts                   (pathname-append dir "src" "Rts"))
    (apv 'include               (pathname-append dir "include"))
    (apv 'common-include        (pathname-append dir "include" "Shared"))
    (apv 'nasm-include          (pathname-append dir "include" "Nasm"))
    (apv 'stdc-include          (pathname-append dir "include" "Standard-C"))
    (apv 'sys-include
                                (pathname-append dir "include" "Sys"))
    (apv 'source                (pathname-append dir "src" "Lib"))
    (apv 'common-source         (pathname-append dir "src" "Lib" "Common"))
    (apv 'repl-source           (pathname-append dir "src" "Lib" "Repl"))
    (apv 'interp-source        (pathname-append dir "src" "Lib" "Interpreter"))
    (apv 'common-asm            (pathname-append dir "src" "Asm" "Shared"))
    (apv 'sparc-asm             (pathname-append dir "src" "Asm" "Sparc"))
    (apv 'x86-nasm-asm          (pathname-append dir "src" "Asm" "Nasm"))
    (apv 'x86-sass-asm          (pathname-append dir "src" "Asm" "IAssassin"))
    (apv 'standard-c-asm        (pathname-append dir "src" "Asm" "Standard-C"))
    (apv 'dotnet-asm            (pathname-append dir "src" "Asm" "IL"))

    ;; 'compatibility varies with host scheme, but 'larceny-compatibility
    ;; always points to src/Compat/Larceny

    (apv 'compatibility         (pathname-append dir "src" "Compat" "Larceny"))
    (apv 'larceny-compatibility (pathname-append dir "src" "Compat" "Larceny"))
    (apv 'srfi                  (pathname-append dir "lib" "SRFI"))
    (apv 'r6rs                  (pathname-append dir "lib" "R6RS"))
    (apv 'auxiliary             (pathname-append dir "lib" "Base"))
    (apv 'debugger              (pathname-append dir "lib" "Debugger"))
    (apv 'always-source?        #t)
    (apv 'verbose-load?         #t)
    (apv 'development?          #t)
    (apv 'host-system           "Larceny")
    (apv 'word-size             32)
    (apv 'globals-table         "globals.cfg")))

(define (nbuild-parameter-template)
  (map (lambda (n) (cons n 'unknown))
       '(compiler 
         util
         rts
         include
         common-include
         nasm-include
         stdc-include
         sys-include
         source
         common-source
         repl-source
         interp-source
         machine-source
         mzscheme-source
         common-asm
         dotnet-asm
         sparc-asm
         x86-nasm-asm
         x86-sass-asm
         standard-c-asm
         always-source?
         verbose-load?
         development?
         compatibility
         larceny-compatibility
         srfi
         r6rs
         auxiliary
         debugger
         root
         host-system
         host-os
         host-endianness
         target-string-rep
         target-machine
         target-os
         target-endianness
         word-size
         globals-table)))

; eof
