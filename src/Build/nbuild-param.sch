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
             (error "make-nbuild-parameter: Unknown property name: " (car as)))))

    ; Apply defaults
    (nbuild-param-defaults parameters)

    ; Test that everything is defined
    (let loop ((ps parameters))
      (cond ((null? ps))
            ((eq? 'unknown (cdar ps))
             (error "make-nbuild-parameter: Missing value for parameter: " (caar ps)))
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
               (error "nbuild-parameter: Too many arguments: " (cons key rest))))))))

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
    (apply-parameter-value 'compiler (pathname-append dir "src" "Compiler"))
    (apply-parameter-value 'util     (pathname-append dir "src" "Build"))
    (apply-parameter-value 'rts      (pathname-append dir "src" "Rts"))
    (apply-parameter-value 'include  (pathname-append dir "include"))
    (apply-parameter-value 'common-include  (pathname-append dir "include" "Shared"))
    (apply-parameter-value 'nasm-include  (pathname-append dir "include" "Nasm"))
    (apply-parameter-value 'stdc-include  (pathname-append dir "include" "Standard-C"))
    (apply-parameter-value 'sys-include   (pathname-append dir "include" "Sys"))
    (apply-parameter-value 'source   (pathname-append dir "src" "Lib"))
    (apply-parameter-value 'common-source (pathname-append dir "src" "Lib" "Common"))
    (apply-parameter-value 'repl-source (pathname-append dir "src" "Lib" "Repl"))
    (apply-parameter-value 'interp-source (pathname-append dir "src" "Lib" "Interpreter"))
    (apply-parameter-value 'common-asm (pathname-append dir "src" "Asm" "Shared"))
    (apply-parameter-value 'sparc-asm (pathname-append dir "src" "Asm" "Sparc"))
    (apply-parameter-value 'x86-nasm-asm (pathname-append dir "src" "Asm" "Nasm"))
    (apply-parameter-value 'x86-sass-asm (pathname-append dir "src" "Asm" "IAssassin"))
    (apply-parameter-value 'standard-C-asm (pathname-append dir "src" "Asm" "Standard-C"))
    (apply-parameter-value 'dotnet-asm     (pathname-append dir "src" "Asm" "IL"))
    ;; 'compatibility varies with host scheme, but 'larceny-compatibility
    ;; always points to src/Compat/Larceny
    (apply-parameter-value 'compatibility (pathname-append dir "src" "Compat" "Larceny"))
    (apply-parameter-value 'larceny-compatibility (pathname-append dir "src" "Compat" "Larceny"))
    (apply-parameter-value 'srfi (pathname-append dir "lib" "SRFI"))
    (apply-parameter-value 'auxiliary (pathname-append dir "lib" "Base"))
    (apply-parameter-value 'debugger (pathname-append dir "lib" "Debugger"))
    (apply-parameter-value 'always-source? #t)
    (apply-parameter-value 'verbose-load? #t)
    (apply-parameter-value 'development? #t)
    (apply-parameter-value 'host-system "Larceny")
    (apply-parameter-value 'word-size 32)
    (apply-parameter-value 'globals-table "globals.cfg")))

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
         standard-C-asm
         always-source?
         verbose-load?
         development?
         compatibility
         larceny-compatibility
         srfi
         auxiliary
         debugger
         root
         host-system
         host-os
         host-endianness
         target-machine
         target-os
         target-endianness
         word-size
         globals-table)))

; eof
