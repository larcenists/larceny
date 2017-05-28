; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on generic Unix
; systems (including MacOS X), under arbitrary Scheme systems.
;
; This program is sets up parameters for a unified development
; environment; it unifies the petit-unix-be.sch and petit-unix-el.sch
; scripts.  
;
; See also petit-setup.sch, which sets parameters based on user input.
; and petit-unix-defns-globals.sch, which defines several globals
; used and mutated here and there.

(load "src/Build/petit-unix-defns-globals.sch")

(define (unix-&-win32-initialize)
  (load (case *host:os*
          ((unix macosx macosx-el solaris linux-el linux-arm-el)
           "src/Build/sysdep-unix.sch")
          ((cygwin win32)
           "src/Build/sysdep-win32.sch")
          (else
           (error 'unix-&-win32-initialize "Must add support for host:os"))
          ))
  (load "src/Build/nbuild-param.sch")
  (set! nbuild-parameter
        (make-nbuild-parameter
         'always-source?    *always-source*
         'verbose-load?     *verbose-load*
         'development?      #t
         'machine-source    (pathname-append
                             "src" "Lib" "Arch" *target:machine-source*)
         'mzscheme-source   (pathname-append "src" "Lib" "MzScheme")
         'host-os           *host:os*
         'host-endianness   *host:endianness*
         'target-string-rep *target:string-rep*
         'target-machine    *target:machine*
         'target-os         *target:os*
         'target-endianness *target:endianness*
         'compatibility     (pathname-append "src" "Compat" *host-dir*)
         'globals-table     *globals-table*
         'host-system       *host-name*))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (param-filename 'compatibility "compat.sch"))
  (compat:initialize)
  (recognize-keywords? #f) ;; don't treat :NONE as (QUOTE NONE)
  (compat:load (param-filename 'util "expander.sch"))
  (compat:load (param-filename 'util "config.sch"))
  #t)

;; param-filename : Symbol x { String } x String           -> FilePath
;; param-filename : Symbol x { String } x (List-of String)
;;                      -> (List-of FilePath)

(define (param-filename param . components)
  (let* ((reversed (reverse components))
         (init-r   (cdr reversed))
         (last     (car reversed))
         (lead     (nbuild-parameter param)))
   (define (one-file x)
     (apply make-filename lead (reverse (cons x init-r))))
   (cond
    ((string? last)  (one-file last))
    ((list? last)    (map one-file last)))))

(define (build-makefile . rest)
  (let ((c (cond (*makefile-configuration*)
                 ((null? rest) (default-makefile-configuration))
                 (else (car rest)))))
    (generate-makefile (param-filename 'rts "Makefile") c)))

(define (build-config-files)
  (define (in-rts . components)
    (apply param-filename 'rts components))
  (define (in-include . components)
    (apply param-filename 'include components))
  (expand-file
   (in-rts "Shared" "arithmetic.mac") (in-rts "Shared" "arithmetic.c"))
  (config (in-rts "except.cfg"))
  (config (in-rts "layouts.cfg"))
  (config (in-rts (nbuild-parameter 'globals-table)))
  (config (in-rts "mprocs.cfg"))
  (case *runtime-type* 
    ((sparc-native) (config (in-rts "sparc-regs.cfg")))
    ((sassy-native) (config (in-rts "iasn-regs.cfg")))
    ((arm-native) (config (in-rts "fence-arm-regs.cfg")))
    ((arm-native0) (config (in-rts "fence-arm-regs0.cfg")))
    ((petit) #t)
    (else (error "No registers config file for this runtime-type: " *runtime-type*)))
  (catfiles (map in-include
                 '("globals.ch"
                   "except.ch"
                   "layouts.ch"
                   "mprocs.ch"))
            (in-include "cdefs.h"))

  ;; for Sparc and Intel and ARM native

  (catfiles (map in-include
                 `("globals.ah"
                   "except.ah"
                   "layouts.ah"
                   "mprocs.ah"
                   ,@(case *runtime-type* 
                       ((sparc-native sassy-native arm-native arm-native0)
                        '("regs.ah"))
                       (else '()))))
            (in-include "asmdefs.h"))

  (compat:load (in-rts "features.sch"))

  ;; Note: *CHANGE-FEATURE-SET* defined as #f above;
  ;; expression has no effect unless SETUP run

  (let-syntax ((feature-case (syntax-rules ()
                               ((feature-case ID ...)
                                (case *change-feature-set*
                                  ((ID) (set! selected-feature-set ID))
                                  ...)))))

    ;; Copied names from features.sch.
    ;; This code might be better off in that file
    ;; (where *CHANGE-FEATURE-SET* would be a parameter to DEFINE-FEATURE-SET)

    (feature-case
     features-sparc-solaris             ; solaris 2.5 or later
     features-petit-solaris             ; solaris 2.5 or later
     features-petit-macosx              ; gcc and GNU libc
     features-petit-macosx-el           ; gcc and GNU libc
     features-petit-win32               ; works for Mingw; believed to work
     features-petit-linux               ; Debian GNU/Linux 3.0 (woody), x86
     features-petit-cygwin              ; Tested with cygwin 1.5.10 (May 2004)
     features-x86-nasm-linux            ; Debian GNU/Linux 3.0 (woody), x86
     features-x86-sassy-macosx
     features-x86-sassy-linux
     features-x86-nasm-win32
     features-x86-sassy-win32
     features-arm-el-hardfp-linux	; Hardfp is modern
     features-petit-linux-redhat5       ; Very old, Redhat linux 5.1
     features-sparc-linux-debian        ; Very old, SPARC Debian v2(?)
     features-petit-macos9-cw3          ; Very old (ca v0.48), CW Pro 3
     features-petit-osf4-alpha          ; Very old, OSF/1 4.0 on DEC Alpha
     ))
  (set! selected-feature-set
        (append *runtime:additional-features* selected-feature-set))

  (define-feature-set)
  )

(define load load)

(define (load-compiler . how)

  (define do-etags #f)
  (define old-load load)
  (define loaded-files '())

  (if (not (null? how))
      (case (car how)
        ((release) ;; matching code in sparc-unix.sch
         (nbuild-parameter 'always-source? #f)
         (nbuild-parameter 'verbose-load? #f)
         (nbuild-parameter 'development? #f))
        ((development) ;; matching code in petit-unix-common.sch
         (nbuild-parameter 'development? #t))
        ((etags)
         (set! do-etags #t)
         (set! load (lambda (filename)
                      (let ((val (old-load filename)))
                        (set! loaded-files (cons filename loaded-files))
                        val))))
        ))

  (cond ((eq? *target:machine* 'x86-sass)
         (compat:load-sassy)))

  (compat:load (param-filename 'util "nbuild.sch"))
  (set! load old-load)
  (cond (do-etags 
         (let ((cmd (apply string-append 
                           (cons "etags "
                                 (apply append 
                                        (map (lambda (x) (list x " "))
                                             loaded-files))))))
           (system cmd))))
  (if (eq? 'petit *heap-type*)
      (configure-system))
  (welcome)
  (unspecified)) 

; eof
