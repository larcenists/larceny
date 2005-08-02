; $Id$
;
; General script for building Larceny on x86 using the NASM backend,
; under Larceny on Unix.

(define nbuild-parameter #f)

(load "Util\\petit-win32.sch") ;; embarressingly wrong thing to do

(define (win32-initialize)
  (load "Util\\sysdep-win32.sch")
  (load "Util\\nbuild-param.sch")
  ; (load "Util\\petit-unix-common.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter 'always-source? #f
                               'verbose-load? #t
                               'development? #t
                               'machine-source "Lib\\Standard-C\\"
                               'host-os 'win32
                               'host-endianness 'little
                               'target-machine 'x86-nasm
                               'target-os 'win32
                               'target-endianness 'little
                               'mzscheme-source   (pathname-append "Lib" "MzScheme")
                               'globals-table "globals-nasm.cfg"))
  ; (common-unix-initialize)
  (unspecified))

(define configure-system
  (let ((old-conf-sys! configure-system))
    (lambda ()
      (select-compiler 'nasm+msvc)
      (old-conf-sys!)
      (set! win32/petit-lib-library-platform 
	    (if (file-exists? "/usr/lib/libdl.a")  ; not for Cygwin, it doesn't
		'("-lm -ldl")
		'("-lm"))))))

(define (petit-application-name)
  "petit")

(define (twobit-application-name)
  "twobit")

(win32-initialize)

; eof
