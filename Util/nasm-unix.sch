; $Id$
;
; General script for building Larceny on x86 using the NASM backend,
; under Larceny on Unix.

(define nbuild-parameter #f)

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/nbuild-param.sch")
  (load "Util/petit-unix-common.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter 'always-source? #f
                               'verbose-load? #t
                               'development? #t
                               'machine-source "Lib/Standard-C/"
                               'host-os 'unix
                               'host-endianness 'little
                               'target-machine 'x86-nasm
                               'target-os 'unix
                               'target-endianness 'little
			       'globals-table "globals-nasm.cfg"))
  (common-unix-initialize)
  (unspecified))

(define (configure-system)
  (select-compiler 'nasm+gcc)
  (set! unix/petit-lib-library-platform 
	(if (file-exists? "/usr/lib/libdl.a")  ; not for Cygwin, it doesn't
	    '("-lm -ldl")
	    '("-lm"))))

(define (petit-application-name)
  "petit")

(define (twobit-application-name)
  "twobit")

(unix-initialize)

; eof
