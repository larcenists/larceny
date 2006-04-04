; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on little-endian Unix
; systems (including Cygwin), under Larceny.

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/nbuild-param.sch")
  (load "Util/petit-unix-common.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter 'always-source? #f
                               'verbose-load? #t
                               'development? #t
                               'machine-source "Lib/Standard-C/"
                               'mzscheme-source "Lib/MzScheme/"
                               'host-os 'unix
                               'host-endianness 'little
                               'target-machine 'standard-c
                               'target-os 'unix
                               'target-endianness 'little))
  (common-unix-initialize)
  (unspecified))

(define (configure-system)
  (select-compiler 'gcc)
  (set! unix/petit-lib-library-platform 
	(if (file-exists? "/usr/lib/libdl.a")  ; not for Cygwin, it doesn't
	    '("-lm -ldl")
	    '("-lm"))))

(define (petit-application-name)
  "petit.bin")

(define (twobit-application-name)
  "twobit.bin")

(unix-initialize)

; eof
