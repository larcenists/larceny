; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on little-endian Unix
; systems (including Cygwin), under Larceny.

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/Configurations/nbuild-param-C-el-unix.sch")
  (load "Util/petit-unix-common.sch")
  (common-unix-initialize)
  (unspecified))

(define (petit-application-name)
  "petit")

(define (twobit-application-name)
  "twobit")

(define (configure-system)
  ; This var is picked up by Asm/Standard-C/dumpheap-unix.sch
  (set! unix/petit-lib-library-platform 
	(list "/usr/lib/libm.a"))
  (if (file-exists? "/usr/lib/libdl.a")  ; not for Cygwin, it doesn't
      (set! unix/petit-lib-library-platform 
	    (append unix/petit-lib-library-platform
                    (list "/usr/lib/libdl.a")))))

(unix-initialize)

; eof
