; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on big-endian Unix
; systems (including MacOS X), under Larceny.
;
; See HOWTO-BUILD and HOWTO-PETIT for documentation.

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
                               'host-endianness 'big
                               'target-machine 'standard-c
                               'target-os 'unix
                               'target-endianness 'big))
  (common-unix-initialize)
  (unspecified))

(define (configure-system)
  (select-compiler 'gcc)
  (let ((os-name (cdr (assq 'os-name (system-features)))))
    (set! unix/petit-lib-library-platform 
	  (cond ((is-macosx?) 
		 (if (dlcompat-available?)
		     '("-ldl")
		     '()))
		((is-sunos?)  '("-lm -ldl"))
		(else         '("-lm -ldl"))))))

(define (petit-application-name)
  "petit")

; classify-unix-system is in Asm/Standard-C/dumpheap-unix.sch

(define (is-macosx?)
  (eq? 'macosx (classify-unix-system)))

(define (dlcompat-available?)
  (file-exists? "/usr/local/include/dlfcn.h"))

(define (is-sunos?)
  (eq? 'sunos (classify-unix-system)))

;; Twobit.app on MacOS X because MacOS X can't distinguish "Twobit"
;; (the directory) and "twobit" (the program).  Unix?  I think not.

(define (twobit-application-name)
  (if (is-macosx?)
      "twobit.app"
      "twobit"))

(unix-initialize)

; eof
