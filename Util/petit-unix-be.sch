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
  (load "Util/Configurations/nbuild-param-C-be-unix.sch")
  (load "Util/petit-unix-common.sch")
  (common-unix-initialize)
  (unspecified))

(define (petit-application-name)
  "petit")

; A hack.

(define is-macosx?
  (let ((isit 'dontknow))
    (lambda ()
      (if (eq? isit 'dontknow)
	  (set! isit (and (string=? "BSD Unix" (cdr (assq 'os-name (system-features))))
			  (file-exists? "/Desktop"))))
      isit)))

(define (dlcompat-available?)
  (file-exists? "/usr/local/include/dlfcn.h"))

(define (is-sunos?)
  (string=? "SunOS" (cdr (assq 'os-name (system-features)))))

;; Twobit.app on MacOS X because MacOS X can't distinguish "Twobit"
;; (the directory) and "twobit" (the program).  Unix?  I think not.

(define (twobit-application-name)
  (if (is-macosx?)
      "twobit.app"
      "twobit"))

(define (configure-system)
  (let ((os-name (cdr (assq 'os-name (system-features)))))
    (set! unix/petit-lib-library-platform 
	  (cond ((is-macosx?) 
		 (if (dlcompat-available?)
		     '("-ldl")
		     '()))
		((is-sunos?)  '("-lm -ldl"))
		(else         '("-lm -ldl"))))))

(unix-initialize)

; eof
