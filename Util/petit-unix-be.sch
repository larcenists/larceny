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

;; Twobit.app on MacOS X because MacOS X can't distinguish "Twobit"
;; (the directory) and "twobit" (the program).  Unix?  I think not.

(define (twobit-application-name)
  (if (string=? "MacOS X" (cdr (assq 'os-name (system-features))))
      "twobit.app"
      "twobit"))

(define (configure-system)
  (let ((os-name (cdr (assq 'os-name (system-features)))))
    (set! unix/petit-lib-library-platform 
	  (cond ((string=? os-name "MacOS X") '())
		((string=? os-name "SunOS")   '("-lm -ldl"))
		(else                         '("-lm -ldl"))))))

(unix-initialize)

; eof
