; Lib/sysparam.sch
; Larceny library -- system parameter abstraction
;
; $Id$

($$trace "sysparam")

(define (system-parameter name value)
  (lambda rest
    (cond ((null? rest)
	   value)
	  ((null? (cdr rest))
	   (set! value (car rest))
	   value)
	  (else
	   (error name ": too many arguments.")
	   #t))))

(define (system-features)
  (list (cons 'architecture     (sys$system-feature 'architecture))
	(cons 'operating-system (sys$system-feature 'os-name))
	(cons 'os-major-version (sys$system-feature 'os-major-version))
	(cons 'os-minor-version (sys$system-feature 'os-minor-version))
	(cons 'gc-technology    (sys$system-feature 'gc-technology))))

; eof
