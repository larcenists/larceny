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

; eof
