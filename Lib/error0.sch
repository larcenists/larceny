; Lib/error0.sch
; Larceny -- Boot-time error procedure! 
;
; $Id$

($$trace "error0")

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (sys$exit 1))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
