; Lib/error0.sch
; Larceny -- Boot-time error procedure! 
;
; $Id: error0.sch,v 1.2 1997/07/07 20:45:06 lth Exp $

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (sys$exit 1))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
