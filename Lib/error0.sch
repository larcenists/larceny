; Lib/error0.sch
; Larceny -- Boot-time error procedure! 
;
; $Id: error0.sch,v 1.3 1997/07/18 13:55:49 lth Exp $

($$trace "error0")

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (sys$exit 1))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
