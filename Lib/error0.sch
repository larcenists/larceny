; Lib/error0.sch
; Larceny -- Boot-time error procedure! 
;
; $Id: error0.sch,v 1.1 1997/03/02 17:26:15 lth Exp $

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (exit))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
