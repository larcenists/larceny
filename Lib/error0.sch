; Lib/error0.sch
; Larceny -- Boot-time error procedure! 
;
; $Id: error0.sch,v 1.1.1.1 1998/11/19 21:52:05 lth Exp $

($$trace "error0")

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (sys$exit 1))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
