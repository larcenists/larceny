; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Boot-time error procedure.

($$trace "error0")

(define (error . rest)
  ($$trace "ERROR")
  (do ((rest rest (cdr rest)))
      ((null? rest) (sys$exit 1))
    (if (string? (car rest))
	($$trace (car rest)))))

; eof
