; $Id: auxload.sch,v 1.1 1997/02/27 16:48:16 lth Exp lth $
;
; This is a simple script which loads into larceny all the files in the
; Auxlib directory. The files must be compiled to FASL code; this is not
; checked.

(define (auxload . rest)
  (let ((path (if (null? rest) "" (car rest))))

    (define (do-load fn)
      (let ((fn (string-append path fn)))
	(display "; Loading ") (display fn) (newline)
	(load fn)))
  
    (do-load "misc.fasl")		; Useful procedures
    (do-load "sort.fasl")		; Sort procedurs
    (do-load "pp.fasl")		        ; Pretty printer
    #t))

; eof
