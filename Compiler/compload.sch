; $Id$
;
; For Larceny:
; Load the compiler and generic assembler, with everything needed.
; If a path is given, it is used. Otherwise, "." is assumed.

(define (compload which . rest)
  (let ((path  (if (null? rest) "" (car rest)))
	(cfiles '("sets.fasl" "pass1.aux.fasl" "switches.fasl"
	          "twobit.imp.fasl" "pass1.fasl" "pass2.aux.fasl"
		  "pass2p1.fasl" "pass2p2.fasl" "pass4.aux.fasl"
		  "pass4p1.fasl" "pass4p2.fasl" "pass4p3.fasl"
		  "compile313.fasl" "makefasl.fasl"))
	(afiles '("assembler.fasl" "peepopt.fasl")))

    (define (do-load fn)
      (let ((fn (string-append path fn)))
	(display "; Loading ") (display fn) (newline)
	(load fn)))

    (cond ((eq? which 'compiler)
	   (for-each do-load cfiles))
	  ((eq? which 'assembler)
	   (for-each do-load afiles))
	  (else
	   (error "compload: illegal: " which)))))

; eof


