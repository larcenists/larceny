; Larceny -- Sparc assembler load utility.
;
; lth@cs.uoregon.edu / August 22, 1995
; $Id: asmload.sch,v 1.1 1997/05/15 00:54:42 lth Exp $
;
; For Larceny:
; Load the Sparc assembler.
; If a path is given, it is used. Otherwise, "." is assumed.

(define (asmload . rest)
  (let ((path  (if (null? rest) "" (car rest)))
	(files '("gen-msi.fasl" "gen-prim.fasl" "sparcasm.fasl"
		 "switches.fasl" "asmutil.fasl"
		 "sparcdis.fasl"
		 )))

    (define (do-load fn)
      (let ((fn (string-append path fn)))
	(display "; Loading ") (display fn) (newline)
	(load fn)))

    (for-each do-load files)))

; eof
