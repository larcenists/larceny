; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Compiler switches used by Twobit's macro expander, redefined here
; for when the expander is used by the interpreter.  This file is loaded
; before Twobit's macro expander.

($$trace "switches")

(define issue-warnings
  (let ((flag #f))        ; Off during bootstrapping to avoid annoying msgs in Petit Larceny
    (lambda rest
      (if (not (null? rest))
	  (set! flag #t))
      flag)))
(define (include-source-code) #t)
(define (include-procedure-names) #t)
(define (include-variable-names) #t)
(define (integrate-procedures . ignored) 'none)
(define (benchmark-mode) #f)
(define (constant-folding-entry x) #f)

; eof
