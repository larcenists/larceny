; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- command line arguments.
;
; The argument vector is a vector of strings, one for each command line 
; argument.  What constitutes an argument is up to the user's shell or
; the C language startup.

($$trace "argv")

(define *argv* '#())

(define (command-line-arguments . rest)
  (cond ((null? rest)
	 *argv*)
	((null? (cdr rest))
	 (let ((old *argv*))
	   (set! *argv* (car rest))
	   old))
	(else
	 (error "Too many arguments to procedure command-line-arguments.")
	 #t)))

; eof
