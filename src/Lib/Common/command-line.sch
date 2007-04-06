; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- command line arguments.
;
; The argument vector is a vector of strings, one for each command line 
; argument.  What constitutes an argument is up to the user's shell or
; the C language startup.  In any case, however, the runtime system
; will provide a vector of bytevector representations of those strings,
; and the translation into Scheme strings is done here.

($$trace "argv")

(define *argv* '#())

(define (command-line-arguments . rest)
  (cond ((null? rest)
	 *argv*)
	((null? (cdr rest))
	 (let* ((old *argv*)
                (new (car rest))
                (n (vector-length new)))
           (do ((i 0 (+ i 1)))
               ((= i n))
             (let ((arg (vector-ref new i)))
               (if (bytevector? arg)
                   (vector-set! new i (sys$cstring->string arg)))))
	   (set! *argv* new)
	   old))
	(else
	 (error "Too many arguments to procedure command-line-arguments.")
	 #t)))

; eof
