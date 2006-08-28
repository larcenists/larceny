; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny run-time system -- interface to system functions
;
; Simple hack that makes low-level system functions available interactively
; in a controlled manner.

($$trace "syshooks")

(define (system-function name)
  (case name
    ((sys$get-resource-usage) sys$get-resource-usage)
    ((sys$gc) sys$gc)
    ((sys$codevector-iflush) sys$codevector-iflush)
    ((sys$tracectl) sys$tracectl)
    ((sys$trace) sys$trace)
    ((io/read-char) io/read-char)
    ((syscall) syscall)
    ((iflush) sys$codevector-iflush)
    (else 
     (error "system-function: " name " is not a function I know about.")
     #t)))

; eof
