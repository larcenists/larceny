; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; The interpreter's top-level environment -- Standard-C additions

($$trace "toplevel-standard-c")

(define (initialize-null-environment-target-specific null) null)
(define (initialize-r4rs-environment-target-specific r4rs) r4rs)
(define (initialize-r5rs-environment-target-specific r5rs) r5rs)
(define (initialize-larceny-environment-target-specific larc) larc)

; eof
