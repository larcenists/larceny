; Dummy procedures for generic arithmetic until the real ones get implemented.
;
; $Id: millicode-support-dummies.sch,v 1.1 92/02/10 03:16:00 lth Exp Locker: lth $
;
; These should move into millicode-support.sch whenever they are written.

(define (fixnum2ratnum-div a b) (error "Fixnum2ratnum-div not implemented"))
(define (heavy-quotient a b) (error "Heavy-quotient not implemented"))
(define (heavy-remainder a b) (error "Heavy-quotient not implemented"))
(define (heavy-modulo a b) (error "Heavy-quotient not implemented"))

