; Dummy procedures for generic arithmetic until the real ones get implemented.
;
; $Id: millicode-support-dummies.sch,v 1.2 92/02/17 18:27:04 lth Exp Locker: lth $
;
; These should move into millicode-support.sch whenever they are written.

(define (fixnum2ratnum-div a b) (error "Fixnum2ratnum-div not implemented"))
(define (heavy-modulo a b) (error "Heavy-quotient not implemented"))

