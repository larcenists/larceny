; Dummy procedures for generic arithmetic until the real ones get implemented.
;
; $Id$

(define (rectnum-add a b) (error "Rectnum-add not implemented"))
(define (rectnum-sub a b) (error "Rectnum-sub not implemented"))
(define (rectnum-mul a b) (error "Rectnum-mul not implemented"))
(define (rectnum-div a b) (error "Rectnum-div not implemented"))
(define (rectnum-neg a) (error "Rectnum-neg not implemented"))
(define (rectnum=? a b) (error "Rectnum=? not implemented"))

(define (fixnum2ratnum-div a b) (error "Fixnum2ratnum-div not implemented"))
(define (heavy-quotient a b) (error "Heavy-quotient not implemented"))
(define (heavy-remainder a b) (error "Heavy-quotient not implemented"))
(define (heavy-modulo a b) (error "Heavy-quotient not implemented"))
(define (generic-make-rectangular a b) 
  (error "Generic-make-rectangular not implemented"))

(define (contagion a b op)
  (error "Contagion not implemented."))

(define (pcontagion a b op)
  (error "Pcontagion not implemented."))

(define (econtagion a b op)
  (error "Econtagion not implemented."))


