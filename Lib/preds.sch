; Copyright Lightship Software.
;
; Larceny library -- general predicates
;
; $Id: preds.sch,v 1.2 1997/02/03 20:07:13 lth Exp $
;
; Modified 950802 / lth:
;    Moved string-equal? to strings.sch
;    Moved vector-equal? to vector.sch
;    Moved all list-based predicates to list.sch
;
; Modified 950711 / lth:
;    Added reverse!, append!.
;
; Modified 950611 / lth:
;    Memq now uses eq? (rather than eqv?).
;
; Modified 950528 / lth: 
;    Added IEEE-compliant list?, and last-pair.
;
; Modified 15 March by Will Clinger:
;    added list-tail, list-ref
 
(define (atom? x) (not (pair? x)))

(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

(define (equal? x y)
  (cond ((eqv? x y) #t)
	((and (pair? x) (pair? y))
	 (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
	((and (string? x) (string? y))
	 (string=? x y))
	((and (vector? x) (vector? y))
	 (vector-equal? x y))
	((and (bytevector? x) (bytevector? y))
	 (bytevector-equal? x y))
	(else #f)))
 
; eof
