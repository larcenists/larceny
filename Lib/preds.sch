; Lib/preds.sch
; Larceny library -- general predicates
;
; $Id$
;
; In part based on code from MacScheme:
;    Copyright Lightship Software.

($$trace "preds")
 
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
	((and (structure? x) (structure? y))
	 ((structure-comparator) x y))
	(else #f)))
 
; eof
