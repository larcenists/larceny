; Copyright Lightship Software, Incorporated.
; 
; $Id$
;
; Larceny library -- general predicates.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (boolean=? x y . rest)
  (cond ((or (not (boolean? x)) (not (boolean? y)))
         (assertion-violation 'boolean=? "illegal arguments" x y))
        ((null? rest)
         (eq? x y))
        (else
         (and (eq? x y) (apply boolean=? y rest)))))

(define (symbol=? x y . rest)
  (cond ((or (not (symbol? x)) (not (symbol? y)))
         (assertion-violation 'symbol=? "illegal arguments" x y))
        ((null? rest)
         (eq? x y))
        (else
         (and (eq? x y) (apply symbol=? y rest)))))
 
; eof
