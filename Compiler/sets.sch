; Standard operations on finite sets.
;
; $Id: sets.sch,v 1.2 92/02/10 03:36:55 lth Exp $
;
; Definition
;   Sets are known to be represented as lists, and the empty set is 
;   represented as the empty list, but nothing else is know.
;
; Operations
;   union
;   intersection
;   difference
;   adjoin
;   set-equal?
;
; Implementation
;   The lists are kept unsorted; the implementation is trivial.

; (union set ...)  -->  set

(define (union . b)

  (define (simple-union a b)
    (let loop ((s b) (a a))
      (cond ((null? a)
	     s)
	    ((not (member (car a) b))
	     (loop (cons (car a) s) (cdr a)))
	    (else
	     (loop s (cdr a))))))

  (let loop ((a '()) (b b))
    (if (null? b)
	a
	(loop (simple-union a (car b)) (cdr b)))))
      

; (intersection set ...)  -->  set

(define (intersection . b)

  (define (simple-intersection a b)
    (let loop ((s '()) (a a))
      (cond ((null? a)
	     s)
	    ((member (car a) b)
	     (loop (cons (car a) s) (cdr a)))
	    (else
	     (loop s (cdr a))))))

  (if (null? b)
      '()
      (let loop ((a (car b)) (b (cdr b)))
	(if (null? b)
	    a
	    (loop (simple-intersection a (car b)) (cdr b))))))


; (difference set set)  -->  set

(define (difference a b)
  (let loop ((s '()) (a a))
    (cond ((null? a)
	   s)
	  ((member (car a) b)
	   (loop s (cdr a)))
	  (else
	   (loop (cons (car a) s) (cdr a))))))

; (adjoin obj set)   -->  set

(define (adjoin a b)
  (if (member a b)
      b
      (cons a b)))

; (set-equal? set set)   -->  boolean

(define (set-equal? a b)
  (and (null? (difference a b))
       (null? (difference b a))))

; eof
