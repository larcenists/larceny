; Copyright 1991 Lightship Software
; Copyright 1995 Lars Thomas Hansen
;
; Larceny library -- vector procedures
;
; $Id: vector.sch,v 1.1 1995/08/03 00:18:21 lth Exp lth $

(define vector
  (lambda l
    (list->vector l)))

; List->vector is a partial primop in larceny for the purposes of the
; thesis. The theory is that we can initialize the vector much more
; efficiently if we don't have to check for intergenerational pointers
; all the time, and we can do this when we know that the vector is in
; the tenured area.

(define list->vector
  (lambda (l)
    (sys$partial-list->vector l (length l))))

(define %list->vector list->vector)

(define vector->list
  (letrec ((loop
             (lambda (v i l)
               (if (< i 0)
                   l
                   (loop v (- i 1) (cons (vector-ref v i) l))))))
    (lambda (v)
      (loop v (- (vector-length v) 1) '()))))

(define (vector-fill! v o)
  (define (loop k)
    (if (< k 0)
	#t
	(begin (vector-set! v k o)
	       (loop (- k 1)))))
  (loop (- (vector-length v) 1)))

(define vector-equal?
  (letrec ((v-equal-loop
	    (lambda (v1 v2 i)
	      (cond ((<? i 0) #t)
		    ((equal? (vector-ref v1 i) (vector-ref v2 i))
		     (v-equal-loop v1 v2 (1- i)))
		    (else #f)))))
    (lambda (v1 v2)
      (if (=? (vector-length v1) (vector-length v2))
          (v-equal-loop v1 v2 (1- (vector-length v1)))
          #f))))
 

; eof
