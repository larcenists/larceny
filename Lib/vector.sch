; Copyright 1991 Lightship Software
; Copyright 1995 Lars Thomas Hansen
;
; Larceny library -- vector procedures
;
; $Id: vector.sch,v 1.2 1997/07/18 13:55:49 lth Exp $

($$trace "vector")

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

(define (vector-equal? v1 v2)

  (define (v-equal-loop i)
    (cond ((< i 0) #t)
	  ((equal? (vector-ref v1 i) (vector-ref v2 i))
	   (v-equal-loop (- i 1)))
	  (else #f)))

  (if (= (vector-length v1) (vector-length v2))
      (v-equal-loop (- (vector-length v1) 1))
      #f))
 

; eof
