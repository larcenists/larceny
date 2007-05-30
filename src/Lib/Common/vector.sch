; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library -- vector procedures.

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
;   (sys$partial-list->vector l (length l))
    (let* ((n (length l))
           (v (make-vector n)))
      (do ((i 0 (+ i 1))
           (l l (cdr l)))
          ((= i n) v)
        (vector-set! v i (car l))))))

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
 
(define (vector-copy vec)
  (let* ((l (vector-length vec))
         (v (make-vector l #f)))
    (do ((i 0 (+ i 1)))
        ((= i l) v)
      (vector-set! v i (vector-ref vec i)))))

(define (procedure-copy proc)
  (let* ((l (procedure-length proc))
         (p (make-procedure l)))
    (do ((i 0 (+ i 1)))
        ((= i l) p)
      (procedure-set! p i (procedure-ref proc i)))))
  

; eof
