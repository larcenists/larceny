; $Id$
; FIXME: this "test" does not yet check its result.

(define (reverse-test)
  (reverse-benchmark1 rev)
  (reverse-benchmark1 rev2)
  (reverse-benchmark1 rev!))

(define (reverse-benchmark1 rev)
  (let ((x (vector->list (make-vector 100 #f))))
    (do ((i 0 (+ i 1))
	 (x x (rev x)))
	((= i 100) #t))))

(define (rev l)
  (define (r l h)
    (if (null? l)
        h
        (r (cdr l) (cons (car l) h))))
  (r l '()))

(define (rev2 l)
  (define (r l h)
    (if (null? l)
	h
	(let ((l (cdr l)) (h (cons (car l) h)))
	  (if (null? l)
	      h
	      (let ((l (cdr l)) (h (cons (car l) h)))
		(if (null? l)
		    h
		    (let ((l (cdr l)) (h (cons (car l) h)))
		      (if (null? l)
			  h
			  (r (cdr l) (cons (car l) h))))))))))
  (r l '()))

(define (rev! l)
  (define (loop prev curr)
    (if (null? curr)
        prev
        (let ((next (cdr curr)))
          (set-cdr! curr prev)
          (loop curr next))))
  (loop '() l))


