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
      

(define (intersection . b)

  (define (simple-intersection a b)
    (let loop ((s '()) (a a))
      (cond ((null? a)
	     s)
	    ((member (car a) b)
	     (loop (cons (car a) s) (cdr a)))
	    (else
	     (loop s (cdr a))))))

  (let loop ((a '()) (b b))
    (if (null? b)
	a
	(loop (simple-intersection a (car b)) (cdr b)))))


(define (difference a b)
  (let loop ((s '()) (a a))
    (cond ((null? a)
	   s)
	  ((member (car a) b)
	   (loop s (cdr a)))
	  (else
	   (loop (cons (car a) s) (cdr a))))))

(define (set-equal? a b)
  (and (null? (difference a b))
       (null? (difference b a))))
