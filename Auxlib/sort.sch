; Larceny auxiliary library -- sorting
; $Id: sort.sch,v 1.1 1997/02/27 16:48:16 lth Exp lth $
;
; Sort and Sort! will sort lists and vectors. The former returns a new
; data structure; the latter sorts the data structure in-place. A mergesort
; algorithm is used.
;
; Based on code written by Richard O'Keefe, available from the Internet
; Scheme Repository (ftp://ftp.cs.indiana.edu/pub/scheme-repository).
; 
; Modified by Lars Thomas Hansen and Will Clinger.
;
; FIXME: It would be prudent to sort vectors in-place using a shell sort
; or quicksort, as this can reduce the space consumption considerably.
;
; FIXME: Would be reasonable to allow strings and bytevectors to be sorted.

(define sort)
(define sort!)

(let ()

  ; Destructive merge of two sorted lists.

  (define (merge!! a b less?)

    (define (loop r a b)
      (if (less? (car b) (car a))
	  (begin (set-cdr! r b)
		 (if (null? (cdr b))
		     (set-cdr! b a)
		     (loop b a (cdr b)) ))
	  ;; (car a) <= (car b)
	  (begin (set-cdr! r a)
		 (if (null? (cdr a))
		     (set-cdr! a b)
		     (loop a (cdr a) b)) )) )

    (cond ((null? a) b)
	  ((null? b) a)
	  ((less? (car b) (car a))
	   (if (null? (cdr b))
	       (set-cdr! b a)
	       (loop b a (cdr b)))
	   b)
	  (else				; (car a) <= (car b)
	   (if (null? (cdr a))
	       (set-cdr! a b)
	       (loop a (cdr a) b))
	   a)))


  ; Sort procedure which copies the input list and then sorts the
  ; new list imperatively. Due to Richard O'Keefe; algorithm
  ; attributed to D.H.D. Warren

  (define (sort!! seq less?)
  
    (define (step n)
      (cond ((> n 2)
	     (let* ((j (quotient n 2))
		    (a (step j))
		    (k (- n j))
		    (b (step k)))
	       (merge!! a b less?)))
	    ((= n 2)
	     (let ((x (car seq))
		   (y (cadr seq))
		   (p seq))
	       (set! seq (cddr seq))
	       (if (less? y x)
		   (begin
		     (set-car! p y)
		     (set-car! (cdr p) x)))
	       (set-cdr! (cdr p) '())
	       p))
	    ((= n 1)
	     (let ((p seq))
	       (set! seq (cdr seq))
	       (set-cdr! p '())
	       p))
	    (else
	     '())))
  
    (step (length seq)))

  (set! sort! (lambda (seq less?)
		(cond ((null? seq)
		       seq)
		      ((pair? seq)
		       (sort!! seq less?))
		      ((vector? seq)
		       (do ((l (sort!! (vector->list seq) less?) (cdr l))
			    (i 0 (+ i 1)))
			   ((null? l) seq)
			 (vector-set! seq i (car l))))
		      (else
		       (error "sort!: not a valid sequence: " seq)))))

  (set! sort (lambda (seq less?)
	       (cond ((null? seq)
		      seq)
		     ((pair? seq)
		      (sort!! (list-copy seq) less?))
		     ((vector? seq)
		      (list->vector (sort!! (vector->list seq) less?)))
		     (else
		      (error "sort: not a valid sequence: " seq)))))
  'sort)

; eof
