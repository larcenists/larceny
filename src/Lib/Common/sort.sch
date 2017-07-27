;;; File   : sort.scm
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;; Updated: 11 June 1991
;
; $Id$
;
; Code originally obtained from Scheme Repository, since hacked.
;
; Sort and Sort! will sort lists and vectors.  The former returns a new
; data structure; the latter sorts the data structure in-place.  A 
; mergesort algorithm is used.

($$trace "sort")

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

(define (sort! seq less?)
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
	 (error "sort!: not a valid sequence: " seq))))

(define (sort seq less?)
  (cond ((null? seq)
	 seq)
	((pair? seq)
	 (sort!! (list-copy seq) less?))
	((vector? seq)
	 (list->vector (sort!! (vector->list seq) less?)))
	(else
	 (error "sort: not a valid sequence: " seq))))

; Added for R6RS.

(define (list-sort less? seq)
  (sort!! (list-copy seq) less?))

; R7RS adds optional start and end arguments.

(define (vector-sort less? seq . rest)
  (cond ((null? rest)
         (list->vector (sort!! (vector->list seq) less?)))
        ((null? (cdr rest))
         (vector-sort less? (vector-copy seq (car rest))))
        (else
         (vector-sort less? (vector-copy seq (car rest) (cadr rest))))))

(define (vector-sort! less? seq . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
         (end   (if (or (null? rest) (null? (cdr rest)))
                    (vector-length seq)
                    (cadr rest)))
         (v (if (null? rest) seq (vector-copy seq start end)))
         (sorted (sort!! (vector->list v) less?)))
    (do ((sorted sorted (cdr sorted))
         (i start (+ i 1)))
        ((null? sorted))
      (vector-set! seq i (car sorted)))))

; eof
