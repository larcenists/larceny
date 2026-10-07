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

; Stable merge sort of the elements of vector v from lo (inclusive) to
; hi (exclusive), in place.  tmp is a vector of at least (hi-lo+1)/2
; elements: to merge two sorted halves, the left half is copied into tmp
; and merged with the right half back into v.  Runs of at most 16
; elements are sorted by insertion, and halves already in order are not
; merged.

(define (vector-sort:merge-sort! v tmp lo hi less?)

  (define (insertion-sort! lo hi)
    (do ((i (+ lo 1) (+ i 1)))
        ((>= i hi))
      (let ((x (vector-ref v i)))
        (let loop ((j i))
          (if (and (> j lo) (less? x (vector-ref v (- j 1))))
              (begin (vector-set! v j (vector-ref v (- j 1)))
                     (loop (- j 1)))
              (vector-set! v j x))))))

  ; Merges v[lo,mid) and v[mid,hi), both sorted, into v[lo,hi).

  (define (merge! lo mid hi)
    (let ((n (- mid lo)))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (vector-set! tmp i (vector-ref v (+ lo i))))
      (let loop ((i 0) (j mid) (k lo))
        (cond ((= i n))                  ; the rest of the right half stays
              ((= j hi)
               (do ((i i (+ i 1))
                    (k k (+ k 1)))
                   ((= i n))
                 (vector-set! v k (vector-ref tmp i))))
              ((less? (vector-ref v j) (vector-ref tmp i))
               (vector-set! v k (vector-ref v j))
               (loop i (+ j 1) (+ k 1)))
              (else
               (vector-set! v k (vector-ref tmp i))
               (loop (+ i 1) j (+ k 1)))))))

  (define (sort! lo hi)
    (if (<= (- hi lo) 16)
        (insertion-sort! lo hi)
        (let ((mid (+ lo (quotient (- hi lo) 2))))
          (sort! lo mid)
          (sort! mid hi)
          (if (less? (vector-ref v mid) (vector-ref v (- mid 1)))
              (merge! lo mid hi)))))

  (sort! lo hi))

; Sorts the elements of v from start to end in place.  A strictly
; descending range is just reversed (which keeps the sort stable, as no
; two elements are equal); the test stops at the first pair in order.

(define (vector-sort:sort-range! less? v start end)

  (define (descending? i)
    (or (>= (+ i 1) end)
        (and (less? (vector-ref v (+ i 1)) (vector-ref v i))
             (descending? (+ i 1)))))

  (define (reverse! i j)
    (if (< i j)
        (let ((x (vector-ref v i)))
          (vector-set! v i (vector-ref v j))
          (vector-set! v j x)
          (reverse! (+ i 1) (- j 1)))))

  (let ((n (- end start)))
    (cond ((<= n 1))
          ((descending? start)
           (reverse! start (- end 1)))
          (else
           (vector-sort:merge-sort! v (make-vector (quotient (+ n 1) 2))
                                    start end less?)))))

(define (sort! seq less?)
  (cond ((null? seq)
	 seq)
	((pair? seq)
	 (sort!! seq less?))
	((vector? seq)
	 (vector-sort:sort-range! less? seq 0 (vector-length seq))
	 seq)
	(else
	 (error "sort!: not a valid sequence: " seq))))

(define (sort seq less?)
  (cond ((null? seq)
	 seq)
	((pair? seq)
	 (sort!! (list-copy seq) less?))
	((vector? seq)
	 (let ((v (vector-copy seq)))
	   (vector-sort:sort-range! less? v 0 (vector-length v))
	   v))
	(else
	 (error "sort: not a valid sequence: " seq))))

; Added for R6RS.

(define (list-sort less? seq)
  (sort!! (list-copy seq) less?))

; R7RS adds optional start and end arguments.

(define vector-sort
  (case-lambda
   ((less? seq) (vector-sort:copy less? (vector-copy seq)))
   ((less? seq start) (vector-sort:copy less? (vector-copy seq start)))
   ((less? seq start end) (vector-sort:copy less? (vector-copy seq start end)))))

(define (vector-sort:copy less? v)
  (vector-sort:sort-range! less? v 0 (vector-length v))
  v)

(define vector-sort!
  (case-lambda
   ((less? seq)
    (vector-sort!:range less? seq 0 (if (vector? seq) (vector-length seq) 0)))
   ((less? seq start)
    (vector-sort!:range less? seq start
                        (if (vector? seq) (vector-length seq) 0)))
   ((less? seq start end)
    (vector-sort!:range less? seq start end))))

(define (vector-sort!:range less? seq start end)
  (if (not (and (vector? seq)
                (fixnum? start)
                (fixnum? end)
                (<= 0 start end (vector-length seq))))
      (assertion-violation 'vector-sort! "illegal arguments" seq start end))
  (vector-sort:sort-range! less? seq start end))

; eof
