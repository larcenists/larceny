; More list procedures, but less general or experimental, hence not in
; list.sch (yet).

; Given a two-arg collector procedure, a list, and an equality
; predicate, return a list of the results of applying the collector
; procedure to x and n where x is an element from l and n is the number
; of elements y following x s.t. (same? x y) [inclusive x].  Eg for l =
; (a a b c c), same = "eq?", collector would be called with (a 2), 
; (b 1), and (c 2).

(define (collect-duplicates collector l same?)
  (if (null? l)
      '()
      (let ((x (car l)))
	(let loop ((n 1) (l l))
	  (cond ((null? (cdr l))
		 (list (collector x n)))
		((not (same? (cadr l) x))
		 (cons (collector x n)
		       (collect-duplicates collector (cdr l) same?)))
		(else
		 (loop (+ n 1) (cdr l))))))))

; Apply p to each pair of neighboring elements in l, resulting in
; a list of length |l|-1.

(define (map-pairwise p l)
  (cond ((null? l) l)
	((null? (cdr l)) '())
	(else (cons (p (car l) (cadr l))
		    (map-pairwise p (cdr l))))))

; Assuming l is sorted, returns list of pairs (x . n) where x is an
; element and n is the number of consecutive occurrences.

(define (count-runs l . rest)
  (let ((same? (if (null? rest) equal? (car rest))))
    (collect-duplicates cons l same?)))

; If l is sorted, returns sorted list with duplicates removed.

(define (uniq l . rest)
  (let ((same? (if (null? rest) equal? (car rest))))
    (collect-duplicates (lambda (a b) a) l same?)))

; Arithmetic mean of list of numbers.

(define (average l)
  (if (null? l)
      (error "Average: empty list.")
      (/ (reduce + 0 l) (length l))))

(define (list-head l n)
  (if (zero? n)
      '()
      (cons (car l) (list-head (cdr l) (- n 1)))))

