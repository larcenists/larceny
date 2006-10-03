; Various combinatory functions.
; Preliminary; these cons quite a bit.
;
; 2002-03-28 / lth

(require 'list)

(define (permutations xs)
  (if (null? xs)
      '(())
      (mappend (lambda (x)
		 (map (lambda (y) (cons x y))
		      (permutations (remove x xs))))
	       xs)))

(define (power-set xs)
  (let ((len (length xs)))
    (let loop ((i 1) (ys '(())))
      (if (> i len)
	  ys
	  (loop (+ i 1) (append (choose xs i) ys))))))

(define (choose xs n)

  (define (choose xs n k)
    (if (zero? n)
	'(())
	(let ((a (map (lambda (y) (cons (car xs) y))
		      (choose (cdr xs) (- n 1) (- k 1)))))
	  (if (<= k n)
	      a
	      (append a (choose (cdr xs) n (- k 1)))))))

  (choose xs n (length xs)))

(define (cross-product . lists)
  (cond ((null? lists) '())
	((null? (cdr lists)) 
	 (map list (car lists)))
	((null? (cddr lists))
	 (let loop1 ((l1 (car lists)) (r '()))
	   (if (null? l1)
	       (reverse r)
	       (let loop2 ((l2 (cadr lists)) (r r))
		 (if (null? l2)
		     (loop1 (cdr l1) r)
		     (loop2 (cdr l2) (cons (list (car l1) (car l2)) r)))))))
	(else
	 (let ((cp (apply cross-product (cdr lists))))
	   (apply append
		  (map (lambda (x) 
			 (map (lambda (c) (cons x c)) cp))
		       (car lists)))))))

; eof
