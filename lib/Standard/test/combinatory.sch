; Tests for lib/combinatory.sch
; 2004-01-26 / lth

(require 'combinatory)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (= 6 (length (permutations '(1 2 3))))
    (fail 'permuations:1))
(or (every? (lambda (x) (= 3 (length x))) (permutations '(1 2 3)))
    (fail 'permutations:2))
(or (every? (lambda (x) (and (memv 1 x) (memv 2 x) (memv 3 x)))
	    (permutations '(1 2 3)))
    (fail 'permutations:3))
(or (let loop ((xs (permutations '(1 2 3))))
      (if (null? (cdr xs))
	  #t
	  (if (member (car xs) (cdr xs))
	      #f
	      (loop (cdr xs)))))
    (fail 'permutations:4))

(or (= 8 (length (power-set '(1 2 3))))
    (fail 'power-set:1))

(or (= 3 (length (choose '(1 2 3) 2)))
    (fail 'choose:1))
(or (and (member '(1 2) (choose '(1 2 3) 2))
	 (member '(1 3) (choose '(1 2 3) 2))
	 (member '(2 3) (choose '(1 2 3) 2)))
    (fail 'choose:2))

(or (equal? '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
	    (cross-product '(1 2 3) '(a b)))
    (fail 'combinatory:1))
