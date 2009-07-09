; Test suite for SRFI-5
; 2003-12-29 / lth
;
; Taken from the SRFI document and lightly modified.

(import (except (rnrs base) let)
        (rnrs io simple)
        (srfi :5 let))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? 55 (let (fibonacci (n 10) (i 0) (f0 0) (f1 1))
		 (if (= i n)
		     f0
		     (fibonacci n (+ i 1) f1 (+ f0 f1)))))
    (fail 'fibonacci:1))

(or (equal? '((5) (4 5) (3 4 5))
	    (let (blast (res '()) . (x (+ 1 2) 4 5))
	      (if (null? x)
		  res
		  (apply blast (cons x res) (cdr x)))))
    (fail 'blast:1))

(writeln "Done.")

