; Test suite for SRFI-9
; 2004-01-01 / lth
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (rnrs exceptions)
        (srfi :9 records))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define-record-type :pare
  (kons x y) 
  pare?
  (x kar set-kar!)
  (z mid set-mid!)
  (y kdr))

(or (equal? #t (pare? (kons 1 2)))
    (fail 'predicate:1))
(or (equal? #f (pare? (cons 1 2)))
    (fail 'predicate:2))
(or (equal? 1 (kar (kons 1 2)))
    (fail 'accessor:1))
(or (equal? 2 (kdr (kons 1 2)))
    (fail 'accessor:2))
(let ((x (kons 1 2)))
  (set-mid! x 37)
  (or (and (equal? 1 (kar x))
	   (equal? 37 (mid x))
	   (equal? 2 (kdr x)))
      (fail 'field-order)))
(or (equal? 3 (let ((k (kons 1 2)))
		(set-kar! k 3)
		(kar k)))
    (fail 'mutator:1))

(writeln "Done.")
