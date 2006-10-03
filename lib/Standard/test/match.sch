; Tests for lib/match.sch
; 2000-05-21 / lth

(require 'match)
(require 'box)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(define mt:map
  (lambda (f l)
    (match l
      (() '())
      ((x . y) (cons (f x) (mt:map f y))))))

(or (equal? (mt:map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))
    (fail 'match:1))

(or (equal? (match-let (((x y z) (list 1 2 3))) (list z y x)) '(3 2 1))
    (fail 'match:2))

(or (let ((x (list 1 (list 2 3))))
      (match x ((_ (_ (set! setit))) (setit 4)))
      (equal? x '(1 (2 4))))
    (fail 'match:3))

; more here!