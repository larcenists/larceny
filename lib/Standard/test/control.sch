; Tests for lib/control.sch
; 2000-05-16 / lth

(require 'control)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (eq? (identity '()) '()) (fail 'identity:1))
(or (let ((k '(foo)))
      (eq? (identity k) k))
    (fail 'identity:2))

(or (equal? (call-with-accumulator
             (lambda (acc)
               #t))
            '())
    (fail 'call-with-accumulator:1))
(or (equal? (call-with-accumulator
             (lambda (acc)
               (acc 1)
               (acc 2)
               (acc 3)))
            '(1 2 3))
    (fail 'call-with-accumulator:2))


(or (eqv? ((compose inexact->exact string->number) "77.5") 155/2)
    (fail 'compose:1))
