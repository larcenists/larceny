; Test code for lib/box.sch
; 2000-05-21 / lth

(require 'box)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (box? (box 1)) 
    (fail 'box:1))
(or (equal? 1 (unbox (box 1)))
    (fail 'box:2))
(or (let ((b (box 1)))
      (set-box! b 2)
      (equal? 2 (unbox b)))
    (fail 'box:3))
(or (equal? (box '(a b c)) (box '(a b c)))
    (fail 'box:4))
(or (box? '#&2)
    (fail 'box:5))    
(or (equal? (box 1) '#&1)
    (fail 'box:6))

; eof

