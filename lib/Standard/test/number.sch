; Tests for lib/number.sch
; 2000-05-18 / lth

(require 'number)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (= -1 (sign -1)) (fail 'sign:1))
(or (= -1 (sign -3.5)) (fail 'sign:2))
(or (= 1 (sign 1)) (fail 'sign:3))
(or (= 1 (sign 3.14159)) (fail 'sign:4))
(or (= 0 (sign 0)) (fail 'sign:5))
(or (= 0 (sign -0.0)) (fial 'sign:6))

; eof
