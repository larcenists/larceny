; Test code for lib/brackets.sch
; 2000-05-25 / lth

(require 'brackets)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (equal? (read (open-input-string "[1 2 3]")) '(1 2 3))
    (fail 'brackets:1))

(or (let ([a 1]
          [b 2])
      (equal? (+ a b) 3))
    (fail 'brackets:2))

; eof
