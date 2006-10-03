; Tests for lib/io.sch
; 2002-03-18 / lth

(require 'io)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(let* ((s "this is a string\nand another \nand finally")
       (p (open-input-string s)))
  (or (equal? (read-line p) "this is a string")
      (fail 'read-line:1))
  (or (equal? (read-line p) "and another ")
      (fail 'read-line:2))
  (or (equal? (with-input-from-port p read-line) "and finally")
      (fail 'read-line:3)))
