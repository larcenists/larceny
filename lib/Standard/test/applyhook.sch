(require 'apply-hook)

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define x (make-apply-hook (lambda (x) (+ x 10)) 'abracadabra))

(or (procedure? x)
    (fail 'apply-hook:1))
(or (apply-hook? x)
    (fail 'apply-hook:2))
(or (= 20 (x 10))
    (fail 'apply-hook:3))
(or (eq? 'abracadabra (apply-hook-extra x))
    (fail 'apply-hook:4))
(or (procedure? (apply-hook-procedure x))
    (fail 'apply-hook:5))
(or (= 20 ((apply-hook-procedure x) 10))
    (fail 'apply-hook:6))

(let ((foo 10))
  (set-apply-hook-procedure! x (lambda (x y) (+ x y foo))))

(set-apply-hook-extra! x 'hocuspocus)

(or (procedure? (apply-hook-procedure x))
    (fail 'apply-hook:7))
(or (= 49 (x 20 19))
    (fail 'apply-hook:8))

(writeln "Done.")
