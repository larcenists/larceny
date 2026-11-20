(define-library (t a)
  (export make-a a-x)
  (import (scheme base))
  (begin (define-record-type ra (make-a x) a? (x a-x))))
