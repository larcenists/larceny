(define-library (t b)
  (export make-b b-y)
  (import (scheme base))
  (begin (define-record-type rb (make-b y) b? (y b-y))))
