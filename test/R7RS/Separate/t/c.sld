; Uses the accessors of (t a) and (t b), which are macros that refer
; to the record types, bindings those libraries do not export.

(define-library (t c)
  (export sum)
  (import (scheme base) (t a) (t b))
  (begin (define (sum p q) (+ (a-x p) (b-y q)))))
