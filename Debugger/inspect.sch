(define (procedure-code-vector proc)
  (procedure-ref proc 0))

(define (procedure-constant-vector proc)
  (procedure-ref proc 1))

(define (procedure-static-link proc)
  (procedure-ref proc 2))

(define (procedure-slots proc)
  (- (procedure-length proc) 3))

(define (procedure-slot-ref proc k)
  (procedure-ref proc (+ k 3)))

; Returns a variable name or #f.

(define (procedure-slot-name proc k)
  #f)
