; Chez v4.x

(define (values . x) x)

(define (call-with-values proc receiver)
  (apply receiver (proc)))

