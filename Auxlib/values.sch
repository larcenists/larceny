(define values list)
(define (call-with-values thunk receiver) (apply receiver (thunk)))
