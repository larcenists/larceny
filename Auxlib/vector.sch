; Auxlib/vector.sch
; Larceny auxiliary library -- vector functions.
;
; $Id: vector.sch,v 1.1.1.1 1998/11/19 21:52:20 lth Exp $

(define (vector-copy v)
  (let ((v2 (make-vector (vector-length v) #f)))
    (do ((i (- (vector-length v) 1) (- i 1)))
        ((< i 0) v2)
      (vector-set! v2 i (vector-ref v i)))))

; eof
