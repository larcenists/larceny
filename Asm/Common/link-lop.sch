; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; LOP segment linker.
;
; Given a lop segment, return a thunk.  Calling the thunk runs the code
; represented by the segment.  This procedure is useful for bypassing the
; evaluator in an interactive system, e.g.,
;
;    (define (eval x env)
;      ((link-lop-segment (assemble (compile x)) env)))

(define (link-lop-segment lop-segment environment)

  (define (link-constant c)
    (case (car c)
      ((data)
       (cadr c))
      ((constantvector)
       (link-constants (cadr c)))
      ((codevector)
       (cadr c))
      ((global)
       (environment-get-cell environment (cadr c)))
      ((bits)
       (error "BITS attribute not supported by link-lop-segment"))
      (else
       (error "Illegal lop constant: " c))))
       
  (define (link-constants cvec)
    (let ((v (make-vector (vector-length cvec) #f)))
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length cvec)) v)
	(vector-set! v i (link-constant (vector-ref cvec i))))))

  (let ((code  (car lop-segment))
	(const (link-constants (cdr lop-segment))))
    (let ((p (make-procedure 3)))
      (procedure-set! p 0 code)
      (procedure-set! p 1 const)
      (procedure-set! p 2 #f)
      p)))

; eof
