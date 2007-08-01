;;;
;;; Sample library - see macros-test.scm for
;;;                  compilation script 
;;;

(library (balloons)

  (export make push pop)
  (import (rnrs))
  
  (define (make w h)
    (cons w h))
  
  (define (push b amt)
    (cons (- (car b) amt) (+ (cdr b) amt)))
  
  (define (pop b)
    (display "Boom! ")
    (display (* (car b) (cdr b)))
    (newline)))