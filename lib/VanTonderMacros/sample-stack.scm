;;;
;;; Sample library - see macros-test.scm for
;;;                  compilation script 
;;;

(library (stack)
  (export make push! pop! empty!)
  (import (rnrs)
          (rnrs mutable-pairs))
  
  (define (make)
    (list '()))
  
  (define (push! s v)
    (set-car! s (cons v (car s))))
  
  (define (pop! s)
    (let ((v (caar s))) (set-car! s (cdar s)) v))
  
  (define (empty! s)
    (set-car! s '()))
  )