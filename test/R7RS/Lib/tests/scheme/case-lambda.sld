;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests this (scheme case-lambda) syntax:
;;;
;;;     case-lambda


(define-library (tests scheme case-lambda)
 (export run-case-lambda-tests)
 (import (scheme base)
         (scheme case-lambda)
         (tests scheme test))

 (begin

  (define (run-case-lambda-tests)

    (let ((foo
           (case-lambda 
            (() 'zero)
            ((x) (list 'one x))
            ((x y) (list 'two x y))
            ((a b c d . e) (list 'four a b c d e))
            (rest (list 'rest rest)))))

      (test (foo) 'zero)
      (test (foo 1) '(one 1))
      (test (foo 1 2) '(two 1 2))
      (test (foo 1 2 3) '(rest (1 2 3)))
      (test (foo 1 2 3 4) '(four 1 2 3 4 ())))
      
    ;;
    )))

