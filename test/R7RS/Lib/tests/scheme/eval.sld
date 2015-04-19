;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme eval) procedures:
;;;
;;;     environment
;;;     eval


(define-library (tests scheme eval)
  (export run-eval-tests)
  (import (scheme base)
          (scheme eval)
          (tests scheme test))

  (cond-expand
   ((library (scheme r5rs))
    (import (scheme r5rs))
    (include "eval.body.scm"))
   (else))

  (begin

   (cond-expand
    ((not (library (scheme r5rs)))
     (begin (define (run-r5rs-eval-tests) #t)))
    (else))

   (define (run-eval-tests)
     (test (eval '(let ((x 3)) x)
                 (environment '(scheme base)))
           3)

     (test (eval
            '(eval:car (eval:cons 2 4))
            (environment
             '(prefix (only (scheme base) car cdr cons null?)
                      eval:)))
           2)

     (test (eval
            '(let* ((n 0)
                    (f (lambda args
                         (cond ((null? args)
                                (set! n (+ n 1)))
                               ((number? (car args))
                                (set! n (+ n (car args))))
                               ((eq? 'reset (car args))
                                (set! n 0))
                               (else
                                (error "bad argument(s) to f" args)))
                         n)))
               (for-each f '(1 1 1 1 1 100 250))
               (f))
            (environment '(scheme base)))
           356)

     (run-r5rs-eval-tests))

   ;;
   ))

