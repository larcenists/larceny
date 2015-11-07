(define-library (tests r6rs eval)
  (export run-eval-tests)
  (import (except (scheme base) error bytevector-copy!)
          (scheme write)
          (r6rs base)
          (r6rs eval)
          (tests scheme test))

 (begin
  (define (run-eval-tests)

    (test (eval '(let ((x 3)) x)
                (environment '(r6rs base)))
          3)

    (test (eval
           '(eval:car (eval:cons 2 4))
           (environment
            '(prefix (only (r6rs base) car cdr cons null?)
                     eval:)))
          2)

#|

    ;; Check that `eval' at compile-time produces values (such as conditions)
    ;; that make sense at compile time (i.e., no phase crossing):
    (test (eval
           '(let-syntax ((x (lambda (stx)
                              (datum->syntax
                               #'here
                               (condition-message
                                (call/cc
                                 (lambda (esc)
                                   (with-exception-handler
                                    (lambda (exn) (esc exn))
                                    (lambda ()
                                      (eval '(assertion-violation 'exptime "ok")
                                            (environment
                                             '(rnrs)
                                             '(rnrs eval))))))))))))
              x)
           (environment '(rnrs) '(for (rnrs eval) expand)))
          "ok")

|#

    ;;
    )))

