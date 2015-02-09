;;; Tests of eval using (scheme r5rs) environments.

(define (run-r5rs-eval-tests)

  (test (eval '(do ((x #f #t))
                   (x 'y)
                 (let loop ()
                   (if x (loop))))
              (null-environment 5))
        'y)

  (test (eval '(let ((x 10))
                 (define (fib n)
                   (if (< n 2)
                       n
                       (+ (fib (- n 1))
                          (fib (- n 2)))))
                 (fib x))
              (scheme-report-environment 5))
        55))
