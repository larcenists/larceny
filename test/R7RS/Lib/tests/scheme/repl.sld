;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests this (scheme repl) procedure:
;;;
;;;     interaction-environment
;;;
;;; Assumes the tests directory is a subdirectory of the current
;;; working directory.


(define-library (tests scheme repl)
  (export run-repl-tests)
  (import (scheme base)
          (scheme repl)
          (tests scheme test))

  (cond-expand
   ((library (scheme eval))
    (import (scheme eval)))
   (else))

  (cond-expand
   ((library (scheme load))
    (import (scheme load)))
   (else))

  (begin

   (cond-expand
    ((library (scheme eval))
     (begin (define (run-tests-using-eval)
              (run-tests-assuming-scheme-eval))))
    ((not (library (scheme eval)))
     (begin (define (environment . args) #t)
            (define (eval . args) #t)
            (define (run-tests-using-eval) #t))))

   (cond-expand
    ((library (scheme load))
     (begin (define (run-tests-using-load)
              (run-tests-assuming-scheme-load))))
    ((not (library (scheme load)))
     (begin (define (load . args) #t)
            (define (run-tests-using-load) #t))))

   (define (run-repl-tests)

     (run-tests-using-eval)
     (run-tests-using-load)

     )

   (define (run-tests-assuming-scheme-eval)

     (test (eval '(let ((x 3)) x)
                 (interaction-environment))
           3)

     (test (eval
            '(car (cons 2 4))
            (interaction-environment))
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
            (interaction-environment))
           356)

     (test/unspec (eval '(begin (define x 331) (define y 401))
                        (interaction-environment)))

     (test (eval '(list x y) (interaction-environment))
           '(331 401))

     (test (eval '(max 833 634 761) (interaction-environment))
           833)

     (test/unspec (eval '(define (max . args)
                           (let* ((args (map * args '(5 -3 4 -2)))
                                  (x (apply + args)))
                             (abs x)))
                        (interaction-environment)))

     (test (eval '(max 833 634 761) (interaction-environment))
           5307)

     )

   (define (run-tests-assuming-scheme-load)

     (test
      (let ((p (open-output-string)))
             (parameterize ((current-output-port p))
              (load "tests/scheme/load-test1.scm"
                    (interaction-environment))
              (get-output-string p)))
           (string-append
            "WHEN I wrote the following pages, or rather the bulk of them, "
            "I lived alone, in the woods, a mile from any "
            "neighbor, in a house which I had built myself, on "
            "the shore of Walden Pond, in Concord, Massachusetts, and earned "
            "my living by the labor of my hands only."))

     (test (begin (load "tests/scheme/load-test2.scm"
                        (interaction-environment))
                  (let ((p (open-output-string)))
                    (parameterize ((current-output-port p))
                     (load "tests/scheme/repl-test2.scm"
                           (interaction-environment))
                     (let* ((s (get-output-string p))
                            (q (open-input-string s))
                            (s1 (read-line q))
                            (s2 (read-line q)))
                       (close-output-port p)
                       (close-input-port q)
                       (list (string->number s1) (string->number s2))))))
           '(55 120))

     )

   ))
