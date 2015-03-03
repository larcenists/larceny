;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests this (scheme load) procedure:
;;;
;;;     load
;;;
;;; Assumes the tests directory is a subdirectory of the current
;;; working directory.


(define-library (tests scheme load)
  (export run-load-tests
          poor-hash                 ; for load-test3.scm
          )
  (import (scheme base)
          (scheme load)
          (tests scheme test))

  (cond-expand
   ((library (scheme eval))
    (import (scheme eval)))
   (else))

  (cond-expand
   ((and (library (scheme eval))
         (library (scheme repl)))
    (import (scheme repl)))
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
    ((and (library (scheme eval))
          (library (scheme repl)))
     (begin (define (run-tests-using-eval-and-repl)
              (run-tests-assuming-scheme-eval-and-repl))))
    ((not (and (library (scheme eval))
               (library (scheme repl))))
     (begin (define (interaction-environment) #f)
            (define (run-tests-using-eval-and-repl) #t))))

   ;; Just a computation that isn't likely to happen by accident.

   (define (poor-hash . args)
     (let* ((args (map * args '(5 -3 4 -2)))
            (x (apply + args)))
       (abs x)))            

   (define (run-load-tests)

     (test
      (let ((p (open-output-string)))
             (parameterize ((current-output-port p))
              (load "tests/scheme/load-test1.scm")
              (get-output-string p)))
           (string-append
            "WHEN I wrote the following pages, or rather the bulk of them, "
            "I lived alone, in the woods, a mile from any "
            "neighbor, in a house which I had built myself, on "
            "the shore of Walden Pond, in Concord, Massachusetts, and earned "
            "my living by the labor of my hands only."))

     (run-tests-using-eval)
     (run-tests-using-eval-and-repl)

     )

   (define (run-tests-assuming-scheme-eval)

     (test
      (let ((p (open-output-string)))
             (parameterize ((current-output-port p))
              (load "tests/scheme/load-test1.scm"
                    (environment '(scheme base)))
              (get-output-string p)))
           (string-append
            "WHEN I wrote the following pages, or rather the bulk of them, "
            "I lived alone, in the woods, a mile from any "
            "neighbor, in a house which I had built myself, on "
            "the shore of Walden Pond, in Concord, Massachusetts, and earned "
            "my living by the labor of my hands only."))

     (test
      (let ((p (open-output-string)))
             (parameterize ((current-output-port p))
              (load "tests/scheme/load-test3.scm"
                    (environment '(except (scheme base) max)
                                 '(rename (only (tests scheme load) poor-hash)
                                          (poor-hash max))))
              (string->number (get-output-string p))))
      5307)

     )

   (define (run-tests-assuming-scheme-eval-and-repl)

     (test (begin (load "tests/scheme/load-test2.scm")
                  (list (eval '(fib 10) (interaction-environment))
                        (eval '(fact 5) (interaction-environment))))
           '(55 120))

     )

   ))
