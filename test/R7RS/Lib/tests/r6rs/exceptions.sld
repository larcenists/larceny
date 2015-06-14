(define-library (tests r6rs exceptions)
  (export run-exceptions-tests)
  (import (except (scheme base) error bytevector-copy!)
          (scheme read)
          (scheme write)
          (scheme file)
          (r6rs base)
          (r6rs exceptions)
          (tests scheme test))

 (begin
  (define (run-exceptions-tests)

    (define (make-error-object msg)
      (guard (exn
              (else exn))
       (error msg)))

    (define an-error-object
      (make-error-object "I am an error"))

    (define another-error-object
      (make-error-object "should be a number"))

    (test/output
     (guard (con
             ((error-object? con)
              (display (error-object-message con))
              'error))
            (raise an-error-object))
     'error
     "I am an error")

    (test/output
     (guard (con
             ((file-error? con)
              (display "error opening file")
              #f))
            (call-with-input-file "foo-must-not-exist.scm" read))
     #f
     "error opening file")

    (test/output
     (with-exception-handler
      (lambda (con)
        (cond
         ((error-object? con)
          (display (error-object-message con)))
         (else
          (display "a warning has been issued")))
        42)
      (lambda ()
        (+ (raise-continuable another-error-object)
           23)))
     65
     "should be a number")

    (test/exn (with-exception-handler (lambda (x) 0)
                                      (lambda () (error #f "bad")))
              &non-continuable)

    
    (let ((v '()))
      (test (guard (exn ((equal? exn 5) 'five))
                   ;; `guard' should jump back in before re-raising
                   (guard (exn ((equal? exn 6) 'six))
                          (dynamic-wind
                              (lambda () (set! v (cons 'in v)))
                              (lambda () (raise 5))
                              (lambda () (set! v (cons 'out v))))))
            'five)
      (test v '(out in out in)))
      

      
    ;;
    )))

