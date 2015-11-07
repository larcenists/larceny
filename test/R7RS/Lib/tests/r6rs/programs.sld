(define-library (tests r6rs programs)
  (export run-programs-tests)
  (import (scheme base)
          (scheme write)
          (r6rs programs)
          (tests scheme test))

 (begin
  (define (run-programs-tests)

    (test (list? (command-line)) #t)
    (test (string? (car (command-line))) #t)
      
    ;;
    )))

