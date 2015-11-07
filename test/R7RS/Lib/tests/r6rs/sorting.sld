(define-library (tests r6rs sorting)
  (export run-sorting-tests)
  (import (except (scheme base) error bytevector-copy!)
          (scheme write)
          (r6rs base)
          (r6rs sorting)
          (tests scheme test))

 (begin
  (define (run-sorting-tests)

    (test (list-sort < '(3 5 2 1)) '(1 2 3 5))
    (test (vector-sort < '#(3 5 2 1)) '#(1 2 3 5))

    (let ((v (vector 3 5 2 1)))
      (test/unspec (vector-sort! < v))
      (test v '#(1 2 3 5)))

    ;;
    )))

