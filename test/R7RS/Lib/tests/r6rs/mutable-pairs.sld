(define-library (tests r6rs mutable-pairs)
  (export run-mutable-pairs-tests)
  (import (scheme base)
          (scheme write)
          (r6rs mutable-pairs)
          (tests scheme test))

 (begin
  (define (f) (list 'not-a-constant-list))
  (define (g) '(constant-list))
  
  (define (run-mutable-pairs-tests)

    (test/unspec (set-car! (f) 3))
    (test/unspec-or-exn (set-car! (g) 3)
                        &assertion)

    (test (let ((x (list 'a 'b 'c 'a))
                (y (list 'a 'b 'c 'a 'b 'c 'a)))
            (set-cdr! (list-tail x 2) x)
            (set-cdr! (list-tail y 5) y)
            (list
             (equal? x x)
             (equal? x y)
             (equal? (list x y 'a) (list y x 'b))))
          '(#t #t #f))
      
    ;;
    )))

