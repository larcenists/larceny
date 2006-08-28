; Doesn't work in Larceny 0.45 due to a bug in the macro expander.

(define-syntax define-values
  (syntax-rules ()
    ((define-values () ?expr) 
     ?expr)
    ((define-values (?v1 ?v2 ...) ?expr)
     (begin
       (define __super-secret-variable-name
         (call-with-values 
          (lambda () ?expr)
          (lambda values values)))
       (define-values #t (?v1 ?v2 ...) __super-secret-variable-name 0)))
    ((define-values #t () ?v ?n)
     (set! ?v #f))
    ((define-values #t (?v1 ?v2 ...) ?v ?n)
     (begin
       (define ?v1 (list-ref ?v ?n))
       (define-values #t (?v2 ...) ?v (+ ?n 1))))))
