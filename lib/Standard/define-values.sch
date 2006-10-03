; DEFINE-VALUES syntax
; 2004-01-13 / lth
;
;   (DEFINE-VALUES (v1 ...) E)
;
; expands into a bunch of definitions of the vs with the
; values returned by E.  There must be as many values
; returned by E as there are vs.

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
