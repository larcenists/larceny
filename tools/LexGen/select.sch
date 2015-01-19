; Given a predicate and a list,
; returns the first element of the list that satisfies the predicate,
; or #f if none satisfies.

(define (select p? x)
  (cond ((null? x) #f)
        ((p? (car x)) (car x))
        (else (select p? (cdr x)))))