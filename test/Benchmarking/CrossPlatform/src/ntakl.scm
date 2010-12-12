;;; NTAKL -- The TAKeuchi function using lists as counters,
;;; with an alternative boolean expression.
 
(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))
 
(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))
 
; Part of the fun of this benchmark is seeing how well the compiler
; can understand this ridiculous code, which dates back to the original
; Common Lisp.  So it probably isn't a good idea to improve upon it.

'
(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))
 
; But SML/NJ runs this benchmark about 15 times as fast when the
; code above is rewritten as follows, so I tried it for Scheme also.

(define (shorterp x y)
  (cond ((null? y) #f)
        ((null? x) #t)
        (else
         (shorterp (cdr x) (cdr y)))))

(define (main . args)
  (run-benchmark
    "ntakl"
    takl-iters
    (lambda (result) (equal? result '(7 6 5 4 3 2 1)))
    (lambda () (lambda () (mas l18 l12 l6)))))
