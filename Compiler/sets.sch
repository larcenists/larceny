; Copyright 1991 Wiliam Clinger.
;
; $Id$
;
; Sets represented as lists.
;
; 5 April 1999.

(define (empty-set) '())

(define (empty-set? x) (null? x))

(define (make-set x)
  (define (loop x y)
    (cond ((null? x) y)
          ((member (car x) y) (loop (cdr x) y))
          (else (loop (cdr x) (cons (car x) y)))))
  (loop x '()))

(define (set-equal? x y)
  (and (subset? x y) (subset? y x)))

(define (subset? x y)
  (every? (lambda (x) (member x y))
          x))

; To get around MacScheme's limit on the number of arguments.

(define apply-union)

(define union
  (letrec ((union2
            (lambda (x y)
              (cond ((null? x) y)
                    ((member (car x) y)
                     (union2 (cdr x) y))
                    (else (union2 (cdr x) (cons (car x) y)))))))
    
    (set! apply-union
          (lambda (sets)
            (do ((sets sets (cdr sets))
                 (result '() (union2 (car sets) result)))
                ((null? sets)
                 result))))
    
    (lambda args
      (cond ((null? args) '())
            ((null? (cdr args)) (car args))
            ((null? (cddr args)) (union2 (car args) (cadr args)))
            (else (union2 (union2 (car args)
                                  (cadr args))
                          (apply union (cddr args))))))))

(define intersection
  (letrec ((intersection2
            (lambda (x y)
              (cond ((null? x) '())
                    ((member (car x) y)
                     (cons (car x) (intersection2 (cdr x) y)))
                    (else (intersection2 (cdr x) y))))))
    (lambda args
      (cond ((null? args) '())
            ((null? (cdr args)) (car args))
            ((null? (cddr args)) (intersection2 (car args) (cadr args)))
            (else (intersection2 (intersection2 (car args)
                                                (cadr args))
                                 (apply intersection (cddr args))))))))

(define (difference x y)
  (cond ((null? x) '())
        ((member (car x) y)
         (difference (cdr x) y))
        (else (cons (car x) (difference (cdr x) y)))))
