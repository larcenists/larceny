; Copyright 1993 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; Sets represented as lists.

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

(define (every? p? x)
  (cond ((null? x) #t)
        ((p? (car x))
         (every? p? (cdr x)))
        (else #f)))

(define (union2 x y)
  (cond ((null? x) y)
        ((member (car x) y)
         (union2 (cdr x) y))
        (else (union2 (cdr x) (cons (car x) y)))))

(define union
  (letrec ((union2
            (lambda (x y)
              (cond ((null? x) y)
                    ((member (car x) y)
                     (union2 (cdr x) y))
                    (else (union2 (cdr x) (cons (car x) y)))))))
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
