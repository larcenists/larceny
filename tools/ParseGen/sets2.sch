; Copyright 1993 William Clinger
; Copyright 2008 William D Clinger (to use R6RS-style hashtables)
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revised to use filter and R6RS-style hashtables for large sets,
; where large is defined by the threshold below.

(define largest-small-set 10)

(define (large-set? x)
  (> (length x) largest-small-set))

(define (set->hashtable x)
  (let ((ht (make-hashtable equal-hash equal?)))
    (for-each (lambda (a) (hashtable-set! ht a #t))
              x)
    ht))

(define (hashtable->set ht)
  (vector->list (hashtable-keys ht)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty-set) '())

(define (empty-set? x) (null? x))

(define (make-set x)
  (if (large-set? x)
      (hashtable->set (set->hashtable x))
      (let ()
        (define (loop x y)
          (cond ((null? x) y)
                ((member (car x) y) (loop (cdr x) y))
                (else (loop (cdr x) (cons (car x) y)))))
        (loop x '()))))

; FIXME: if both x and y are true sets, then the second
; call to subset? in the code below is unnecessary.

(define (set-equal? x y)
  (let ((n1 (length x))
        (n2 (length y)))
    (and (= n1 n2)
         (subset? x y)
         (subset? y x))))

(define (subset? x y)
  (if (large-set? y)
      (let ((ht (set->hashtable y)))
        (every? (lambda (a) (hashtable-contains? ht a))
                x))
      (every? (lambda (a) (member a y))
              x)))

(define (every? p? x)
  (cond ((null? x) #t)
        ((p? (car x))
         (every? p? (cdr x)))
        (else #f)))

(define (union2 x y)
  (define (union2 x y)
    (cond ((null? x) y)
          ((member (car x) y)
           (union2 (cdr x) y))
          (else (union2 (cdr x) (cons (car x) y)))))
  (if (or (large-set? x) (large-set? y))
      (let ((ht (set->hashtable x)))
        (for-each (lambda (a) (hashtable-set! ht a #t))
                  y)
        (hashtable->set ht))
      (union2 x y)))

(define (union . args)
  (cond ((null? args) '())
        ((null? (cdr args)) (car args))
        ((null? (cddr args)) (union2 (car args) (cadr args)))
        (else
         (let ((ht (set->hashtable (car args))))
           (for-each (lambda (x)
                       (for-each (lambda (a) (hashtable-set! ht a #t))
                                 x))
                     (cdr args))
           (hashtable->set ht)))))

(define (intersection2 x y)
  (define (intersection2 x y)
    (cond ((null? x) '())
          ((member (car x) y)
           (cons (car x) (intersection2 (cdr x) y)))
          (else (intersection2 (cdr x) y))))
  (let ((n1 (length x))
        (n2 (length y)))
    (cond ((and (> n1 largest-small-set) (> n2 largest-small-set))
           (let ((ht (set->hashtable x)))
             (filter (lambda (a) (hashtable-contains? ht a))
                     y)))
          ((> n1 n2)
           (intersection2 x y))
          (else
           (intersection2 y x)))))

(define intersection
  (lambda args
    (cond ((null? args) '())
          ((null? (cdr args)) (car args))
          ((null? (cddr args)) (intersection2 (car args) (cadr args)))
          (else (intersection2 (intersection2 (car args)
                                              (cadr args))
                               (apply intersection (cddr args)))))))

(define (difference x y)
  (cond ((large-set? y)
         (let ((ht (set->hashtable y)))
           (filter (lambda (a) (not (hashtable-contains? ht a)))
                   x)))
        ((null? x) '())
        ((member (car x) y)
         (difference (cdr x) y))
        (else (cons (car x) (difference (cdr x) y)))))
