;;; Copyright 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-sort compare lst)
  (define (merge left right)
    (let loop ((left left)
               (right right)
               (result '()))
      (cond
       ((and (null? left) (null? right))
        (reverse result))
       ((null? left)
        (loop left (cdr right) (cons (car right) result)))
       ((null? right)
        (loop (cdr left) right (cons (car left) result)))
       (else
        (let ((left1 (car left))
              (right1 (car right)))
          (if (compare left1 right1)
              (loop (cdr left) right (cons left1 result))
              (loop left (cdr right) (cons right1 result))))))))
  (define (pair-map! proc lst)
    ;; { a, b, c, d[, e] } -> f -> { f(a, b), f(c, d)[, e] }
    (unless (or (null? lst) (null? (cdr lst)))
      (let ((first (car lst))
            (second (car (cdr lst)))
            (rest (cdr (cdr lst))))
        (set-car! lst (proc first second))
        (set-cdr! lst rest)
        (pair-map! proc rest))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((sublists (map list lst)))
        (do ()
            ((null? (cdr sublists))
             (car sublists))
          (pair-map! merge sublists)))))

;;; This must not mutate the return value of previous returns in case of
;;; multiple returns, so we can't copy & in-place-sort.
(define (vector-sort compare vector)
  (list->vector (list-sort compare (vector->list vector))))

(define (vector-sort! compare vector)
  (define (ref i) (vector-ref vector i))
  (define (set i obj) (vector-set! vector i obj))
  (define (swap i j)
    (let ((i-val (ref i))
          (j-val (ref j)))
      (set i j-val)
      (set j i-val)))
  (define (partition from to)
    (let* ((pivot-index (floor-quotient (+ from to) 2))
           (pivot-value (ref pivot-index)))
      (swap pivot-index to)
      (do ((i from (+ i 1))
           (left from (if (compare (ref i) pivot-value)
                          (begin (swap i left)
                                 (+ left 1))
                          left)))
          ((= i to)
           (swap left to)
           (do ((right left (+ right 1)))
               ((or (= right to)
                    (compare pivot-value (ref right)))
                (values left right)))))))
  (define (quicksort from to)
    (when (< from to)
      (let-values (((left right) (partition from to)))
        (quicksort from left)
        (quicksort right to))))
  (quicksort 0 (- (vector-length vector) 1)))
