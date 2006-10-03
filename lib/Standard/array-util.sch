; Array utilities.
; $Id$
;
;   (LIST->ARRAY <list>) => <array>
;   (ARRAY->LIST <array>) => <list>
;   (ARRAY-COPY <array>) => <array>
;   (ARRAY=? <pred> <array> <array> <array> ...) => <boolean>
;   (ARRAY-POPULATE! <array> <fn>) => <array>

(require 'array)

; list->array and array->list assume row-major element ordering.

(define (list->array l)                 ; Assumes 0-based

  (define (shape l)
    (if (pair? l)
        (cons (length l)
              (shape (car l)))
        '()))

  (define (populate a l indices)
    (if (pair? l)
        (do ((i 0 (+ i 1))
             (l l (cdr l)))
            ((null? l) a)
          (populate a (car l) (append indices (list i))))
        (apply array-set! a l indices)))

  (populate (apply make-array 0 (shape l))
            l
            '()))

(define (array->list a)                 ; Loses shape info

  (define (unpopulate a shape indices)
    (if (pair? shape)
        (do ((i (caar shape) (+ i 1))
             (l '()
                (cons (unpopulate a (cdr shape) (append indices (list i)))
                      l)))
            ((> i (cadar shape)) (reverse l)))
        (apply array-ref a indices)))

  (unpopulate a (array-shape a) '()))

(define (array-copy a)
  (array-populate! (apply make-array 0 (array-shape a))
                   (lambda indices (apply array-ref a indices))))

; Arrays are not comparable using equal? because they contain indexing
; procedures that are not comparable.  Also, a more generalized notion
; of equality may be useful: arrays are equal if they have the same
; shape _independent of the bounds_ and if they are elementwise equal.
;
; In Larceny it is currently possible to install a comparator procedure,
; but it's global.  It would be better to install a comparator procedure
; in the RTD, like the printer.

(define (array=? = a b . others)

  (define (same-shape? sa sb)
    (and (= (length sa) (length sb))
         (every? (lambda (a b)
                   (or (and (number? a) 
                            (number? b) 
                            (= a b))
                       (and (pair? a) 
                            (pair? b) 
                            (= (- (cadr a) (car a)) (- (cadr b) (car b))))))
                 sa sb)))

  (define (same-contents? sa sb ia ib)
    (if (pair? sa)
        (do ((i (caar sa) (+ i 1))
             (j (caar sb) (+ j 1))
             (t #t (and t (same-contents? (cdr sa) 
                                          (cdr sb) 
                                          (append ia (list i))
                                          (append ib (list j))))))
            ((or (not t) (> i (cadar sa))) t))
        (= (apply array-ref a ia) (apply array-ref b ib))))

  (define (same? a b)
    (let ((sa (array-shape a))
          (sb (array-shape b)))
      (and (same-shape? sa sb)
           (same-contents? sa sb '() '()))))
          
  (and (same? a b)
       (or (null? others)
           (apply array=? = b others))))

(define (array-populate! a f)
  (define (populate shape indices)
    (if (pair? shape)
        (do ((i (caar shape) (+ i 1)))
            ((> i (cadar shape)))
          (populate (cdr shape) (append indices (list i))))
        (apply array-set! a (apply f indices) indices)))
  (let ((s (array-shape a)))
    (populate s '())
    a))
  
; eof
