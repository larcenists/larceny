;;; Bulk vector copies and fills during the regional collector's
;;; concurrent marking.
;;;
;;; vector-copy! and vector-fill! of at least
;;; sys$vector-like-move-threshold elements do the barriers' work in C
;;; (range_pre_barrier and range_post_barrier, Rts/Sys/primitive.c).
;;; This program moves 50000 objects back and forth between two old
;;; vectors, clearing the source with vector-fill! after each copy, and
;;; allocates between moves so that marking runs while it does.  Without
;;; the SATB pre-barrier the marker can miss the objects (the vector it
;;; already scanned gains them, the one it has not loses them), and the
;;; program fails or crashes.  Run it with
;;;
;;;     larceny -rrof -- vector-move.sch
;;;     larceny -rrof -size1 2M -- vector-move.sch
;;;
;;; (and with the other collectors).  It prints "vector-move: ok" and
;;; exits.

(define n 50000)
(define a (make-vector n #f))
(define b (make-vector n #f))
(do ((i 0 (+ i 1))) ((= i n)) (vector-set! a i (list 'x i (make-vector 3 i))))
(collect)
(define (ok? v)
  (let loop ((i 0))
    (or (= i n)
        (let ((x (vector-ref v i)))
          (and (pair? x) (eq? (car x) 'x) (eqv? (cadr x) i)
               (vector? (caddr x)) (eqv? (vector-ref (caddr x) 2) i)
               (loop (+ i 1)))))))
(define junk '())
(define (churn k)
  (do ((i 0 (+ i 1))) ((= i k))
    (set! junk (cons (make-vector 10 i) (if (= 0 (remainder i 5000)) '() junk)))))
(define result
  (let loop ((r 0) (from a) (to b))
    (cond ((= r 400) 'passed)
          (else
           (vector-copy! to 0 from)
           (vector-fill! from #f)
           (churn 20000)
           (if (ok? to)
               (loop (+ r 1) to from)
               (list 'failed-at r))))))
(display "vector-move: ")
(display (if (eq? result (quote passed)) "ok" result))
(newline)
(exit)
