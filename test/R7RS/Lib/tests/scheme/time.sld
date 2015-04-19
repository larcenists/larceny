;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme time) procedures:
;;;
;;;     current-jiffy
;;;     current-second
;;;     jiffies-per-second


(define-library (tests scheme time)
  (export run-time-tests)
  (import (scheme base)
          (scheme time)
          (tests scheme test))

  (begin

   (define (run-time-tests)

     ;; Compilers might optimize this into (if #f #f),
     ;; but the tests would still work.

     (define (countdown n)
       (if (> n 0)
           (countdown (- n 1))))

     (define (count-until thunk)
       (define million 1000000)
       (let loop ((k 0))
         (if (thunk)
             (* k million)
             (begin (countdown million)
                    (loop (+ k 1))))))

     (define loops/s 0)

     (test (map exact?
                (list (current-second) (current-jiffy) (jiffies-per-second)))
           '(#f #t #t))

     ;; Jiffy timing should be accurate to within a tenth of a second,
     ;; even if there's just one jiffy per second.

     (test (let* ((t0 (current-second))
                  (t1 (+ t0 (inexact 1))))
             (count-until (lambda () (<= t1 (current-second))))
             (let* ((jifs/second (jiffies-per-second))
                    (t0 (current-second))
                    (j0 (current-jiffy))
                    (j1 (+ j0 jifs/second)))
               (let* ((n (count-until (lambda () (<= j1 (current-jiffy)))))
                      (j (current-jiffy))
                      (t (current-second)))
                 (set! loops/s n)
                 (list (- (exact (round t)) (exact (round t0)))
                       (<= j1 j)
                       (<= j (+ j1 (/ jifs/second 10)))))))
           '(1 #t #t))

     loops/s)))
