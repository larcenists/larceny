;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme ephemeron) procedures:
;;;
;;;     ephemeron?
;;;     make-ephemeron
;;;     ephemeron-broken?
;;;     ephemeron-key
;;;     ephemeron-datum
;;;     reference-barrier
;;;
;;; The number of references to an object is very implementation-dependent,
;;; so these tests might not be completely portable.


(define-library (tests scheme ephemeron)
  (export run-ephemeron-tests)
  (import (scheme base)
          (scheme ephemeron)
          (tests scheme test))

  ;; Adapted from srfi-124-test.sps

  (begin

   (define (iota n)
     (define (loop n z)
       (if (= n 0)
           z
           (loop (- n 1) (cons (- n 1) z))))
     (loop n '()))

   (define keys
     (map (lambda (n)
            (list (+ n 89870)))
          (iota 10)))

   (define vals
     (map vector
          (map (lambda (n) (+ n 5000))
               (map car keys))))

   (define ephemera
     (map make-ephemeron keys vals))

   (define N 5)

   ;; Allocating 10 million pairs ten times should force a full gc.

   (define pairs-to-overflow-nursery 10000000)
   (define overflows-to-force-full-gc 10)

   (define (force-gc)
     (define (loop n x)
       (if (> n 0)
           (loop (- n 1)
                 (car (iota pairs-to-overflow-nursery)))
           x))
     (loop overflows-to-force-full-gc 0))

   (define (run-ephemeron-tests)

     (test (map ephemeron-key ephemera)
           keys)

     (test (map ephemeron-datum ephemera)
           vals)

     (test (map ephemeron-broken? ephemera)
           (map (lambda (x) #f) ephemera))

     (set! keys (reverse (reverse (list-tail keys N))))

     (force-gc)

     (test (map ephemeron-key (list-tail ephemera N))
           keys)

     (test (map ephemeron-datum (list-tail ephemera N))
           (list-tail vals N))

     (test (map ephemeron-broken? ephemera)
           (append (vector->list (make-vector N #t))
                   (map (lambda (x) #f)
                        (list-tail ephemera N)))))))
