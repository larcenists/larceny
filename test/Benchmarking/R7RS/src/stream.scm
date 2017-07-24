;;; Stream benchmark.
;;;
;;; Uses an example taken from the SRFI 41 document.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme stream))

(define (pythagorean-triples-using-streams n)
  (stream-ref
    (stream-of (list a b c)
      (n in (stream-from 1))
      (a in (stream-range 1 n))
      (b in (stream-range a n))
      (c is (- n a b))
      (= (+ (* a a) (* b b)) (* c c)))
    n))

(define (pythagorean-triples-using-loops nth)
  (call-with-current-continuation
   (lambda (return)
     (let ((count 0))
       (do ((n 1 (+ n 1)))
           ((> count nth))
         (do ((a 1 (+ a 1)))
             ((> a n))
           (do ((b a (+ b 1)))
               ((> b n))
             (let ((c (- n a b)))
               (if (= (+ (* a a) (* b b)) (* c c))
                   (begin (set! count (+ count 1))
                          (if (> count nth)
                              (return (list a b c)))))))))))))

(define (go n)
  (pythagorean-triples-using-streams n))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "stream"))
    ;(write (pythagorean-triples-using-loops input1))
    ;(newline)
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
