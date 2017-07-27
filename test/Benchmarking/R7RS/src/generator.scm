;;; Generator benchmark.
;;;
;;; Uses an example taken from the SRFI 41 document.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme generator))

(define (pythagorean-triples-using-generators nth)
  (call-with-current-continuation
   (lambda (return)
     (let ((count 0)
           (n-values (make-iota-generator +inf.0 1)))
       (do ((n (n-values) (n-values)))
           ((> count nth))
         (let ((a-values (make-range-generator 1 (+ n 1))))
           (do ((a (a-values) (a-values)))
               ((eof-object? a))
             (let ((b-values (make-range-generator a (+ n 1))))
               (do ((b (b-values) (b-values)))
                   ((eof-object? b))
                 (let ((c (- n a b)))
                   (if (= (+ (* a a) (* b b)) (* c c))
                       (begin (set! count (+ count 1))
                              (if (> count nth)
                                  (return (list a b c)))))))))))))))

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
  (pythagorean-triples-using-generators n))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "generator"))
    ;(write (pythagorean-triples-using-loops input1))
    ;(newline)
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
