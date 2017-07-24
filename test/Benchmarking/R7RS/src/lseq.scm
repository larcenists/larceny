;;; Lazy sequence benchmark.
;;;
;;; Uses an example taken from the SRFI 41 document.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme generator)
        (scheme lseq))

(define (pythagorean-triples-using-lazy-sequences nth)
  (call-with-current-continuation
   (lambda (return)
     (let ((count 0)
           (n-values (generator->lseq (make-iota-generator +inf.0 1))))
       (do ((n        (lseq-car n-values) (lseq-car n-values))
            (n-values (lseq-cdr n-values) (lseq-cdr n-values)))
           ((> count nth))
         (let ((a-values
                (generator->lseq (make-range-generator 1 (+ n 1)))))
           (do ((a        (lseq-car a-values) (lseq-car a-values))
                (a-values (lseq-cdr a-values) (lseq-cdr a-values)))
               ((null? a-values))
             (let ((b-values
                    (generator->lseq (make-range-generator a (+ n 1)))))
               (do ((b        (lseq-car b-values) (lseq-car b-values))
                    (b-values (lseq-cdr b-values) (lseq-cdr b-values)))
                   ((null? b-values))
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
  (pythagorean-triples-using-lazy-sequences n))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "lseq"))
    ;(write (pythagorean-triples-using-loops input1))
    ;(newline)
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
