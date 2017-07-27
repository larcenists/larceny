;;; Vector benchmark for (scheme vector).

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme vector))

(define (symbols n)
  (vector-map string->symbol
              (vector-map (lambda (s) (string-append "x" s))
                          (vector-map
                           number->string
                           (vector-unfold (lambda (i x) (values i x))
                                          n
                                          'ignored)))))

(define (powerset universe)
  (define (ps j)
    (if (= j (vector-length universe))
        (vector '#())
        (let* ((x (vector-ref universe j))
               (pu2 (ps (+ j 1))))
          (vector-append
           pu2
           (vector-map (lambda (y)
                         (vector-append-subvectors
                          universe j (+ j 1) y 0 (vector-length y)))
                       pu2)))))
  (ps 0))

(define (permutations universe)
  (define (permutations j)
    (if (= j (vector-length universe))
        (vector '#())
        (let* ((x (vector-ref universe j))
               (perms2 (permutations (+ j 1))))
          (vector-concatenate
           (vector->list
            (vector-map
             (lambda (perm)
               (let ((n (vector-length perm)))
                 (vector-map (lambda (i)
                               (vector-append-subvectors
                                perm 0 i
                                universe j (+ j 1)
                                perm i (vector-length perm)))
                             (vector-unfold (lambda (i seed)
                                              (values seed (+ seed 1)))
                                            (+ n 1)
                                            0))))
             perms2))))))
  (permutations 0))

(define (go n)
  (let* ((universe (symbols n))
         (subsets (powerset universe))
         (perms (permutations universe)))
    (call-with-values
     (lambda ()
       (vector-partition (lambda (perm)
                           (vector-index (lambda (v)
                                           (equal? v perm))
                                         subsets))
                         perms))
     (lambda (v n)
       (vector-copy v 0 n)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "vector"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
