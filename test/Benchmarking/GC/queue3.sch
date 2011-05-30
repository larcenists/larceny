; Queue-like object lifetimes.
;
; (queue-benchmark m n k) allocates m lists of length n,
; storing them in a circular buffer that holds k of the lists.
;
; The lists are build out of two-element vectors, which
; Larceny represents with a header word and one word of
; padding.  That representation allows fairer comparisons
; with Java.

(define default-m 1000)
(define default-n 1000000)
(define default-k 10)

(define (queue-benchmark . rest)
  (let ((m (if (null? rest) default-m (car rest)))
        (n (if (or (null? rest) (null? (cdr rest)))
               default-n
               (cadr rest)))
        (k (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
               default-k
               (caddr rest))))
    (run-benchmark (string-append "queue"
                                  (number->string m)
                                  ":"
                                  (number->string n)
                                  ":"
                                  (number->string k))
                   1
                   (lambda () (queue-test m n k))
                   (lambda (x) #t))))

(define (queue-test m n k)
  (let ((q (make-vector k #f)))
    (define (loop m i)
      (cond ((fx=? m 0)
             (mylength (vector-ref q 0)))
            ((fx=? i k)
             (loop m 0))
            (else
             (vector-set! q i (make-list n))
             (loop (fx- m 1) (fx+ i 1)))))
    (define (make-list n)
      (do ((n n (fx- n 1))
           (r #f (vector n r)))
          ((fx=? n 0) r)))
    (define (mylength v)
      (do ((v v (vector-ref v 1))
           (r 0 (fx+ r 1)))
          ((not (vector? v))
           r)))
    (loop m 0)))


