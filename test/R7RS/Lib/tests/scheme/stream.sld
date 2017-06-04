;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme stream) procedures:
;;;
;;;     stream-null
;;;     stream-cons
;;;     stream?
;;;     stream-null?
;;;     stream-pair?
;;;     stream-car
;;;     stream-cdr
;;;     stream-lambda
;;;     define-stream
;;;     list->stream
;;;     port->stream
;;;     stream
;;;     stream->list
;;;     stream-append
;;;     stream-concat
;;;     stream-constant
;;;     stream-drop
;;;     stream-drop-while
;;;     stream-filter
;;;     stream-fold
;;;     stream-for-each
;;;     stream-from
;;;     stream-iterate
;;;     stream-length
;;;     stream-let
;;;     stream-map
;;;     stream-match
;;;     stream-of
;;;     stream-range
;;;     stream-ref
;;;     stream-reverse
;;;     stream-scan
;;;     stream-take
;;;     stream-take-while
;;;     stream-unfold
;;;     stream-unfolds
;;;     stream-zip


(define-library (tests scheme stream)
  (export run-stream-tests)
  (import (scheme base)
          (scheme stream)
          (tests scheme test))

  ;; Adapted from srfi-41-test.sps7 and examples in SRFI 41.

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define (run-stream-tests)

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
                ((> n 228))
              (do ((a 1 (+ a 1)))
                  ((> a n))
                (do ((b a (+ b 1)))
                    ((> b n))
                  (let ((c (- n a b)))
                    (if (= (+ (* a a) (* b b)) (* c c))
                        (begin (set! count (+ count 1))
                               (if (> count nth)
                                   (return (list a b c)))))))))))))

     (test (pythagorean-triples-using-streams 50)
           (pythagorean-triples-using-loops 50))

     (test-assert (eq? stream-null stream-null))

     (test-assert (stream? stream-null))

     (test-assert (stream? (stream-cons 'x stream-null)))

     (test-assert (stream-null? stream-null))

     (test-deny (stream-null? (stream-cons 'x stream-null)))

     (test-deny (stream-pair? stream-null))

     (test-assert (stream-pair? (stream-cons 'x stream-null)))

     (let* ((x '*)
            (y '*)
            (s (stream-cons x (stream-cons y stream-null))))
       (set! x 'one)
       (set! y 'two)
       (test (stream-car s) 'one)
       (set! x 'three)
       (test (stream-car s) 'one)
       (test (stream-car (stream-cdr s)) 'two)
       (set! y 'four)
       (test (stream-car (stream-cdr s)) 'two))

     (test/exn (stream-car '(x)) &error)
     (test/exn (stream-cdr '(x)) &error)

     ;; From examples in SRFI 41

     (let ()

       (define strm123
         (stream-cons 1
                      (stream-cons 2
                                   (stream-cons 3
                                                stream-null))))

       (define iter
         (stream-lambda (f x)
                        (stream-cons x (iter f (f x)))))

       (define nats (iter (lambda (x) (+ x 1)) 0))

       (define stream-add
         (stream-lambda (s1 s2)
                        (stream-cons
                         (+ (stream-car s1) (stream-car s2))
                         (stream-add (stream-cdr s1)
                                     (stream-cdr s2)))))

       (define evens (stream-add nats nats))

       (test (stream-car strm123) 1)

       (test (stream-car (stream-cdr strm123)) 2)

       (test (stream-pair?
              (stream-cdr
               (stream-cons (/ 1 0) stream-null)))
             #f)

       (test (stream? (list 1 2 3)) #f)

       (test (stream-car (stream-cdr nats)) 1)

       (test (stream-car evens) 0)

       (test (stream-car (stream-cdr evens)) 2)

       (test (stream-car (stream-cdr (stream-cdr evens))) 4))

     (let ()

       (define s1 (list->stream '(1 2 3 4 5)))

       (define-stream (stream-map proc strm)
         (if (stream-null? strm)
             stream-null
             (stream-cons
               (proc (stream-car strm))
               (stream-map proc (stream-cdr strm)))))

       (define s2 (stream-map - s1))

       (test (stream->list s2) '(-1 -2 -3 -4 -5)))

     (test (stream->list (port->stream (open-input-string "abcd")))
           (string->list "abcd"))

     (let ((s (stream 1 (/ 2 0) 3)))
       (test (stream-car s) 1)
       (test (stream-car (stream-cdr (stream-cdr s))) 3)
       (test/exn (stream-car (stream-cdr s)) &error))

     (test (stream->list 10
                         (stream-map (lambda (x) (* x x))
                                     (stream-from 0)))
           '(0 1 4 9 16 25 36 49 64 81))

     (test (stream->list 5 (stream-append (stream-from 20)
                                          (stream-from 0)))
           '(20 21 22 23 24))

     (test (stream->list 7 (stream-append (list->stream '(20 21))
                                          (stream-from 0)))
           '(20 21 0 1 2 3 4))

     (test (stream->list 5 (stream-concat (stream (stream-from 20)
                                                  (stream-from 0))))
           '(20 21 22 23 24))

     (test (stream->list 7 (stream-concat (stream (list->stream '(20 21))
                                                  (stream-from 0))))
           '(20 21 0 1 2 3 4))

     (test (stream->list 8 (stream-constant 'la 'di 'da))
           '(la di da la di da la di))

     (test-assert (stream-null? (stream-drop 0 stream-null)))

     (test-assert (stream-null? (stream-drop 6 stream-null)))

     (test-assert (stream-null? (stream-drop 3 (stream 1 2))))

     (test (stream->list 4 (stream-drop 300 (stream-from 0)))
           '(300 301 302 303))

     (test (stream->list 5 (stream-drop-while (lambda (n) (= n (* n n)))
                                              (stream-from 0)))
           '(2 3 4 5 6))

     (test-assert (stream? (stream-drop-while char? (stream-from 0))))

     (test (stream->list 2 (stream-filter (lambda (n) (= n (* n n)))
                                          (stream-from 0)))
           '(0 1))

     (let ((s (stream 0 50 99 24 78 13 8 82)))
       (test (stream-fold max (stream-car s) (stream-cdr s)) 99))

     (let ((n 0))
       (test/unspec (stream-for-each (lambda (x) (set! n (+ n x)))
                                     (stream 1 2 3 4 5 6 7 8 9 10)))
       (test n 55)
       (test/unspec (stream-for-each (lambda (x y) (set! n (+ n (* x y))))
                                     (stream 2 2 2)
                                     (stream-from 100)))
       (test n (+ 55 200 202 204))
       (test/unspec (stream-for-each (lambda (x y z) (set! n (+ n (* x y z))))
                                     (stream-from 200)
                                     (stream 2 2 2)
                                     (stream 1 1)))
       (test n (+ 55 200 202 204 400 402)))

     (test (stream->list 10 (stream-from 0 3))
           '(0 3 6 9 12 15 18 21 24 27))

     (test (stream->list 6 (stream-iterate (lambda (x) (+ x x)) 3))
           '(3 6 12 24 48 96))

     (test (stream-length stream-null) 0)

     (test (stream-length (stream 1 2 3 4)) 4)

     (test (stream->list 4
                         (stream-let f ((s1 (stream-from 0 4))
                                        (s2 (stream-from 8)))
                           (if (> (stream-car s1) (stream-car s2))
                               s1
                               (f (stream-cdr s1) (stream-cdr s2)))))
           '(12 16 20 24))

     (test (stream->list (stream-map * (stream 3 4 5)
                                       (stream-from 50)
                                       (stream-from 1)))
           '(150 408 780))

     (test (stream->list (stream-map * (stream-from 50)
                                       (stream 3 4 5)
                                       (stream-from 1)))
           '(150 408 780))

     (test (stream->list 3 (stream-map list (stream-from 50)))
           '((50) (51) (52)))

     (test (stream-match (stream)
             (()                0)
             ((x y z)           z)
             ((x y . _) (= x y) x)
             (else        (/ 1 0)))
           0)

     (test (stream-match (stream 20 30 40)
             (()                0)
             ((x y z)           z)
             ((x y . _) (= x y) x)
             (else        (/ 1 0)))
           40)

     (test (stream-match (stream-from 20 0)
             (()                0)
             ((x y z)           z)
             ((x y . _) (= x y) x)
             (else        (/ 1 0)))
           20)

     (test (stream->list (stream-match (stream 20 30 40 50)
                           (()                (stream-from 100))
                           ((x y z)           (stream-from 50))
                           ((x y . _) (= x y) (stream-from 0))
                           (else (stream-cdr else))))
           '(30 40 50))

     (test (stream->list (stream-of (* x x) (x in (stream-range 4 8))))
           '(16 25 36 49))

     (test (stream->list 5 (stream-of (* x x) (x in (stream-from 0))))
           '(0 1 4 9 16))

     (test (stream->list (stream-of (+ x y)
                                    (x in (stream-range 100 110))
                                    (y in (stream-range (+ x x) (+ x x x)))
                                    (z is (+ y 2))
                                    ((lambda (n) (< n 210)) z)))
           '(300 301 302 303 304 305 306 307    ; x = 100, y = 200, 201, ...
             303 304 305 306 307 308            ; x = 101, y = 202, 203, ...
             306 307 308 309                    ; x = 102, y = 204, 205, ...
             309 310))                          ; x = 103, y = 206, 207, ...

     (test (stream->list (stream-range 10 15))
           '(10 11 12 13 14))

     (test (stream->list (stream-range 10 5))
           '(10 9 8 7 6))

     (test (stream->list 5 (stream-range 10 +inf.0 2))
           '(10 12 14 16 18))

     (test (stream->list 5 (stream-cdr (stream-range 10 500 3.0)))
           '(13.0 16.0 19.0 22.0 25.0))

     (test (stream->list 5 (stream-range 10.0 500 3))
           '(10.0 13.0 16.0 19.0 22.0))

     (test (stream-ref (stream-from 100) 50) 150)

     (test/exn (stream-ref (stream 1 2 3) 50) &error)

     (let* ((s0 (stream 3 4 5))
            (s1 (stream-map / s0 (stream 1 0 1)))
            (s2 (stream-reverse s1)))
       (test (stream-length s2) 3)
       (test (stream-ref s2 0) 5)
       (test/exn (stream-ref s2 1) &error)
       (test (stream-ref s2 2) 3))

     (test (stream->list 10 (stream-scan * 1 (stream-from 1)))
           '(1 1 2 6 24 120 720 5040 40320 362880))

     (test (stream->list (stream-take 8 (stream-from 101)))
           '(101 102 103 104 105 106 107 108))

     (test (stream->list (stream-take 8 (stream 101 102 103)))
           '(101 102 103))

     (test (stream->list (stream-take-while odd? (stream-from 101)))
           '(101))

     (test (stream->list (stream-take-while integer? (stream 101 102 103)))
           '(101 102 103))


     (test (stream->list (stream-unfold (lambda (x) (expt x 3))
                                        (lambda (x) (< x 7))
                                        (lambda (x) (+ x 2))
                                        0))
           '(0 8 64 216))

     (let ()
       (define (stream-partition pred? strm)
         (stream-unfolds
           (lambda (s)
             (if (stream-null? s)
                 (values s '() '())
                 (let ((a (stream-car s))
                       (d (stream-cdr s)))
                   (if (pred? a)
                       (values d (list a) #f)
                       (values d #f (list a))))))
           strm))

       (test (call-with-values
              (lambda ()
                (stream-partition odd? (stream-range 1 6)))
              (lambda (odds evens)
                (list (stream->list odds)
                      (stream->list evens))))
             '((1 3 5) (2 4))))

     (test (stream->list 5 (stream-zip (stream-from 1) (stream-from 20)))
           '((1 20) (2 21) (3 22) (4 23) (5 24))))))
