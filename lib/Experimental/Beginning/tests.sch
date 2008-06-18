; Tests for the beginning language.

(define (test pgm expected)
  (check-beginning-program pgm)
  (let ((results (interpret-beginning-program pgm)))
    (if (not (equal? results expected))
        (begin (display "***** ERROR *****")
               (newline)
               (pretty-print pgm)
               (newline)
               (display "Expected:")
               (newline)
               (pretty-print expected)
               (newline)
               (display "Actual:")
               (newline)
               (pretty-print results)
               (newline)
               (newline)))))


(define test1
  '((#\a) (#\a)))

(define test2
  '(("abc") ("abc")))

(define test3
  '((false) (#f)))

(define test4
  '((true) (#t)))

(define test5
  '((34) (34)))

(define test6
  '(('xyz) (xyz)))

(define test7
  '((empty) (())))

(define test8
  '(((define x 17) x)
    (17)))

(define test11
  '(((define (f n)
       (if (< n 2)
           n
           (+ (f (- n 1))
              (f (- n 2)))))
     (f 10))
    (55)))

(define test12
  '(((define f
       (lambda (n)
         (if (= n 0)
             1
             (* n (f (- n 1))))))
     (define input 5)
     (f input))
    (120)))

(define test13
  '(((define (f n)
       (cond ((= n 0)
              0)
             ((= n 1)
              1)
             ((and (or (odd? n) (even? n))
                   (positive? n))
              (g (f (- n 1))
                 (f (- n 2))))
             (else 0)))
     (define g
       (lambda (x y)
         (if (zero? x)
             y
             (g (- x 1) (+ y 1)))))
     (f 10))
    (55)))

(define test14
  '(((define (f n)
       (cond ((= n 0)
              0)
             ((= n 1)
              1)
             ((and (or (odd? n) (even? n))
                   (positive? n))
              (g (f (- n 1))
                 (f (- n 2))))
             (else 0)))
     (define g
       (lambda (x y)
         (if (zero? x)
             y
             (g (- x 1) (+ y 1)))))
     (f 20))
    (6765)))

(define tests
  (list test1 test2 test3 test4 test5 test6 test7 test8
        test11 test12 test13 test14))

(for-each (lambda (t) (apply test t))
          tests)
