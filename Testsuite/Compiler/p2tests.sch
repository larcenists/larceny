; Compiler tests, designed to exercise Twobit's pass 2.
; These tests should be run at all levels of optimization,
; with all possible settings of compiler switches.
;
; Requires Lib/test.sch.
; Test coverage should be confirmed using Util/stcov.sch.


; To discourage optimizations, some of these tests use an identity
; function that the compiler is unlikely to recognize as the identity.

(define identity
  (let ((n 3))
    (lambda (x)
      (let ((v (make-vector n x)))
        (vector-set! v (- n 1) v)
        (set! n (+ n 1))
        (if (= n 5)
            (set! n 2))
        (if (eqv? x (vector-ref v 1))
            (vector-ref v 0)
            (identity x))))))

; Some of the tests require global variables.

(define *x* 0)
(define *y* 1)
(define *z* 2)

(test "lambda-0"
      (let ((x (identity 3))
            (y (identity 4)))
        ((identity (lambda (z)
                     (+ x (expt y z))))
         5))
      1027)

(test "lambda-1"
      (let ((x (identity 3))
            (y (identity 4)))
        ((identity (lambda (z)
                     (define (f w)
                       (+ x (expt w z)))
                     (f (identity y))))
         5))
      1027)

(test "lambda-2"
      (let ((x (identity 3))
            (y (identity 4)))
        (define (f z)
          (+ x (expt y z)))
        (f (identity 5)))
      1027)

(test "assignment-0"
      (begin
       (set! *x* (identity 0))
       (set! *x* (identity (+ *x* 1)))
       (set! *x* (identity *x*))
       (set! *x* (identity (+ *x* 2)))
       *x*)
      3)

(test "assignment-1"
      (let ((x (identity 3)))
        (set! x (identity (+ x 1)))
        (set! x (identity x))
        (set! x (identity (+ x 1)))
        x)
      5)

(test "assignment-2"
      (let ((x (identity 3))
            (y (identity 4)))
        (set! x (begin (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (set! y (identity (+ y 1)))
                       (identity (+ x (identity y)))))
        (set! x (identity x))
        (set! x (identity (+ x 1)))
        x)
      15)

(test "if-0"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (identity (< x y))
            x
            y))
      3)

(test "if-1"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (identity (> x y))
            x
            y))
      4)

(test "if-2"
      (let ((x 3)
            (y 4))
        (if (< x y)
            17
            19))
      17)

(test "if-3"
      (let ((x 3)
            (y 4))
        (if (< y x)
            17
            19))
      19)

(test "if-4"
      (let ((x 3)
            (y 4))
        (if (if (< x y) #f #f)
            17
            19))
      19)

(test "if-5"
      (let ((x 3)
            (y 4))
        (if (if (< x y) #f #t)
            17
            19))
      19)

(test "if-6"
      (let ((x 3)
            (y 4))
        (if (if (< x y) #t #f)
            17
            19))
      17)

(test "if-7"
      (let ((x 3)
            (y 4))
        (if (if (< x y) #t #t)
            17
            19))
      17)

(test "if-8"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (begin (set! x (identity y))
                   (set! y (+ y y))
                   (= x y))
            17
            19))
      19)

(test "if-9"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (begin (set! x (identity y))
                   (set! y (+ y y))
                   (< x y))
            17
            19))
      17)

(test "if-10"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (not (< x y))
            17
            19))
      19)

(test "if-11"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (if (< x y) #t #t)
            17
            19))
      17)

(test "if-12"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (if (< x y) #t #f)
            17
            19))
      17)

(test "if-13"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (if (< x y) #f #t)
            17
            19))
      19)

(test "if-14"
      (let ((x (identity 3))
            (y (identity 4)))
        (if (if (< x y) #f #f)
            17
            19))
      19)

(test "begin-0"
      (let ((x (identity 3))
            (y (identity 4))
            (z 5))
        (begin (begin 0 1)
               (begin (begin 2)
                      (begin x
                             (begin (identity x)
                                    (+ x y)
                                    (set! x (identity (* x z))))
                             (lambda (x) x)
                             y)
                      (set! y (+ y z)))
               (identity (+ x y))))
      24)

(test "call-0"
      (let ((x (identity 3))
            (y (identity 4))
            (z 5))
        ((begin (set! x (+ x 1))
                (set! x (+ x x))
                (lambda (w)
                  (list x y)))
         (begin (set! y (+ y z))
                (set! y (+ y y))
                (identity y))))
      '(8 18))

(test "call-1"
      (let ((x (identity 3))
            (y (identity 4))
            (z 5))
        ((identity
          (lambda (w)
            (list x y)))
         (begin (set! y (+ y z))
                (set! y (+ y y))
                (identity y))))
      '(3 18))

(test "call-2"
      (memq 'x '(x y . z))
      '(x y . z))

(test "call-3"
      (let ((f (identity
                (lambda (x y z)
                  (list x y z))))
            (a 1)
            (b 20))
        ((begin (set! f (car (f f f f))) f)
         (begin (set! a (+ a 1)) a)
         (begin (set! b (+ b 1)) b)
         (begin (if (positive? a) 99 66))))
      '(2 21 99))

(test "let-0"
      (let ()
        (identity 37))
      37)

(test "let-1"
      (let ((x 37))
        (identity x))
      37)

(test "let-2"
      (let ((list 14))
        ((lambda (x y . z)
           (vector x y z))
         1 2 3 4 5 6 7))
      '#(1 2 (3 4 5 6 7)))

(test "let-3"
      (let ((x (identity 3))
            (y (identity 4)))
        x)
      3)

(test "let-4"
      (let ((x (identity 3))
            (y (identity 4)))
        y)
      4)

(test "let-5"
      (let ((x (identity 3))
            (y (identity 4)))
        (identity 5))
      5)

(test "let-6"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((z (set! x (+ x x)))
              (w (set! y (+ y y))))
          (list x y)))
      '(6 8))

(test "let-7"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((x (lambda (z) (list x z)))
              (y (lambda (w) (list w y))))
          (x (y (identity 5)))))
      '(3 (5 4)))

(test "let-8"
      (let ((x 3)
            (y 4))
        (+ x y))
      7)

(test "let-9"
      (let ((list 14))
        ((lambda (x y . z)
           (vector x y))
         1 2 3 4 5 6 7))
      '#(1 2))

(test "let-10"
      (let ((list 14))
        ((lambda (x y . z)
           (vector x z))
         1 2 3 4 5 6 7))
      '#(1 (3 4 5 6 7)))

(test "let-11"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((x (begin (set! x (+ x (identity 100)))
                        (* x x)))
              (y (lambda (w) (list w y))))
          (y (identity x))))
      '(10609 4))

(test "let-12"
      (let ((x 'a)
            (y '(d a b c)))
        (cons #\a (memq x y)))
      '(#\a a b c))

(test "let-13"
      (let ((x 15)
            (y 16)
            (z (lambda (x) x))
            (w (if (identity #t) 17 4321))
            (a (begin (identity 1) (identity 2)))
            (b (identity 3)))
        (set! x 201)
        (set! y 202)
        (set! z (identity z))
        (set! w 203)
        (set! a 204)
        (set! b 205)
        (list (z x) y w a b))
      '(201 202 203 204 205))

(test "define-0"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((f (identity 5))
              (g (identity 6)))
          (set! f (lambda (z)
                    (g x y z)))
          (set! g (lambda (a b c)
                    (list a b c (+ x y))))
          (f (identity 28))))
      '(3 4 28 7))

(test "define-1"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((f (identity 5))
              (g (identity 6)))
          (set! f (lambda (z)
                    (g x y z)))
          (set! g (lambda (a b c)
                    (list a b c (+ x y))))
          ((identity f) (identity 28))))
      '(3 4 28 7))

(test "define-2"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((f (identity 5))
              (g (identity 6)))
          (set! f (lambda (z)
                    ((identity g) x y z)))
          (set! g (lambda (a b c)
                    (list a b c (+ x y))))
          (f (identity 28))))
      '(3 4 28 7))

(test "define-3"
      (let ((x (identity 3))
            (y (identity 4)))
        (let ((f (identity 5))
              (g (identity 6)))
          (set! f (lambda (z)
                    ((identity g) x y z)))
          (set! g (lambda (a b c)
                    (list a b c (+ x y))))
          ((identity f) (identity 28))))
      '(3 4 28 7))

(test "define-4"
      (let ((x (identity 3))
            (y (identity 4)))
        (define (f a . rest)
          (+ a (car (reverse rest))))
        (list (f 1 2 3 4 5)
              (f 1000 91)))
      '(6 1091))

(test "define-5"
      (let ((a (identity 1))
            (b (identity 2))
            (c (identity 3))
            (d (identity 4))
            (e (identity 5)))
        (define (f z)
          (list z a b c d e))
        (set! b e)
        (set! d a)
        (set! c (+ d d))
        (f (identity 9)))
      '(9 1 5 2 1 5))

(test "define-6"
      (let ((a (identity 1))
            (b (identity 2))
            (c (identity 3))
            (d (identity 4))
            (e (identity 5)))
        (define (f z)
          (list z a b c d e))
        (set! b e)
        (set! d (+ d d))
        (set! c (+ d d))
        (f (identity 9)))
      '(9 1 5 16 8 5))

(test "define-7"
      (let ((a (identity 1))
            (b (identity 2))
            (c (identity 3))
            (d (identity 4))
            (e (identity 5)))
        (define (f z)
          ((identity
            (lambda ()
              (list z a b c d e)))))
        (set! b (identity e))
        (set! d a)
        (set! c (+ d d))
        (f (identity 9)))
      '(9 1 5 2 1 5))

(test "define-8"
      (let* ((k (lambda (v) v))
             (g (lambda (x)
                  (call-with-current-continuation
                   (lambda (again)
                     (set! k
                           (lambda (v)
                             (set! k (lambda (v) v))
                             (again (+ v 1))))
                     x)))))
        (let ((a (identity 1))
              (b (identity 2))
              (c (identity 3))
              (d (identity 4))
              (e (identity 5)))
          (define (f z)
            ((identity
              (lambda ()
                (list z a b c d e)))))
          (set! b (g e))
          (set! d a)
          (set! c (+ d d))
          (k 99)
          (f (identity 9))))
      '(9 1 100 2 1 5))

(test "define-9"
      (let ((foo (lambda (n x y)
                   (define (f n)
                     (if (zero? n) x (g (- n 1))))
                   (define (g n)
                     (if (zero? n) y (f (- n 1))))
                   (f n))))
        (foo 1001 'ab 'ba))
      'ba)

(test "define-10"
      (let ((foo (lambda (n x y)
                   (define (f m)
                     (define (ff n)
                       (if (zero? n) x (g (- n 1))))
                     (ff m))
                   (define (g m)
                     (define (gg n)
                       (if (zero? n) y (f (- n 1))))
                     (gg m))
                   (f n))))
        (foo 1002 'ab 'ba))
      'ab)

(test "define-11"
      (let ((foo (lambda (n x y z w)
                   (define (f n)
                     (if (zero? n) x (g (- n 1))))
                   (define (g n)
                     (if (zero? n) y (h (- n 1))))
                   (define (h n)
                     (if (zero? n) z (i (- n 1))))
                   (define (i n)
                     (if (zero? n) w (f (- n 1))))
                   (f n))))
        (foo 1003 'ab 'ba 'aa 'bb))
      'bb)
