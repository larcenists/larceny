; Copyright 1998 William Clinger
;
; $Id$
;
; Compiler tests, designed to exercise Twobit's pass 4.
; These tests should be run at all levels of optimization,
; with all possible settings of compiler switches.
;
; Requires Testsuite/Lib/test.sch.
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

(test "lambda (tail)"
      (let* ((f (identity
                 (lambda (x)
                   (lambda ()
                     x))))
             (g (f 17)))
        (g))
      17)

(test "lambda (rest argument)"
      (let ((f (identity
                (lambda (x y . z)
                  (cons x (cons y z))))))
        (f 1 2 3 4 5))
      '(1 2 3 4 5))

(test "lambda (lotsa formals)"
      (let ((f (identity
                (lambda ( x1  x2  x3  x4  x5  x6  x7  x8  x9 x10
                         x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                         x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40)
                  (+ x1
                     (*  x2  x3  x4  x5  x6  x7  x8  x9 x10
                        x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                        x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                        x31 x32 x33 x34 x35 x36 x37 x38 x39)
                     x40)))))
        (f  1  2  3  4  5  6  7  8  9 20
           11 12 13 14 15 16 17 18 19 20
           21 22 23 24 25 26 27 28 29 20
           31 32 33 34 35 36 37 38 39 40))
      27197176108263257811520375653203863142400000041)

(test "assignment (tail)"
      (let* ((x 0)
             (f (identity
                 (lambda ()
                   (set! x (+ x x))))))
        (set! x 27)
        (f)
        x)
      54)

(test "assignment (global, tail)"
      (let* ((x 0)
             (f (identity
                 (lambda ()
                   (set! *x* (+ x x))))))
        (set! x 27)
        (f)
        *x*)
      54)

(test "cg-sort-vars-1"
      (let ((x (identity 3))
            (y (identity 4))
            (z (identity 5)))
        (let ((f (identity
                  (lambda ()
                    (list x y z)))))
          (f)))
      '(3 4 5))

(test "cg-eval-vars-1"
      (let ((x1 (identity 1))
            (x2 (identity 2))
            (x3 (identity 3))
            (x4 (identity 4))
            (x5 (identity 5))
            (x6 (identity 6))
            (x7 (identity 7))
            (x8 (identity 8))
            (x9 (identity 9))
            (x10 (identity 10))
            (x11 (identity 11))
            (x12 (identity 12))
            (x13 (identity 13))
            (x14 (identity 14))
            (x15 (identity 15))
            (x16 (identity 16))
            (x17 (identity 17))
            (x18 (identity 18))
            (x19 (identity 19))
            (x20 (identity 20))
            (x21 (identity 21))
            (x22 (identity 22))
            (x23 (identity 23))
            (x24 (identity 24))
            (x25 (identity 25))
            (x26 (identity 26))
            (x27 (identity 27))
            (x28 (identity 28))
            (x29 (identity 29))
            (x30 (identity 30))
            (x31 (identity 31))
            (x32 (identity 32))
            (x33 (identity 33))
            (x34 (identity 34))
            (x35 (identity 35))
            (x36 (identity 36))
            (x37 (identity 37))
            (x38 (identity 38))
            (x39 (identity 39))
            (x40 (identity 40)))
        (let ((f (identity
                  (lambda ()
                    (list  x1  x2  x3  x4  x5  x6  x7  x8  x9 x10
                          x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                          x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                          x31 x32 x33 x34 x35 x36 x37 x38 x39 x40)))))
          (f)))
      '( 1  2  3  4  5  6  7  8  9 10
        11 12 13 14 15 16 17 18 19 20
        21 22 23 24 25 26 27 28 29 30
        31 32 33 34 35 36 37 38 39 40))

(test "cg-eval-vars-2"
      (let ((x1 (identity 1))
            (x2 (identity 2))
            (x3 (identity 3))
            (x4 (identity 4))
            (x5 (identity 5))
            (x6 (identity 6))
            (x7 (identity 7))
            (x8 (identity 8))
            (x9 (identity 9))
            (x10 (identity 10))
            (x11 (identity 11))
            (x12 (identity 12))
            (x13 (identity 13))
            (x14 (identity 14))
            (x15 (identity 15))
            (x16 (identity 16))
            (x17 (identity 17))
            (x18 (identity 18))
            (x19 (identity 19))
            (x20 (identity 20))
            (x21 (identity 21))
            (x22 (identity 22))
            (x23 (identity 23))
            (x24 (identity 24))
            (x25 (identity 25))
            (x26 (identity 26))
            (x27 (identity 27))
            (x28 (identity 28))
            (x29 (identity 29))
            (x30 (identity 30))
            (x31 (identity 31))
            (x32 (identity 32))
            (x33 (identity 33))
            (x34 (identity 34))
            (x35 (identity 35))
            (x36 (identity 36))
            (x37 (identity 37))
            (x38 (identity 38))
            (x39 (identity 39))
            (x40 (identity 40)))
        (let ((f (identity
                  ((identity
                    (lambda ()
                      (lambda ()
                        (list  x1  x2  x3  x4  x5  x6  x7  x8  x9 x10
                              x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                              x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                              x31 x32 x33 x34 x35 x36 x37 x38 x39 x40))))))))
          (f)))
      '( 1  2  3  4  5  6  7  8  9 10
        11 12 13 14 15 16 17 18 19 20
        21 22 23 24 25 26 27 28 29 30
        31 32 33 34 35 36 37 38 39 40))

(test "cg-body-1"
      (let ((f (lambda (x y)
                 (define (g z)
                   (list x z))
                 (lambda (w)
                   (g (+ y w))))))
        ((f 3 4) 5))
      '(3 9))

(test "cg-body-2"
      (let ((f (lambda (x y)
                 (define (g z)
                   (list x z))
                 (lambda w
                   (g (+ y ((identity car) w)))))))
        ((f 3 4) 5))
      '(3 9))

(test "cg-body-3"
      (cadr ((let ((x (identity 3))
                   (y (identity 4)))
               (define (g z)
                 (list x z))
               (lambda w
                 (g (+ y ((identity car) w)))))
             19))
      23)

(test "cg-body-4"
      (let ((x (identity 3))
            (y (identity 4)))
        (define (f z)
          ((identity expt) x z))
        (f (identity 5)))
      243)

(test "cg-body-5"
      (let ((x (identity 3))
            (y (identity 4)))
        (+ 2
           (let ()
             (define (f z)
               ((identity expt) x z))
             (f (identity 5)))))
      245)

(test "cg-variable-1"
      (begin (set! *x* 97)
             *x*)
      97)

(test "cg-sequential-loop-1"
      (let ()
        (define (f)
          (identity (begin)))
        (f)
        17)
      17)

(test "cg-sequential-loop-2"
      (let ()
        (define (f)
          (list (identity (begin))))
        (identity (f))
        17)
      17)

