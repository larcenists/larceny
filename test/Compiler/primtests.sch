; Copyright 2000 William Clinger
;
; Compiler/assembler tests, designed to exercise the primitives
; that are introduced by Twobit.  These tests must be run in a
; development environment.
;
; These tests should be run with all possible settings of the
; following compiler switches:
;     runtime-safety-checking
;     integrate-procedures
;     control-optimization
;     lambda-optimization
;     common-subexpression-elimination
;     representation-inference
;     peephole-optimization
;     fill-delay-slots (SPARC Larceny only)
;
; Requires Testsuite/Lib/test.sch.
; Test coverage should be confirmed by inspecting MacScheme machine
; assembly language.  Performance should be confirmed by inspecting
; target machine assembly language.


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

(define *big*
  (do ((n 1 (+ n n)))
      ((not (fixnum? n))
       (quotient n 2))))

(test ".cons"
      (.cons *x* (.cons *y* (.cons *z* '())))
      '(0 1 2))

(test ".cons (Twobit: let with rest argument)"
      (let ((f (identity
                (lambda (n)
                  ((lambda x (cdr x)) 1 2 n 4 5)))))
        (f 33))
      '(2 33 4 5))

(test ".car .cdr"
      (.car (.cdr (identity '(11 12 13))))
      12)

(test ".car .cdr (Twobit: lotsa formals with rest argument)"
      (let ((f (identity
                (lambda ( x1  x2  x3  x4  x5  x6  x7  x8  x9 x10
                         x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                         x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                         x31 x32 x33 x34 x35 x36 x37 x38 x39 . x40)
                  (+ x1
                     (*  x2  x3  x4  x5  x6  x7  x8  x9 x10
                        x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                        x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
                        x31 x32 x33 x34 x35 x36 x37 x38 x39)
                     (car x40))))))
        (f  1  2  3  4  5  6  7  8  9 20
           11 12 13 14 15 16 17 18 19 20
           21 22 23 24 25 26 27 28 29 20
           31 32 33 34 35 36 37 38 39 40))
      27197176108263257811520375653203863142400000041)

(test ".car:pair cdr:pair"
      (.car:pair (identity (.cdr:pair (identity '(a b c d e)))))
      'b)

(test ".car:pair cdr:pair (Twobit)"
      (letrec ((f (lambda (x y)
                    (if (and (pair? x) (pair? y))
                        (cons (list (car x) (car y))
                              (f (cdr x) (cdr y)))
                        '()))))
        (f (identity '(a b c d))
           (identity '(1 2 3 4))))
      '((a 1) (b 2) (c 3) (d 4)))

(test ".make-cell .cell-ref .cell-set! .cell-set!:nwb"
      (let ((c1 (.make-cell *x*))
            (c2 (.make-cell *z*)))
        (.cell-set!:nwb c1 1)
        (.cell-set! c2 (* (.cell-ref c2) (.cell-ref c2)))
        (.cell-set! c2 (* (.cell-ref c2) (.cell-ref c2)))
        (.cell-set! c2 (* (.cell-ref c2) (.cell-ref c2)))
        (+ (.cell-ref c1) (.cell-ref c2)))
      257)

(test ".make-cell .cell-ref .cell-set! (Twobit: mutable locals)"
      (let ((f (identity
                (lambda (n)
                  (define x n)
                  (define y 1)
                  (define (loop)
                    (if (positive? x)
                        (begin (set! y (* x y))
                               (set! x (- x 1))
                               (loop))))
                  (loop)
                  y))))
        (f 5))
      120)

(test ".unspecified"
      (.unspecified)
      (.unspecified))

(test ".unspecified (Twobit: if)"
      (if (identity #f) 37)
      (.unspecified))

(test ".undefined"
      (.undefined)
      (.undefined))

(test ".fixnum?"
      (let ((f (identity
                (lambda (x)
                  (map (lambda (a) (.fixnum? a)) x)))))
        (f (list -3  0  1  4 37 (expt 2 100) 0.0 2/3 +3+4i 'a '#() " " 45 +)))
      '(         #t #t #t #t #t      #f        #f #f   #f  #f  #f   #f #t #f))

(test ".symbol?"
      (map (lambda (x) (.symbol? x))
           (identity
            '(-3 47 3.14 duh "a" #())))
      '(      #f #f  #f   #t  #f  #f))

(test ".char?"
      (map (lambda (x) (.char? x))
           (identity
            '(-3 47 #\5 duh "a" #())))
      '(      #f #f  #t  #f  #f  #f))

(test ".char->integer"
      (map (lambda (x) (.char->integer x))
           (identity (string->list "Yeah, right!")))
      '(89 101 97 104 44 32 114 105 103 104 116 33))

(test ".fixnum? .symbol? .char? .char->integer (Twobit: case)"
      (let ((f (identity
                (lambda (x)
                  (case x
                    ((3 y #\r) 1)
                    ((4 z)     2)
                    ((x z)     3)
                    ((#\a #\c) 4)
                    ((#\b 1)   5)
                    (else      6))))))
        (map f '(1 2 3 4 x y z #\a #\b #\c #\d #\r)))
      '(         5 6 1 2 3 1 2  4   5   4   6   1))

(test ".--"
      (map (identity (lambda (x) (.-- x)))
           '(-17 -12 -4 -1 0  1  3456  3.25 -3/4 +3-4i -3.0-4.5i))
      '(      17  12  4  1 0 -1 -3456 -3.25  3/4 -3+4i  3.0+4.5i))

(test ".check!"
      (begin (.check! (identity #t) 17 'a)
             'ok)
      'ok)

(test ".vector-length:vec .vector-ref:trusted .vector-set!:trusted"
      (let ((f (identity
                (lambda (v1 v2)
                  (.vector-set!:trusted v2 2 (.vector-ref:trusted v1 1))
                  (.vector-set!:trusted v2 3 (.vector-ref:trusted v1 2))
                  (.vector-set!:trusted v2 4 (.vector-ref:trusted v1 3))
                  (list (.vector-length:vec v1)
                        (.vector-length:vec v2)
                        v1
                        v2)))))
        (f (list->vector '(w x y z))
           (list->vector '(a b c d e f g h))))
      '(4 8 #(w x y z) #(a b x y z f g h)))

(test ".vector-set!:trusted:nwb"
      (let ((v (identity (vector 'a 'b 'c 'd 'e))))
        (.vector-set!:trusted:nwb v 0 300)
        (.vector-set!:trusted:nwb v 4 304)
        v)
      '#(300 b c d 304))

(test "trusted vector primitives (Twobit)"
      (let ((f (lambda (v)
                 (let ((n (vector-length v)))
                   (let loop ((i 0))
                     (if (< i n)
                         (begin (vector-set! v i '*)
                                (loop (+ i 1)))))
                   (let loop ((i 2))
                     (if (< i n)
                         (begin (vector-set! v i i)
                                (loop (+ i 1))))))
                 v)))
        (let ((v1 (make-vector 2))
              (v2 (make-vector 6)))
          (f v1)
          (f v2)
          (list v1 v2)))
      '(#(* *) #(* * 2 3 4 5)))

(test ".string-length:str .string-ref:trusted .string-set!:trusted"
      (let ((f (identity
                (lambda (v1 v2)
                  (.string-set!:trusted v2 2 (.string-ref:trusted v1 1))
                  (.string-set!:trusted v2 3 (.string-ref:trusted v1 2))
                  (.string-set!:trusted v2 4 (.string-ref:trusted v1 3))
                  (list (.string-length:str v1)
                        (.string-length:str v2)
                        v1
                        v2)))))
        (f (string-append "wxyz")
           (string-append "abcdefgh")))
      '(4 8 "wxyz" "abxyzfgh"))

(test "trusted string primitives (Twobit)"
      (let ((f (lambda (v)
                 (let ((n (string-length v)))
                   (let loop ((i 0))
                     (if (< i n)
                         (begin (string-set! v i #\*)
                                (loop (+ i 1)))))
                   (let loop ((i 2))
                     (if (< i n)
                         (begin (string-set! v i #\a)
                                (loop (+ i 1))))))
                 v)))
        (let ((v1 (make-string 2))
              (v2 (make-string 6)))
          (f v1)
          (f v2)
          (list v1 v2)))
      '("**" "**aaaa"))

(test ".+:idx:idx"
      (map (identity (lambda (x y) (.+:idx:idx x y)))
           '(  0   0   0   1   1   1  30  30  30)
           '(  0   1  25  75   0   1  25  75   1))
      '(       0   1  25  76   1   2  55 105  31))

(test ".-:idx:idx"
      (map (identity (lambda (x y) (.-:idx:idx x y)))
           '(  0   0   0   1   1   1  30  30  30)
           '(  0   1  25  75   0   1  25  75   1))
      '(       0  -1 -25 -74   1   0   5 -45  29))

(test ".+:idx:idx with immediate"
      (map (identity (lambda (x) (.+:idx:idx x 1)))
           '(  0   1  30))
      '(       1   2  31))

(test ".-:idx:idx with immediate"
      (map (identity (lambda (x) (.-:idx:idx x 2)))
           '(  0   1  25))
      '(      -2  -1  23))

(test "index primitives (Twobit)"
      (let ((f (lambda (s0)
                 (let* ((n (string-length s0))
                        (s1 (make-string n))
                        (s2 (make-string n)))
                   (let loop ((i 0) (j (- n 1)))
                     (if (< i n)
                         (begin (string-set! s1 i (string-ref s0 i))
                                (string-set! s2 j (string-ref s0 i))
                                (loop (+ i 1) (- j 1)))))
                   (string-append s0 s1 s2)))))
        (f "12345"))
      "123451234554321")

(test ".+:fix:fix"
      (map (identity (lambda (x y) (.+:fix:fix x y)))
           (list (- *big*) (- *big*) 0 50 *big* *big*)
           (list (- *big*)    *big* 75 30     1 *big*))
      (list (- (* 2 *big*))     0   75 80 (+ *big* 1) (* 2 *big*)))

(test ".-:fix:fix"
      (map (identity (lambda (x y) (.-:fix:fix x y)))
           (list (- *big*) (- *big*) 0 50 *big* *big*)
           (list (- *big*)    *big* 75 30     1 *big*))
      (list      0 (- (* 2 *big*)) -75 20 (- *big* 1) 0))

(test ".=:fix:fix .<:fix:fix etc"
      (let ((args1 (list (- *big*) (- *big*) (- *big*)
                         0 0 0
                         *big* *big* *big*))
            (args2 (list (- *big*) 0 *big*
                         (- *big*) 0 *big*
                         (- *big*) 0 *big*)))
        (list (map (identity (lambda (x y) (.=:fix:fix x y))) args1 args2)
              (map (identity (lambda (x y) (.<:fix:fix x y))) args1 args2)
              (map (identity (lambda (x y) (.<=:fix:fix x y))) args1 args2)
              (map (identity (lambda (x y) (.>:fix:fix x y))) args1 args2)
              (map (identity (lambda (x y) (.>=:fix:fix x y))) args1 args2)))
      '((#t #f #f #f #t #f #f #f #t)
        (#f #t #t #f #f #t #f #f #f)
        (#t #t #t #f #t #t #f #f #t)
        (#f #f #f #t #f #f #t #t #f)
        (#t #f #f #t #t #f #t #t #t)))
