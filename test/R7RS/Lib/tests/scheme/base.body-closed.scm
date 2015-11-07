;;; 

(define (run-base-exact-closed-tests)

  (cond-expand
   (exact-closed
    (test (rational? 6/10)                      #t)
    (test (rational? 6/3)                       #t)))

  (cond-expand
   (exact-closed
    (test (integer? 8/4)                        #t)))

  (cond-expand
   (exact-closed
    (test (exact-integer? 32/5)                 #f)))

  (for-each
   (lambda (x y)
     (let ((try-one
            (lambda (x y)
              (let ((try-x
                     (lambda (x x2)
                       (test (= x x2) #t)
                       (test (< x x2) #f)
                       (test (> x x2) #f)
                       (test (<= x x2) #t)
                       (test (>= x x2) #t))))
                (try-x x x)
                (when (exact? x)
                  (try-x x (inexact x))
                  (try-x (inexact x) x)))
              (test (< x y) #t)
              (test (<= x y) #t)
              (test (> x y) #f)
              (test (>= x y) #f)
              (test (< y x) #f)
              (test (<= y x) #f)
              (test (> y x) #t)
              (test (>= y x) #t))))
       (try-one x y)
       (try-one (inexact x) y)
       (try-one x (inexact y))
       (try-one (inexact x) (inexact y))))
   (list 1/2 1 3/2   (expt 2 100) (expt 2 100))
   (list 1   2 51/20 (expt 2 102) (/ (* 4 (expt 2 100)) 3)))

  (test (/ 3 4 5)                               3/20)
  (test (/ 2 3)                                 2/3)
  (test (/ 3)                                   1/3)

  (test (abs (- (expt 2 100)))                   (expt 2 100))

  (divmod-test +3 +5/6)
  (divmod-test -3 +5/6)
  (divmod-test +3 -5/6)
  (divmod-test -3 -5/6)

  (divmod-test +3 +7/11)
  (divmod-test -3 +7/11)
  (divmod-test +3 -7/11)
  (divmod-test -3 -7/11)

  (test (numerator (/ 6 4))                     3)
  (test (denominator (/ 6 4))                   2)

  (test (round 7/2)                             4)

  (test (rationalize (/ 3 10) 1/10)           1/3)
  (test/approx (rationalize
                (inexact (/ 3 10))
                1/10)
               (inexact (/ 1 3)))

  (test (sqrt 36/49)                          6/7)

  (test (expt 5 -3)                         1/125)
  (test (expt 1/2 -4)                          16)
  (test (expt 4/3 3)                        64/27)

  (for-each 
   (lambda (n)
     (test (string->number (number->string n)) n)
     (test (string->number (number->string (inexact n) 10 5)) (inexact n))
     (when (exact? n)
       (test (string->number (number->string n 16) 16) n)
       (test (string->number (string-append "#x" (number->string n 16))) n)
       (test (string->number (number->string n 8) 8) n)
       (test (string->number (string-append "#o" (number->string n 8))) n)
       (test (string->number (number->string n 2) 2) n)
       (test (string->number (string-append "#b" (number->string n 2))) n)
       (test (string->number (number->string n 10) 10) n)
       (test (string->number (string-append "#d" (number->string n 10)))
                n)))
   '(1 15 1023 -5 1/2 #e2e200))

  ;; Originally from Ikarus:
  (test-string-to-number
   ("+13476238746782364786237846872346782364876238477" 
    13476238746782364786237846872346782364876238477)
   ("1/2" (/ 1 2))
   ("-1/2" (/ 1 -2))
   ("#b-00000110110/10" -27)
   ("#e#b-00000110110/10" -27)
   ("#b#e-00000110110/10" -27))

   )
