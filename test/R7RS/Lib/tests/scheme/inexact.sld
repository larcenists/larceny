;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme inexact) procedures:
;;;
;;;     acos
;;;     asin
;;;     atan
;;;
;;;     cos
;;;     sin
;;;     tan
;;;
;;;     exp
;;;     log
;;;
;;;     finite?
;;;     infinite?
;;;     nan?
;;;
;;;     sqrt
;;;
;;; Also tests many procedures from (scheme base), since base.sld
;;; doesn't test any procedures on inexact arguments.


(define-library (tests scheme inexact)
  (export run-inexact-tests)
  (import (scheme base)
          (scheme inexact)
          (tests scheme test))

  (begin

   (define pi-approx (/ 103993 33102))

   (define (try-reals f but-not)
     (if (not (member 0 but-not))
         (f 0))
     (f -1.0)
     (f 0.0)
     (f 1.0)
     (f 1/2)                       ; FIXME: ratios
     (f (expt 2 30))
     (f (expt 2 60))
     (f (expt 2 90))
     (f (- (expt 2 90)))
     (if (not (member +inf.0 but-not))
         (f +inf.0))
     (if (not (member -inf.0 but-not))
         (f -inf.0))
     (if (not (memq #t (map nan? but-not)))
         (f +nan.0)))

   (define (zero-or-nan? v)
     (or (equal? v 0)
         (nan? v)))

   ;; div and mod aren't R7RS, but these helper syntaxes keep the R6RS names

   ;; Based on tests from Ikarus:
   (define-syntax divmod-test/?
     (syntax-rules ()
      ((_ x1 x2)
       (begin
        (test/values (floor/ x1 x2)
                     (floor-quotient x1 x2)
                     (floor-remainder x1 x2))
        (test/values (truncate/ x1 x2)
                     (truncate-quotient x1 x2)
                     (truncate-remainder x1 x2))))))

   (define-syntax divmod-test
     (syntax-rules ()
      ((_ x1 x2)
       (begin
        (divmod-test/? x1 x2)
        (test (floor-quotient x1 x2)    (floor    (/ x1 x2)))
        (test (truncate-quotient x1 x2) (truncate (/ x1 x2)))
        (test (+ (* x2 (floor-quotient x1 x2))
                 (floor-remainder x1 x2))
              x1)
        (test (+ (* x2 (truncate-quotient x1 x2))
                 (truncate-remainder x1 x2))
              x1)
        (test (truncate-quotient x1 x2)  (quotient x1 x2))
        (test (truncate-remainder x1 x2) (remainder x1 x2))
        (test (modulo x1 x2) (floor-remainder x1 x2))))))

   (define-syntax test-string-to-number
     (syntax-rules ()
       ((_ (str num) ...) (begin (test (string->number str) num) ...))))

   (define-syntax test/approx-string-to-number
     (syntax-rules ()
       ((_ (str num) ...) (begin (test/approx (string->number str) num) ...))))

   (define (run-inexact-tests)

     (test (eqv? 0.0 +nan.0) #f)
     (test/unspec (eqv? +nan.0 +nan.0))

     (test (real? -2.5+0.0i)                       #f)
     (test (real? -2.5+0i)                         #t)
     (test (real? -2.5)                            #t)
     (test (real? #e1e10)                          #t)
     (test (rational? 6/10)                        #t)
     (test (rational? 6/3)                         #t)
     (test (rational? 2)                           #t)
     (test (integer? 3.0)                          #t)

     (test (number? +nan.0)                        #t)
     (test (complex? +nan.0)                       #t)
     (test (real? +nan.0)                          #t)
     (test (rational? +nan.0)                      #f)
     (test (complex? +inf.0)                       #t)
     (test (real? -inf.0)                          #t)
     (test (rational? -inf.0)                      #f)
     (test (integer? -inf.0)                       #f)

     (test (exact? 3.0)                            #f)
     (test (exact? #e3.0)                          #t)
     (test (inexact? 3.)                           #t)

     (test (inexact? +inf.0)                       #t)

     (test (exact-integer? 32.0)                   #f)

     (test (inexact 2) 2.0)
     (test (inexact 2.0) 2.0)
     (test (exact 2) 2)
     (test (exact 2.0) 2)

     (test (= +inf.0 +inf.0)            #t)
     (test (= -inf.0 +inf.0)            #f)
     (test (= -inf.0 -inf.0)            #t)
     (test (= +nan.0 +nan.0)            #f)

     (try-reals
      (lambda (x)
        (test (< -inf.0 x +inf.0)         #t)
        (test (> +inf.0 x -inf.0)         #t))
      '(+inf.0 -inf.0 +nan.0))

     (try-reals
      (lambda (x)
        (test (< +nan.0 x)                #f)
        (test (> +nan.0 x)                #f))
      '())

     (test (zero? +0.0)                   #t)
     (test (zero? -0.0)                   #t)
     (test (zero? 2.0)                    #f)
     (test (zero? -2.0)                   #f)
     (test (zero? +nan.0)                 #f)
     (test (positive? 10)                 #t)
     (test (positive? -10)                #f)
     (test (positive? +inf.0)             #t)
     (test (negative? -inf.0)             #t)
     (test (positive? +nan.0)             #f)
     (test (negative? 10)                 #f)
     (test (negative? -10)                #t)
     (test (negative? +nan.0)             #f)
     (test (finite? +inf.0)               #f)
     (test (finite? 5)                    #t)
     (test (finite? 5.0)                  #t)
     (test (infinite? 5.0)                #f)
     (test (infinite? +inf.0)             #t)
     (test (infinite? +nan.0)             #f)
     (test (nan? +nan.0)                  #t)
     (test (nan? +inf.0)                  #f)
     (test (nan? 1020.0)                  #f)
     (test (nan? 1020/3)                  #f)

     (test (odd? 5.0) #t)
     (test (odd? 50.0) #f)
     (test (even? 5.0) #f)
     (test (even? 50.0) #t)

     (test (max 3.9 4)                             4.0)

     (try-reals
      (lambda (x)
        (test (max +inf.0 x)                          +inf.0)
        (test (min -inf.0 x)                          -inf.0))
      '(+nan.0))

     (test (+ 3.0 4)                               7.0)
     (test (+ +inf.0 +inf.0)                       +inf.0)
     (test (+ +inf.0 -inf.0)                       +nan.0)

     (test (* 4 3.0)                               12.0)
     (test (* 5 +inf.0)                            +inf.0)
     (test (* -5 +inf.0)                           -inf.0)
     (test (* +inf.0 +inf.0)                       +inf.0)
     (test (* +inf.0 -inf.0)                       -inf.0)
     (test (zero-or-nan? (* 0 +inf.0)) #t)
     (test (zero-or-nan? (* 0 +nan.0)) #t)
     (test (zero? (* 1.0 0)) #t)
    
     (try-reals 
      (lambda (x)
        (test (+ +inf.0 x)                            +inf.0)
        (test (+ -inf.0 x)                            -inf.0))
      '(+inf.0 -inf.0 +nan.0))
    
     (try-reals 
      (lambda (x)
        (test (+ +nan.0 x)                            +nan.0))
      '())

     (try-reals 
      (lambda (x)
        (test (* +nan.0 x)                            +nan.0))
      '(0))
    
     (test (+ 0.0 -0.0)  0.0)
     (test (+ -0.0 0.0)  0.0)
     (test (+ 0.0 0.0)   0.0)
     (test (+ -0.0 -0.0) -0.0)
    
     (test (- +inf.0 +inf.0)                       +nan.0)
    
     (test (- 0.0)       -0.0)
     (test (- -0.0)      0.0)
     (test (- 0.0 -0.0)  0.0)
     (test (- -0.0 0.0)  -0.0)
     (test (- 0.0 0.0)   0.0)
     (test (- -0.0 -0.0) 0.0)
    
     (test (/ 3 2.0)                               1.5)
     (test (/ 0.0)                                 +inf.0)
     (test (/ 1.0 0)                               +inf.0)
     (test (/ -1 0.0)                              -inf.0)
     (test (/ +inf.0)                              0.0)

     (test (/ 0 3.5)                               0.0)
     (test (/ 0 0.0)                               +nan.0)
     (test (/ 0.0 0)                               +nan.0)
     (test (/ 0.0 0.0)                             +nan.0)

     (test (abs -inf.0)                            +inf.0)

     (test/values (truncate/ -5.0 -2)              2.0 -1.0)

     (divmod-test +17.0 +3.0)
     (divmod-test +17.0 -3.0)
     (divmod-test -17.0 +3.0)
     (divmod-test -17.0 -3.0)
     (divmod-test +16.0 +3.0)
     (divmod-test +16.0 -3.0)
     (divmod-test -16.0 +3.0)
     (divmod-test -16.0 -3.0)
     (divmod-test +15.0 +3.0)
     (divmod-test +15.0 -3.0)
     (divmod-test -15.0 +3.0)
     (divmod-test -15.0 -3.0)

     (divmod-test +17.0 +3.0)
     (divmod-test +17.0 -3.0)
     (divmod-test -17.0 +3.0)
     (divmod-test -17.0 -3.0)
     (divmod-test +16.0 +3.0)
     (divmod-test +16.0 -3.0)
     (divmod-test -16.0 +3.0)
     (divmod-test -16.0 -3.0)
     (divmod-test +15.0 +3.0)
     (divmod-test +15.0 -3.0)
     (divmod-test -15.0 +3.0)
     (divmod-test -15.0 -3.0)
     (divmod-test +10.0 +4.0)
     (divmod-test +10.0 -4.0)
     (divmod-test -10.0 +4.0)
     (divmod-test -10.0 -4.0)

;    (divmod-test/? +17.0 +nan.0)
;    (divmod-test/? -17.0 +nan.0)
;    (divmod-test/? +17.0 +inf.0)
;    (divmod-test/? +17.0 -inf.0)
;    (divmod-test/? -17.0 +inf.0)
;    (divmod-test/? -17.0 -inf.0)

     (test (denominator (inexact (/ 6 4))) 2.0)

     (test (floor -4.3)                            -5.0)
     (test (ceiling -4.3)                          -4.0)
     (test (truncate -4.3)                         -4.0)
     (test (round -4.3)                            -4.0)

     (test (floor 3.5)                             3.0)
     (test (ceiling 3.5)                           4.0)
     (test (truncate 3.5)                          3.0)
     (test (round 3.5)                             4.0)
    
     (test (floor +inf.0)                          +inf.0)
     (test (ceiling -inf.0)                        -inf.0)
     (test (round +nan.0)                          +nan.0)
    
     (test (rationalize (exact .3) 1/10)          1/3)
     (test/approx (rationalize .3 1/10) #i1/3)
    
     (test (rationalize +inf.0 3)                  +inf.0)
     (test (rationalize +inf.0 +inf.0)             +nan.0)
     (test (rationalize 3 +inf.0)                  0.0)

     (test/approx (exp 1)    2.718281828459045)
     (test (exp +inf.0)                   +inf.0)
     (test (exp -inf.0)                   0.0)
     (test/approx (log 2.718281828459045) 1.0)
     (test (log +inf.0)                   +inf.0)
     (test (log 0.0)                      -inf.0)
     (test/approx (log 100 10) 2.0)
     (test/approx (log 1125899906842624 2) 50.0)

     ;; (log 0) is mathematically undefined and an error in R7RS
     ;;
     ;; (test/exn (log 0) &assertion)

     ;; FIXME: several complex numbers here

     (test/approx (log -inf.0) +inf.0+3.141592653589793i) ; FIXME

     (test/approx (cos 0) 1.0)
     (test/approx (cos 0.0) 1.0)
     (test/approx (cos (/ pi-approx 2.0)) 0.0)
     (test/approx (cos (- (/ pi-approx 2.0))) 0.0)
     (test/approx (cos (- pi-approx)) -1.0)

     (test/approx (sin 0) 0.0)
     (test/approx (sin 0.0) 0.0)
     (test/approx (sin (/ pi-approx 2.0)) 1.0)
     (test/approx (sin (- (/ pi-approx 2.0))) -1.0)
     (test/approx (sin (- pi-approx)) 0.0)

     (test/approx (acos -1) pi-approx)
     (test/approx (acos 1) 0.0)
     (test/approx (acos 1.0) 0.0)
     (test/approx (asin 0) 0.0)
     (test/approx (asin 0.0) 0.0)
     (test/approx (asin 1) (/ pi-approx 2.0))
     (test/approx (asin -1) (- (/ pi-approx 2.0)))
     (test/approx (atan -inf.0) -1.5707963267948965)
     (test/approx (atan +inf.0) 1.5707963267948965)
     (test/approx (log -1.0+0.0i) 0.0+3.141592653589793i) ; FIXME
     (unless (eqv? 0.0 -0.0)
       (test/approx (log -1.0-0.0i) 0.0-3.141592653589793i)) ; FIXME

     (test/approx (sqrt 5) 2.23606797749979)
     (test/approx (sqrt -5) 0.0+2.23606797749979i) ; FIXME

     (test (sqrt +inf.0)                +inf.0)
     (test (sqrt -inf.0)                +inf.0i)

     (test (expt 0 0.0) 1.0)

     (test/approx (expt 0.0 0.0) 1.0)
     (test (inexact? (expt 0.0 0.0)) #t)

     ;; (test/unspec-or-exn (expt 0 -5) &implementation-restriction)
     ;; (test/unspec-or-exn (expt 0 -5+.0000312i) &implementation-restriction)

     (test (expt 0 0)                   1)
     (test (expt 0.0 0.0)               1.0)
    
     (for-each 
      (lambda (n)
        (test (string->number (number->string n)) n)
        (test (string->number (number->string (inexact n) 10)) (inexact n))
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
      '(1 15 1023 -5 2.0 2e200))
     (test (string->number "nope") #f)

     (test (string->number "1e2")                  100.0)
     (test (string->number "0/0")                  #f)
     (test (string->number "+inf.0")               +inf.0)
     (test (string->number "-inf.0")               -inf.0)
     (test (string->number "+nan.0")               +nan.0)

     ;; Originally from Ikarus:
     (test-string-to-number
      ("10" 10)
      ("1" 1)
      ("-17" -17)
      ("#x24" 36)
      ("#x-24" -36)
      ("#b+00000110110" 54)
      ("#e10" 10)
      ("#e1" 1)
      ("#e-17" -17)
      ("#e#x24" 36)
      ("#e#x-24" -36)
      ("#e#b+00000110110" 54)
      ("#x#e24" 36)
      ("#x#e-24" -36)
      ("#b#e+00000110110" 54)
      ("#e1e1000" (expt 10 1000))
      ("#e-1e1000" (- (expt 10 1000)))
      ("#e1e-1000" (expt 10 -1000))
      ("#e-1e-1000" (- (expt 10 -1000))))

     (test/approx-string-to-number
      ("#i1e100" (inexact (expt 10 100)))
      ("#i1e1000" (inexact (expt 10 1000)))
      ("#i-1e1000" (inexact (- (expt 10 1000))))
      ("1e100" (inexact (expt 10 100)))
      ("1.0e100" (inexact (expt 10 100)))
      ("1.e100" (inexact (expt 10 100)))
      ("0.1e100" (inexact (expt 10 99)))
      (".1e100" (inexact (expt 10 99)))
      ("+1e100" (inexact (expt 10 100)))
      ("+1.0e100" (inexact (expt 10 100)))
      ("+1.e100" (inexact (expt 10 100)))
      ("+0.1e100" (inexact (expt 10 99)))
      ("+.1e100" (inexact (expt 10 99)))
      ("-1e100" (inexact (- (expt 10 100))))
      ("-1.0e100" (inexact (- (expt 10 100))))
      ("-1.e100" (inexact (- (expt 10 100))))
      ("-0.1e100" (inexact (- (expt 10 99))))
      ("-.1e100" (inexact (- (expt 10 99)))))

     )

     ;;;
     ))
