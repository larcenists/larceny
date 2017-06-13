;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; References
;;;
;;; Milton Abramowitz and Irene A Stegun [editors].
;;; Handbook of Mathematical Functions With Formulas, Graphs, and
;;; Mathematical Tables.  United States Department of Commerce.
;;; National Bureau of Standards Applied Mathematics Series, 55,
;;; June 1964.  Fifth Printing, August 1966, with corrections.
;;;
;;; R W Hamming.  Numerical Methods for Scientists and Engineers.
;;; McGraw-Hill, 1962.
;;;
;;; Donald E Knuth.  The Art of Computer Programming, Volume 2,
;;; Seminumerical Algorithms, Second Edition.  Addison-Wesley, 1981.

;;; I have deliberately avoided recent references, and have also
;;; avoided looking at any code or pseudocode for these or similar
;;; functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Abramowitz and Stegun 6.1.5 ::  z! = Gamma(z+1)
;;; Abramowitz and Stegun 6.1.15 :  Gamma(z+1) = z Gamma(z)
;;;
;;; Gamma(x+2) = (x+1) Gamma(x+1) = (x+1) x Gamma(x)
;;; Gamma(x) = Gamma(x+2) / (x (x + 1))
;;;
;;; Those equations reduce the computation of Gamma(x) to the range
;;;     1.0 <= x <= 2.0

(define (flgamma x)
  (check-flonum! 'flgamma x)
  (cond ((fl>=? x flgamma:upper-cutoff)
         +inf.0)
        ((fl<=? x flgamma:lower-cutoff)
         (cond ((flinteger? x) +nan.0)    ; pole error
               ((flodd? (fltruncate x)) 0.0)
               (else -0.0)))
        (else (Gamma x))))

(define (Gamma x)
  (cond ((fl>? x 2.0)
         (let ((x (fl- x 2.0)))
           (fl* x (fl+ x 1.0) (Gamma x))))
        ((fl>? x 1.0)
         (let ((x (fl- x 1.0)))
           (fl* x (Gamma x))))
        ((fl=? x 1.0)
         1.0)
        ((and (fl<=? x 0.0)
              (flinteger? x))    ; pole error
         +nan.0)
        ((fl<? x 0.0)
         (fl/ (Gamma (fl+ x 2.0)) x (fl+ x 1.0)))
        (else
         (fl/ (polynomial-at x gamma-coefs)))))

;;; Series expansion for 1/Gamma(x), from Abramowitz and Stegun 6.1.34

(define gamma-coefs
  '(0.0
    1.0
    +0.5772156649015329
    -0.6558780715202538
    -0.0420026350340952
    +0.1665386113822915 ; x^5
    -0.0421977345555443
    -0.0096219715278770
    +0.0072189432466630
    -0.0011651675918591
    -0.0002152416741149 ; x^10
    +0.0001280502823882
    -0.0000201348547807
    -0.0000012504934821
    +0.0000011330272320
    -0.0000002056338417 ; x^15
    +0.0000000061160950
    +0.0000000050020075
    -0.0000000011812746
    +0.0000000001043427
    +0.0000000000077823 ; x^20
    -0.0000000000036968
    +0.0000000000005100
    -0.0000000000000206
    -0.0000000000000054
    +0.0000000000000014 ; x^25
    +0.0000000000000001
    ))

;;; If x >= flgamma:upper-cutoff, then (Gamma x) is +inf.0

(define flgamma:upper-cutoff
  (do ((x 2.0 (+ x 1.0)))
      ((flinfinite? (Gamma x))
       x)))

;;; If x <= flgamma:lower-cutoff, then (Gamma x) is a zero or NaN

(define flgamma:lower-cutoff
  (do ((x -2.0 (- x 1.0)))
      ((flzero?
        (Gamma (fladjacent x 0.0)))
       x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; log (Gamma (x))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: the current draft spec for flloggamma is surely wrong.
;;; Presumably the intended spec is as stated below.

;;; Returns two values:
;;;     log ( |Gamma(x)| )
;;;     sgn (Gamma(x))
;;;
;;; The draft spec is unclear concerning sgn (Gamma(x)),
;;; but (flsgn x) returns (flcopysign 1.0 x) so I'm assuming
;;; sgn (Gamma(x)) is +1.0 for positive and -1.0 for negative.
;;; For real x, Gamma(x) is never actually zero, but it is
;;; undefined if x is zero or a negative integer.

;;; For small absolute values, this is trivial.
;;; Abramowitz and Stegun give several asymptotic formulas
;;; that might be good enough for large values of x.
;;;
;;; 6.1.41 :  As x goes to positive infinity, log (Gamma (x)) goes to
;;;
;;;     (x - 1/2) log x - x + 1/2 log (2 pi)
;;;         + 1/(12x) - 1/(360x^3) + 1/(1260x^5) - 1/(1680x^7) + ...
;;;
;;; 6.1.48 states a continued fraction.

;;; FIXME: I don't know how to compute log (|Gamma(x)|) accurately
;;; for negative x of large magnitude.

(define (flloggamma x)
  (check-flonum! 'flloggamma x)
  (cond ((flinfinite? x)
         (if (flpositive? x)
             (values x 1.0)
             (values +nan.0 +nan.0)))
        ((fl>=? x 20.0)
         (values (eqn6.1.48 x) 1.0))
        ((fl>? x 0.0)
         (let ((g (flgamma x)))
           (values (log g) 1.0)))
        (else
         (let ((g (flgamma x)))
           (values (log (flabs g))
                   (flcopysign 1.0 g))))))

;;; This doesn't seem to be as accurate as the continued fraction
;;; of equation 6.1.48, so it's commented out for now.

#;
(define (eqn6.1.41 x)
  (let* ((x^2 (fl* x x))
         (x^3 (fl* x x^2))
         (x^5 (fl* x^2 x^3))
         (x^7 (fl* x^2 x^5)))
    (fl+ (fl* (fl- x 0.5) (fllog x))
         (fl- x)
         (fl* 0.5 (fllog fl-2pi))
         (fl/ (fl* 12.0 x))
         (fl/ (fl* 360.0 x^3))
         (fl/ (fl* 1260.0 x^5))
         (fl/ (fl* 1680.0 x^7)))))

(define (eqn6.1.48 x)
  (let ((+ fl+)
        (/ fl/))
    (+ (fl* (fl- x 0.5) (fllog x))
       (fl- x)
       (fl* 0.5 (fllog fl-2pi))
       (/ #i1/12
          (+ x
             (/ #i1/30
                (+ x
                   (/ #i53/210
                      (+ x
                         (/ #i195/371
                            (+ x
                               (/ #i22999/22737
                                  (+ x
                                     (/ #i29944523/19733142
                                        (+ x
                                           (/ #i109535241009/48264275462
                                              (+ x)))))))))))))))))

;;; With IEEE double precision, eqn6.1.48 is at least as accurate as
;;; (log (flgamma x)) starting around x = 20.0

(define flloggamma:upper-threshold 20.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bessel functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: Equation 9.1.10 is exact, but it's an infinite series.
;;; It doesn't converge fast enough for x greater than 1.5 or so,
;;; and underflows (using inexact arithmetic) for large n.
;;;
;;; FIXME: need something better for large x and large n

(define (flfirst-bessel x n)
  (check-flonum! 'flfirst-bessel x)
  (cond (c-functions-are-available
         (jn n x))
        ((< n 0)
         (let ((result (flfirst-bessel x (- n))))
           (if (even? n) result (- result))))

        ((and (< 9 n 101)                ; FIXME
              (fl<? 1.0 x 125.0))         ; FIXME
;        (write (list "9.1.75" x n)) (newline)
         (eqn9.1.75 x n))

        ((and (< 2 n)
              (fl>? x 50.0))
         (eqn9.1.27 x n))

        ((and (< n 3)
              (fl>? x 50.0))
         (eqn9.2.1 x n))
#;
        ((and (< 2 n)                    ; FIXME
              (fl>? x 50.0))             ; FIXME
;        (write (list "9.2.1" x n)) (newline)
         (eqn9.2.1 x n))
        (else
         (eqn9.1.10 x n))))

(define (iota n)
  (do ((n (- n 1) (- n 1))
       (x '() (cons n x)))
      ((< n 0) x)))

;;; For n = 0 and kmax = 20, this agrees with C99 jn for 0 <= x <= 1.5.
;;; It should become more accurate for larger n but less accurate for
;;; larger x.  Should be okay if n > x.

(define (eqn9.1.10 x n)
  (fl* (inexact (expt (* 0.5 x) n))
       (polynomial-at (flsquare x)
                      (cond ((= n 0)
                             eqn9.1.10-coefficients-0)
                            ((= n 1)
                             eqn9.1.10-coefficients-1)
                            (else
                             (eqn9.1.10-coefficients n))))))

(define (eqn9.1.10-coefficients n)
  (define (loop k prev)
    (if (flzero? (inexact prev))
        '()
        (let ((c (/ (* -1/4 prev) k (+ n k))))
          (cons c (loop (+ k 1) c)))))
  (let ((c (/ (fact n))))
    (map inexact (cons c (loop 1 c)))))

(define eqn9.1.10-coefficients-0
  (eqn9.1.10-coefficients 0))

(define eqn9.1.10-coefficients-1
  (eqn9.1.10-coefficients 1))

;;; Returns an approximation to J_{m+n}(x).
;;;
;;; FIXME: this doesn't seem to work at all, so I may have introduced a bug.

(define (eqn9.1.14 x m n kmax)
  (fl* (inexact (expt (* 0.5 x) (+ m n)))
       (polynomial-at (flsquare x)
                      (map (lambda (k)
                             (fl/ (fl* (inexact (expt -0.25 k))
                                       (flgamma (inexact (+ m n k k 1))))
                                  (fl* (factorial (inexact k))
                                       (flgamma (inexact (+ m k 1)))
                                       (flgamma (inexact (+ n k 1)))
                                       (flgamma (inexact (+ m n k 1))))))
                           (iota (+ kmax 1))))))

;;; Equation 9.1.27 says J_{n-1}(x) + J_{n+1}(x) = (2n/x) J_n(x)
;;;
;;; J_{n+1}(x) = (2n/x) J_n(x) - J_{n-1}(x)

(define (eqn9.1.27 x n0)
  (define (loop n jn jn-1)
    (cond ((= n n0)
           jn)
          (else
           (loop (+ n 1)
                 (fl- (fl* (fl/ (inexact (+ n n)) x) jn)
                      jn-1)
                 jn))))
  (if (< n0 1) ; FIXME
      (flfirst-bessel x n0)
      (loop 2 (flfirst-bessel x 2) (flfirst-bessel x 1))))

;;; Equation 9.1.75 states an equality between J_n(x)/J_{n-1}(x)
;;; and a continued fraction.
;;;
;;; Precondition: |x| > 0
;;;
;;; This works very well provided (flfirst-bessel x 0) is accurate
;;; and x is small enough for it to run in reasonable time.

(define (eqn9.1.75 x n)
  (define k (max 10 (* 2 (exact (flceiling x)))))
  (define (loop x2 m i)
    (if (> i k)
        (fl/ 1.0 (fl* m x2))
        (fl/ 1.0
             (fl- (fl* m x2)
                  (loop x2 (+ m 1.0) (+ i 1))))))
; (if (and (> n 0) (flpositive? x))
  (if (and (> n 3) (flpositive? x))
      (fl* (eqn9.1.75 x (- n 1))
           (loop (fl/ 2.0 x) (inexact n) 0))
      (flfirst-bessel x n)))

;;; Equation 9.2.1 states an asymptotic approximation that agrees
;;; with C99 jn to 6 decimal places for n = 0 and x = 1e6.

(define (eqn9.2.1 x n)
  (fl* (flsqrt (/ 2.0 (fl* fl-pi x)))
       (flcos (fl- x (fl* 0.5 (inexact n) fl-pi) (fl* 0.25 fl-pi)))))




(define flsecond-bessel FIXME)
(define flerf FIXME)
(define flerfc FIXME)

