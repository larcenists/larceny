;;; Implementations of the R7RS may support (scheme complex) without
;;; supporting (scheme inexact), so inexact number notation is isolated
;;; here.

(define (try-reals f but-not)
  (if (not (member 0 but-not))
      (f 0))
  (f -1.0)
  (f 0.0)
  (f 1.0)
  (f 1/2)
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

(define (try-complexes f but-not)
  (try-reals f but-not)
  (f 1+2i))

(define (run-complex-tests-inexact)

  (test (finite? 3.0+inf.0i) #f)
  (test (infinite? 3.0+inf.0i) #t)

  ;; R6RS example is wrong, because roundoff error in second argument
  ;; can affect the result (changing result to 1.0 instead of 0.0).
  ;; According to general principles stated in both the R7RS and R6RS,
  ;; therefore, the result of this computation should be inexact.

  (test/approx (expt 0 5+.0000312i) 0.0)

  (test/approx (expt 0.0 5+.0000312i) 0.0)
  (test (inexact? (expt 0.0 5+.0000312i)) #t)

  (test/approx (make-rectangular 1.1 0.0) 1.1+0.0i)
  (test/approx (make-rectangular 1.1 2.2) 1.1+2.2i)
  (test/approx (make-polar 1.1 0.0) 1.1+0.0i)
  (test/approx (make-polar 1.1 2.2) 1.1@2.2)

  (test/approx (real-part 1.1+2.2i)              1.1)
  (test/approx (imag-part 1.1+2.2i)              2.2)
  (test/approx (magnitude 1.1@2.2)              1.1)

  ;; These are commented out because the R7RS doesn't require
  ;; the imaginary part of a real number to be exact.

#|

  (test (exact? (imag-part 0.0)) #t)
  (test (exact? (imag-part 1.0)) #t)
  (test (exact? (imag-part 1.1)) #t)
  (test (exact? (imag-part +nan.0)) #t)
  (test (exact? (imag-part +inf.0)) #t)
  (test (exact? (imag-part -inf.0)) #t)

|#

  (test (zero? (imag-part 0.0)) #t)
  (test (zero? (imag-part 1.0)) #t)
  (test (zero? (imag-part 1.1)) #t)
  (test (zero? (imag-part +nan.0)) #t)
  (test (zero? (imag-part +inf.0)) #t)
  (test (zero? (imag-part -inf.0)) #t) 

  (test/approx (angle 1.1@2.2)                  2.2)

  (test/approx (angle -1.0)         3.141592653589793)
  (test/approx (angle -1.0+0.0i)    3.141592653589793)
  (unless (eqv? 0.0 -0.0)
    (test/approx (angle -1.0-0.0i)    -3.141592653589793))
  (test (angle +inf.0)       0.0)
  (test/approx (angle -inf.0)       3.141592653589793)

  (test (magnitude (make-rectangular +inf.0 1)) +inf.0)
  (test (magnitude (make-rectangular -inf.0 1)) +inf.0)
  (test (magnitude (make-rectangular 1 +inf.0)) +inf.0)
  (test (magnitude (make-rectangular 1 -inf.0)) +inf.0)

  (test/approx (angle -1)   3.141592653589793)

  (if (not (eqv? -0.0 0.0))
      (test/approx (log -0.0) (make-rectangular -inf.0 pi-approx)))

  (try-complexes
   (lambda (z)
     (test (= +nan.0 z)                #f))
   '())

  )
