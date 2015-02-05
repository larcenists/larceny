;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme complex) procedures:
;;;
;;;     make-polar
;;;     make-rectangular
;;;     angle
;;;     magnitude
;;;     real-part
;;;     imag-part


(define-library (tests scheme complex)
  (export run-complex-tests)
  (import (scheme base)
          (scheme complex)
          (tests scheme test))

  (cond-expand
   ((library (scheme inexact))
    (import (scheme inexact))
    (include "complex.body.scm"))
   ((not (library (scheme inexact)))
    (begin (define (run-complex-tests-inexact) #t))))

  (begin

   (define pi-approx (/ 333 106))

   (define (run-complex-tests)

     (test/approx (make-rectangular 1 0) 1+0i)
     (test/approx (make-rectangular 1 2) 1+2i)
     (test/approx (make-polar 1 0) 1+0i)
     (test/approx (make-polar 1 2) 1@2)

     (test/approx (real-part 1+2i)              1)
     (test/approx (imag-part 1+2i)              2)
     (test/approx (magnitude 3@2)               3)
     (test/approx (angle 3@2)                   2)

     (test (exact? (imag-part 0)) #t)
     (test (exact? (imag-part 1)) #t)
     (test (exact? (imag-part 1)) #t)

     (test (zero? (imag-part 0)) #t)
     (test (zero? (imag-part 1)) #t)
     (test (zero? (imag-part 1)) #t)

     (test/approx (angle -1)       pi-approx)
     (test/approx (angle -1+0i)    pi-approx)

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
      '(1+2i))

     (run-complex-tests-inexact)

     ;;;
     )))
