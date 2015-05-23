;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This R7RS-portable implementation of div, mod, and friends is
;;; mostly derived from Larceny's src/Lib/Common/number.sch.
;;;

(define (real-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))))

(define (rational-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))
       (finite? (real-part obj))))

(define (integer-valued? obj)
  (and (number? obj)
       (zero? (imag-part obj))
       (finite? (real-part obj))
       (= 1 (denominator (real-part obj)))))

(define (div-and-mod x y)
  (cond ((and (exact-integer? x) (exact-integer? y))
         (cond ((= y 0)
                (assertion-violation 'div "zero divisor" x y))
               ((>= x 0)
                (values (quotient x y) (remainder x y)))
               ((< y 0)
                ; x < 0, y < 0
                (let* ((q (quotient x y))
                       (r (- x (* q y))))
                  (if (= r 0)
                      (values q 0)
                      (values (+ q 1) (- r y)))))
               (else
                ; x < 0, y > 0
                (let* ((q (quotient x y))
                       (r (- x (* q y))))
                  (if (= r 0)
                      (values q 0)
                      (values (- q 1) (+ r y)))))))
        ((or (not (real? x))
             (not (real? y))
             (infinite? x)
             (nan? x)
             (= y 0))
         (assertion-violation 'div "illegal arguments" x y))
        ((< 0 y)
         (let* ((q (floor (/ x y)))
                (r (- x (* q y))))
           (values q r)))
        (else
         (let* ((q (floor (/ x (- y))))
                (r (+ x (* q y))))
           (values (- q) r)))))

(define (div x y)
  (cond ((and (exact-integer? x)
              (exact-integer? y)
              (>= x 0))
         (quotient x y))
        (else
         (call-with-values
          (lambda () (div-and-mod x y))
          (lambda (q r) q)))))

(define (mod x y)
  (cond ((and (exact-integer? x)
              (exact-integer? y)
              (>= x 0))
         (remainder x y))
        (else
         (call-with-values
          (lambda () (div-and-mod x y))
          (lambda (q r) r)))))

(define (div0-and-mod0 x y)
  (call-with-values
   (lambda () (div-and-mod x y))
   (lambda (q r)
     (cond ((< r (abs (/ y 2)))
            (values q r))
           ((> y 0)
            (values (+ q 1) (- x (* (+ q 1) y))))
           (else
            (values (- q 1) (- x (* (- q 1) y))))))))

(define (div0 x y)
  (call-with-values
   (lambda () (div0-and-mod0 x y))
   (lambda (q r) q)))

(define (mod0 x y)
  (call-with-values
   (lambda () (div0-and-mod0 x y))
   (lambda (q r) r)))




   real-valued? rational-valued? integer-valued?
   div mod div-and-mod div0 mod0 div0-and-mod0
   assertion-violation
