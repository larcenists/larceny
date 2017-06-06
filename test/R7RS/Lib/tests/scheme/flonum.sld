;;; Copyright (C) William D Clinger (2017).
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (srfi 144) constants and procedures:
;;;
;;;     fl-e
;;;     fl-1/e
;;;     fl-e-2
;;;     fl-e-pi/4
;;;     fl-log2-e
;;;     fl-log10-e
;;;     fl-log-2
;;;     fl-1/log-2
;;;     fl-log-3
;;;     fl-log-pi
;;;     fl-log-10
;;;     fl-1/log-10
;;;     fl-pi
;;;     fl-1/pi
;;;     fl-2pi
;;;     fl-pi/2
;;;     fl-pi/4
;;;     fl-2/sqrt-pi
;;;     fl-pi-squared
;;;     fl-degree
;;;     fl-2/pi
;;;     fl-sqrt-2
;;;     fl-sqrt-3
;;;     fl-sqrt-5
;;;     fl-sqrt-10
;;;     fl-1/sqrt-2
;;;     fl-cbrt-2
;;;     fl-cbrt-3
;;;     fl-4thrt-2
;;;     fl-phi
;;;     fl-log-phi
;;;     fl-1/log-phi
;;;     fl-euler
;;;     fl-e-euler
;;;     fl-sin-1
;;;     fl-cos-1
;;;     fl-gamma-1/2
;;;     fl-gamma-1/3
;;;     fl-gamma-2/3
;;;
;;;     fl-greatest
;;;     fl-least
;;;     fl-epsilon
;;;     fl-fast-fl+*
;;;     fl-integer-exponent-zero
;;;     fl-integer-exponent-nan
;;;
;;;     flonum
;;;     fladjacent
;;;     flcopysign
;;;     make-flonum
;;;
;;;     flinteger-fraction
;;;     flexponent
;;;     flinteger-exponent
;;;     flnormalized-fraction-exponent
;;;     flsign-bit
;;;
;;;     flonum?
;;;     fl=?
;;;     fl<?
;;;     fl>?
;;;     fl<=?
;;;     fl>=?
;;;     flunordered?
;;;     flmax
;;;     flmin
;;;     flinteger?
;;;     flzero?
;;;     flpositive?
;;;     flnegative?
;;;     flodd?
;;;     fleven?
;;;     flfinite?
;;;     flinfinite?
;;;     flnan?
;;;     flnormalized?
;;;     fldenormalized?
;;;
;;;     fl+
;;;     fl*
;;;     fl+*
;;;     fl-
;;;     fl/
;;;     flabs
;;;     flabsdiff
;;;     flsgn
;;;     flnumerator
;;;     fldenominator
;;;     flfloor
;;;     flceiling
;;;     flround
;;;     fltruncate
;;;
;;;     flexp
;;;     flexp2
;;;     flexp-1
;;;     flsquare
;;;     flsqrt
;;;     flcbrt
;;;     flhypot
;;;     flexpt
;;;     fllog
;;;     fllog1+
;;;     fllog2
;;;     fllog10
;;;     make-fllog-base
;;;
;;;     flsin
;;;     flcos
;;;     fltan
;;;     flasin
;;;     flacos
;;;     flatan
;;;     flsinh
;;;     flcosh
;;;     fltanh
;;;     flasinh
;;;     flacosh
;;;     flatanh
;;;
;;;     flquotient
;;;     flremainder
;;;     flremquo
;;;
;;;     flgamma
;;;     flloggamma
;;;     flfirst-bessel
;;;     flsecond-bessel
;;;     flerf
;;;     flerfc


(define-library (tests scheme flonum)
  (export run-flonum-tests)
  (import (scheme base)
          (srfi 144)
          (tests scheme test)
          (primitives bytevector-like-ref) ; FIXME
          (rnrs arithmetic bitwise) ; FIXME
          (scheme write) ; FIXME
          (scheme inexact))

  (begin
;;; FIXME: Larceny-specific code for visualization of flonums.
;;; Assumes IEEE double precision, Larceny's usual representation,
;;; and little-endian.

(define (show x)
  (map (lambda (i) (bytevector-like-ref x i))
       '(4 5 6 7 8 9 10 11)))

(define (show-sign x)
  (bitwise-arithmetic-shift (list-ref (show x) 7) -7))

(define (show-exponent x)
  (bitwise-ior
   (bitwise-arithmetic-shift (bitwise-and (list-ref (show x) 7) 127)
                             3)
   (bitwise-arithmetic-shift (bitwise-and (list-ref (show x) 6) #b11100000)
                             -5)))

(define (show-significand x)
  (let ((bytes (show x)))
    (+ (* (list-ref bytes 0) 1)
       (* (list-ref bytes 1) 256)
       (* (list-ref bytes 2) 256 256)
       (* (list-ref bytes 3) 256 256 256)
       (* (list-ref bytes 4) 256 256 256 256)
       (* (list-ref bytes 5) 256 256 256 256 256)
       (* (bitwise-and (list-ref bytes 6) #b00011111)
          256 256 256 256 256 256))))
)

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define-syntax test/=
     (syntax-rules ()
      ((test/= expr1 expr2)
       (test expr2 expr1))))

   (define-syntax test/FIXME
     (syntax-rules ()
      ((test/FIXME expr1 expr2)
       (begin (test/= expr1 expr2)
              (write 'expr2) (newline)
              (write (show-significand expr2)) (newline)
              (write (show-significand expr1)) (newline) (newline)))))

   ;; convenient values for test cases

   (define posints (map flonum '(1 2 3 4 5 10 65536 1e23)))
   (define nats (cons (flonum 0) posints))
   (define ints (append (map flonum '(-20 -8 -2 -1)) nats))
   (define posfracs (map flonum '(1/1000 1/10 1/3 1/2)))
   (define extremes
     (list (fl- fl-greatest) (fl- fl-least) fl-least fl-greatest))
   (define infinities (map flonum (list -inf.0 +inf.0)))
   (define weird (append infinities (list (flonum +nan.0))))

   (define somereals (append (map flonum
                                  (list (fl- fl-greatest)
                                        -10
                                        (fl- fl-least)
                                        0))
                             posfracs
                             posints))
   (define somereals+weird
     (append somereals weird))

   (define negzero (flonum -0.0))
   (define zero (flonum 0))
   (define one (flonum 1))
   (define two (flonum 2))

   (define neginf (flonum -inf.0))
   (define posinf (flonum +inf.0))
   (define nan (flonum +nan.0))


   (define (run-flonum-tests)

     ;; Mathematical constants

     (test/= 2.718281828459045235360287                    fl-e)
     (test/= .3678794411714423215955238                    fl-1/e)
     (test/= 7.389056098930650227230427                    fl-e-2)
     (test/= 2.1932800507380154566                         fl-e-pi/4)
     (test/= 1.4426950408889634073599246810018921374266    fl-log2-e)
     (test/= .4342944819032518276511289                    fl-log10-e)
     (test/= .6931471805599453094172321                    fl-log-2)
     (test/= 1.4426950408889634073599246810018921374266    fl-1/log-2)
     (test/= 1.0986122886681096913952452                   fl-log-3)
     (test/= 1.144729885849400174143427                    fl-log-pi)
     (test/= 2.3025850929940456840179915                   fl-log-10)
     (test/= 0.4342944819032518276511289189166050822944    fl-1/log-10)

     (test/= 3.1415926535897932384626433832795028841972    fl-pi)
     (test/= 0.3183098861837906715377675267450287240689    fl-1/pi)
     (test/= 6.283185307179586476925287                    fl-2pi)
     (test/= 1.570796326794896619231322                    fl-pi/2)
     (test/= .7853981633974483096156608                    fl-pi/4)
     (test/= .5641895835477562869480795                    (/ fl-2/sqrt-pi 2))
     (test/= 9.869604401089358618834491                    fl-pi-squared)
     (test/= 0.0174532925199432957692369076848861271344    fl-degree)
     (test/= .3183098861837906715377675                    (/ fl-2/pi 2))

     (test/= 1.4142135623730950488016887242096980785697    fl-sqrt-2)
     (test/= 1.7320508075688772935274463415058723669428    fl-sqrt-3)
     (test/= 2.2360679774997896964091736687311762354406    fl-sqrt-5)
     (test/= 3.1622776601683793319988935444327185337196    fl-sqrt-10)
     (test/= 1.4142135623730950488016887242096980785697    (* 2 fl-1/sqrt-2))
     (test/= 1.2599210498948731647672106072782283505703    fl-cbrt-2)
     (test/= 1.4422495703074083823216383107801095883919    fl-cbrt-3)
     (test/= 1.1892071150027210667174999705604759152930    fl-4thrt-2)

     (test/= 1.6180339887498948482045868343656381177203    fl-phi)
     (test/= 0.4812118250596034474977589134243684231352    fl-log-phi)
     (test/= 2.0780869212350275376013226061177957677422    fl-1/log-phi)
     (test/= 0.5772156649015328606065120900824024310422    fl-euler)
     (test/= 1.7810724179901979852365041031071795491696    fl-e-euler)

     (test/= 0.8414709848078965066525023216302989996226    fl-sin-1)
     (test/= 0.5403023058681397174009366074420766037323    fl-cos-1)

     (test/= 1.7742438509055160272981674833411451827975    fl-gamma-1/2)
     (test/= 2.6789385347077476336556929409746776441287    fl-gamma-1/3)
     (test/= 1.3541179394264004169452880281545137855193    fl-gamma-2/3)

     ;; Implementation Constants

     (test-assert (inexact? fl-greatest))
     (test-assert (inexact? fl-least))
     (test-assert (inexact? fl-epsilon))

     (test-assert (real? fl-greatest))
     (test-assert (real? fl-least))
     (test-assert (real? fl-epsilon))

     (test-assert (flonum? fl-greatest))
     (test-assert (flonum? fl-least))
     (test-assert (flonum? fl-epsilon))

     (test-assert (< 0.0
                     fl-least
                     fl-epsilon
                     1.0
                     (+ 1.0 fl-epsilon)
                     fl-greatest
                     posinf))
     (test-assert (= (* 2 fl-greatest) posinf))
     (test-assert (= 1 (/ (+ 1 (+ 1.0 fl-epsilon)) 2)))
     (test-assert (= 0 (/ fl-least 2)))

     (test-assert (boolean? fl-fast-fl+*))
     (test-assert (exact-integer? fl-integer-exponent-zero))
     (test-assert (exact-integer? fl-integer-exponent-nan))

     ;; Constructors

     (test (flonum 3) (flonum 3.0))
     (test (map flonum somereals) somereals)
     (test (map flonum weird) weird)

     (test (map fladjacent somereals somereals) somereals)
     (test (map fladjacent weird weird) weird)

     (test (fladjacent zero posinf) fl-least)
     (test (fladjacent zero neginf) (fl- fl-least))
     (test (fladjacent fl-least posinf) (fl+ fl-least fl-least))
     (test (fladjacent fl-least neginf) zero)
     (test (fladjacent (fl- fl-least) posinf) zero)
     (test (fladjacent (fl- fl-least) neginf) (fl* -2.0 fl-least))

     (test (fladjacent zero one) fl-least)
     (test (fladjacent zero (fl- one)) (fl- fl-least))
     (test (fladjacent fl-least one) (fl+ fl-least fl-least))
     (test (fladjacent fl-least (fl- one)) zero)
     (test (fladjacent (fl- fl-least) one) zero)
     (test (fladjacent (fl- fl-least) (fl- one)) (fl* -2.0 fl-least))

     (test (fl- (fladjacent one fl-greatest) one) fl-epsilon)
     (test (fl- one (fladjacent one zero)) (fl/ fl-epsilon 2.0))

     (test (fladjacent posinf zero) fl-greatest)
     (test (fladjacent neginf zero) (fl- fl-greatest))

     (test (flcopysign zero posinf) zero)
     (test (flcopysign zero neginf) negzero)
     (test (flcopysign zero one) zero)
     (test (flcopysign zero (fl- one)) negzero)
     (test (flcopysign one fl-least) one)
     (test (flcopysign one (fl- fl-greatest)) (fl- one))
     (test (flcopysign (fl- one) zero) one)
     (test (map flcopysign somereals somereals) somereals)
     (test (map flcopysign somereals (map fl- somereals))
           (map fl- somereals))
     (test (map flcopysign infinities infinities) infinities)
     (test (map flcopysign infinities (reverse infinities))
           (reverse infinities))

     (test (make-flonum zero 12) zero)
     (test (make-flonum zero -24) zero)
     (test (make-flonum zero 0) zero)
     (test (map make-flonum somereals (map (lambda (x) 0) somereals))
           somereals)
     (test (map make-flonum somereals (map (lambda (x) 2) somereals))
           (map (lambda (x) (fl* (flonum 4) x)) somereals))
     (test (map make-flonum somereals (map (lambda (x) -4) somereals))
           (map (lambda (x) (fl/ x (flonum 16))) somereals))
     (test (make-flonum fl-greatest 1) posinf)
     (test (make-flonum (fl- fl-greatest) 1) neginf)
     (test (make-flonum fl-greatest -1) (fl/ fl-greatest two))
     (test (make-flonum (fl- fl-greatest) -1) (fl- (fl/ fl-greatest two)))
     (test (make-flonum fl-least 1) (fl* two fl-least))
     (test (make-flonum (fl- fl-least) 1) (fl- (fl* two fl-least)))
     (test (make-flonum fl-least -1) zero)
     (test (make-flonum (fl- fl-least) -1) negzero)

     ;; Accessors

     (call-with-values
      (lambda () (flinteger-fraction 3.75))
      (lambda (q r)
        (test q (flonum 3))
        (test r (flonum .75))))

     (call-with-values
      (lambda () (flinteger-fraction -3.75))
      (lambda (q r)
        (test q (flonum -3))
        (test r (flonum -.75))))

     (test/= (flonum 12.0)
             (flexponent (flexpt two (flonum 12))))
     (test/approx (flexponent (flexpt two (flonum 12.5)))
                  (flonum 12.5))
     (test/= (flonum -5.0)
             (flexponent (flexpt two (flonum -5))))
     (test/approx (flexponent (flexpt two (flonum -4.5)))
                  (flonum -4.5))

     ;; FIXME
     ;;     flinteger-exponent

     (let* ((correct?
             (lambda (x y n)
               (or (fl=? x (* y (expt two n)))
                   (fl=? x (* 4.00 y (expt two (- n 2))))
                   (fl=? x (* 0.25 y (expt two (+ n 2)))))))
            (test-flnormalized-fraction-exponent
             (lambda (x)
               (call-with-values
                (lambda () (flnormalized-fraction-exponent x))
                (lambda (y n)
                  (list (flonum? y)
                        (exact-integer? n)
                        (fl<=? (flonum 0.5) (flabs y))
                        (fl<? (flabs y) one)
                        (correct? x y n)))))))
       (test (test-flnormalized-fraction-exponent zero)
             '(#t #t #f #t #t))
       (test (test-flnormalized-fraction-exponent negzero)
             '(#t #t #f #t #t))
       (test (test-flnormalized-fraction-exponent one)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent two)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent fl-least)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent fl-greatest)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent (fl- fl-least))
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent (fl- fl-greatest))
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent posinf)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent neginf)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent nan)
             '(#t #t #f #f #f))

       )

     (test (flsign-bit one) 0)
     (test (flsign-bit zero) 0)
     (test (flsign-bit negzero) 1)
     (test (flsign-bit (flonum -2)) 1)
     (test (flsign-bit posinf) 0)
     (test (flsign-bit neginf) 1)

     ;; Predicates

     (let ((alltrue  (map (lambda (x) #t) somereals))
           (allfalse (map (lambda (x) #f) somereals)))

       (test (map flonum? somereals) alltrue)
       (test (map flonum? weird) '(#t #t #t))

       (test-deny   (fl=? zero fl-least))
       (test-assert (fl=? fl-least fl-least))
       (test-deny   (fl=? one fl-least))
       (test-assert (fl=? neginf neginf))
       (test-deny   (fl=? neginf posinf))
       (test-deny   (fl=? posinf neginf))
       (test-assert (fl=? posinf posinf))
       (test-deny   (fl=? zero nan))
       (test-deny   (fl=? nan one))
       (test (map fl=? somereals somereals) alltrue)
       (test (map fl=? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl=? (cdr somereals) somereals) (cdr allfalse))

       (test-assert (fl<? zero fl-least))
       (test-deny   (fl<? fl-least fl-least))
       (test-deny   (fl<? one fl-least))
       (test-deny   (fl<? neginf neginf))
       (test-assert (fl<? neginf posinf))
       (test-deny   (fl<? posinf neginf))
       (test-deny   (fl<? posinf posinf))
       (test-deny   (fl<? zero nan))
       (test-deny   (fl<? nan one))
       (test (map fl<? somereals somereals) allfalse)
       (test (map fl<? somereals (cdr somereals)) (cdr alltrue))
       (test (map fl<? (cdr somereals) somereals) (cdr allfalse))

       (test-deny   (fl>? zero fl-least))
       (test-deny   (fl>? fl-least fl-least))
       (test-assert (fl>? one fl-least))
       (test-deny   (fl>? neginf neginf))
       (test-deny   (fl>? neginf posinf))
       (test-assert (fl>? posinf neginf))
       (test-deny   (fl>? posinf posinf))
       (test-deny   (fl>? zero nan))
       (test-deny   (fl>? nan one))
       (test (map fl>? somereals somereals) allfalse)
       (test (map fl>? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl>? (cdr somereals) somereals) (cdr alltrue))

       (test-assert (fl<=? zero fl-least))
       (test-assert (fl<=? fl-least fl-least))
       (test-deny   (fl<=? one fl-least))
       (test-assert (fl<=? neginf neginf))
       (test-assert (fl<=? neginf posinf))
       (test-deny   (fl<=? posinf neginf))
       (test-assert (fl<=? posinf posinf))
       (test-deny   (fl<=? zero nan))
       (test-deny   (fl<=? nan one))
       (test (map fl<=? somereals somereals) alltrue)
       (test (map fl<=? somereals (cdr somereals)) (cdr alltrue))
       (test (map fl<=? (cdr somereals) somereals) (cdr allfalse))

       (test-deny   (fl>=? zero fl-least))
       (test-assert (fl>=? fl-least fl-least))
       (test-assert (fl>=? one fl-least))
       (test-assert (fl>=? neginf neginf))
       (test-deny   (fl>=? neginf posinf))
       (test-assert (fl>=? posinf neginf))
       (test-assert (fl>=? posinf posinf))
       (test-deny   (fl>=? zero nan))
       (test-deny   (fl>=? nan one))
       (test (map fl>=? somereals somereals) alltrue)
       (test (map fl>=? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl>=? (cdr somereals) somereals) (cdr alltrue))

       (test-deny   (flunordered? zero fl-least))
       (test-deny   (flunordered? fl-least fl-least))
       (test-deny   (flunordered? one fl-least))
       (test-deny   (flunordered? neginf neginf))
       (test-deny   (flunordered? neginf posinf))
       (test-deny   (flunordered? posinf neginf))
       (test-deny   (flunordered? posinf posinf))
       (test-assert (flunordered? zero nan))
       (test-assert (flunordered? nan one))
       (test (map flunordered? somereals somereals) allfalse)
       (test (map flunordered? somereals (cdr somereals)) (cdr allfalse))
       (test (map flunordered? (cdr somereals) somereals) (cdr allfalse))

       )

     (test (flmax) neginf)
     (test (flmax zero) zero)
     (test (flmax zero one) one)
     (test (flmax one zero) one)
     (test (apply flmax somereals) (car (reverse somereals)))

     (test (flmin) posinf)
     (test (flmin one) one)
     (test (flmin zero one) zero)
     (test (flmin one zero) zero)
     (test (apply flmin somereals) (car somereals))

     (test (map flinteger? somereals)
           (map fl=?
                somereals
                (map flround somereals)))

     (test-deny   (flzero? neginf))
     (test-deny   (flzero? (fl- fl-least)))
     (test-assert (flzero? negzero))
     (test-assert (flzero? zero))
     (test-deny   (flzero? fl-least))
     (test-deny   (flzero? posinf))

     (test-deny   (flpositive? neginf))
     (test-deny   (flpositive? (fl- fl-least)))
     (test-deny   (flpositive? negzero))
     (test-deny   (flpositive? zero))
     (test-assert (flpositive? fl-least))
     (test-assert (flpositive? posinf))

     (test-assert (flnegative? neginf))
     (test-assert (flnegative? (fl- fl-least)))
     (test-deny   (flnegative? negzero))    ; explicit in SRFI 144
     (test-deny   (flnegative? zero))
     (test-deny   (flnegative? fl-least))
     (test-deny   (flnegative? posinf))

     (test-deny   (flodd? zero))
     (test-assert (flodd? one))

     (test-assert (fleven? zero))
     (test-deny   (fleven? one))

     (test (map flfinite? somereals)
           (map (lambda (x) #t) somereals))
     (test (map flfinite? weird)
           (map (lambda (x) #f) weird))

     (test-assert (flinfinite? neginf))
     (test-assert (flinfinite? posinf))
     (test-deny   (flinfinite? nan))
     (test (map flinfinite? somereals)
           (map (lambda (x) #f) somereals))

     (test-deny   (flnan? neginf))
     (test-deny   (flnan? posinf))
     (test-assert (flnan? nan))
     (test (map flnan? somereals)
           (map (lambda (x) #f) somereals))

     (test-assert (flnormalized? fl-greatest))
     (test-deny   (flnormalized? fl-least))
     (test-deny   (fldenormalized? fl-greatest))
     (test-assert (fldenormalized? fl-least))

     ;; Arithmetic

     (test (fl+) zero)
     (test (fl+ zero) zero)
     (test (flzero? (fl+ negzero)) #t)
     (test (fl+ one) one)
     (test (fl+ one one) two)
     (test (fl+ nan one) nan)
     (test (fl+ one nan) nan)
     (test (map fl+ somereals somereals somereals)
           (map (lambda (x) (fl* (flonum 3) x))
                somereals))
     (test (map fl+ infinities infinities) infinities)
     (test (map flnan?
                (map fl+ infinities (reverse infinities)))
           (map (lambda (x) #t) infinities))
     
     (test (fl*) one)
     (test (fl* zero) zero)
     (test (flzero? (fl* negzero)) #t)
     (test (fl* one) one)
     (test (fl* one one) one)
     (test (fl* nan one) nan)
     (test (fl* one nan) nan)
     (test (map fl* somereals somereals somereals)
           (map (lambda (x) (flonum (expt x 3)))
                somereals))
     (test (map fl* infinities infinities)
           (map (lambda (x) posinf) infinities))
     (test (map fl* infinities (reverse infinities))
           (map (lambda (x) neginf) infinities))
     
     (test (fl- zero) negzero)
     (test (fl- negzero) zero)
     (test (fl- one) (flonum -1))
     (test (fl- one one) zero)
     (test (fl- nan one) nan)
     (test (fl- one nan) nan)
     (test (map fl- somereals somereals somereals)
           (map (lambda (x) (if (eqv? x zero) zero (fl- x)))
                somereals))
     (test (map flnan? (map fl- infinities infinities))
           '(#t #t))
     (test (map fl- infinities (reverse infinities))
           infinities)
     
     (test (fl/ zero) posinf)
     (test (fl/ negzero) neginf)
     (test (fl/ one) one)
     (test (fl/ one one) one)
     (test (fl/ nan one) nan)
     (test (fl/ one nan) nan)
     (test (map fl/ somereals somereals somereals)
           (map (lambda (x) (if (flzero? x) (fl/ zero zero) (fl/ x)))
                somereals))
     (test (map flnan? (map fl/ infinities infinities))
           '(#t #t))
     (test (map flnan? (map fl/ infinities (reverse infinities)))
           '(#t #t))
     
     (test (flabs zero) zero)
     (test (flabs negzero) zero)
     (test (flabs one) one)
     (test (flabs (flonum -5.25)) (flonum 5.25))
     
     (test (flabsdiff zero one) one)
     (test (flabsdiff one zero) one)
     (test (flabsdiff one one) zero)
     (test (flabsdiff posinf neginf) posinf)
     (test (flabsdiff neginf posinf) posinf)

     (test (flsgn posinf) one)
     (test (flsgn neginf) (fl- one))
     (test (flsgn zero) one)
     (test (flsgn negzero) (fl- one))
     (test (flsgn two) one)
     (test (flsgn (fl- two)) (fl- one))

     (test (flnumerator (flonum 2.25)) (flonum 9))
     (test (fldenominator (flonum 2.25)) (flonum 4))
     (test (flnumerator (flonum -2.25)) (flonum -9))
     (test (fldenominator (flonum -2.25)) (flonum 4))
     (test (map flnumerator ints) ints)
     (test (map fldenominator ints)
           (map (lambda (x) one) ints))
     (test (map flnumerator weird) weird)
     (test (map fldenominator infinities) (list one one))
     (test-assert (flnan? (flnumerator nan)))
     (test-assert (flnan? (fldenominator nan)))

     (test (flfloor    (flonum -3.125)) (flonum -4))
     (test (flceiling  (flonum -3.125)) (flonum -3))
     (test (flround    (flonum -3.125)) (flonum -3))
     (test (fltruncate (flonum -3.125)) (flonum -3))

     (test (flfloor    (flonum -3.75)) (flonum -4))
     (test (flceiling  (flonum -3.75)) (flonum -3))
     (test (flround    (flonum -3.75)) (flonum -4))
     (test (fltruncate (flonum -3.75)) (flonum -3))

     (test (flfloor    (flonum -3.5)) (flonum -4))
     (test (flceiling  (flonum -3.5)) (flonum -3))
     (test (flround    (flonum -3.5)) (flonum -4))
     (test (fltruncate (flonum -3.5)) (flonum -3))

     (test (map flfloor    ints) ints)
     (test (map flceiling  ints) ints)
     (test (map flround    ints) ints)
     (test (map fltruncate ints) ints)

     (test (map flfloor    posfracs) (map (lambda (x) zero) posfracs))
     (test (map flceiling  posfracs) (map (lambda (x) one) posfracs))
     (test (map flround    posfracs) (map (lambda (x) zero) posfracs))
     (test (map fltruncate posfracs) (map (lambda (x) zero) posfracs))

     (test (map flfloor    weird) weird)
     (test (map flceiling  weird) weird)
     (test (map flround    weird) weird)
     (test (map fltruncate weird) weird)

     )))

;;;     flexp
;;;     flexp2
;;;     flexp-1
;;;     flsquare
;;;     flsqrt
;;;     flcbrt
;;;     flhypot
;;;     flexpt
;;;     fllog
;;;     fllog1+
;;;     fllog2
;;;     fllog10
;;;     make-fllog-base
;;;
;;;     flsin
;;;     flcos
;;;     fltan
;;;     flasin
;;;     flacos
;;;     flatan
;;;     flsinh
;;;     flcosh
;;;     fltanh
;;;     flasinh
;;;     flacosh
;;;     flatanh
;;;
;;;     flquotient
;;;     flremainder
;;;     flremquo
;;;
;;;     flgamma
;;;     flloggamma
;;;     flfirst-bessel
;;;     flsecond-bessel
;;;     flerf
;;;     flerfc
