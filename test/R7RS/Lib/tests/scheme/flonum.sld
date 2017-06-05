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
          (scheme inexact))

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

   (define zero (flonum 0))
   (define one (flonum 1))

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

     (test/= 3.1415926535897982384626433832795028841972    fl-pi)
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
     (test-assert (= 1 (/ (+ 1.0 fl-epsilon) 2)))
     (test-assert (= 0 (/ fl-least 2)))

     (test-assert (boolean? fl-fast-fl+*))
     (test-assert (exact-integer? fl-integer-exponent-zero))
     (test-assert (exact-integer? fl-integer-exponent-nan))

     ;; Constructors (FIXME)

     ;;     flonum
     ;;     fladjacent
     ;;     flcopysign
     ;;     make-flonum

     ;; Accessors (FIXME)

     ;;     flinteger-fraction
     ;;     flexponent
     ;;     flinteger-exponent
     ;;     flnormalized-fraction-exponent
     ;;     flsign-bit

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
     (test-assert (flzero? (flonum -0.0)))
     (test-assert (flzero? zero))
     (test-deny   (flzero? fl-least))
     (test-deny   (flzero? posinf))

     (test-deny   (flpositive? neginf))
     (test-deny   (flpositive? (fl- fl-least)))
     (test-deny   (flpositive? (flonum -0.0)))
     (test-deny   (flpositive? zero))
     (test-assert (flpositive? fl-least))
     (test-assert (flpositive? posinf))

     (test-assert (flnegative? neginf))
     (test-assert (flnegative? (fl- fl-least)))
     (test-deny   (flnegative? (flonum -0.0)))    ; explicit in SRFI 144
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

;;;     flnormalized?
;;;     fldenormalized?

     ;; Arithmetic

     (test (fl+) zero)
     (test (fl+ zero) zero)
     (test (fl+ (flonum -0.0)) (flonum -0.0))
     (test (fl+ one) one)
     (test (fl+ one one) (flonum 2))
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
     (test (fl* (flonum -0.0)) (flonum -0.0))
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
     


     )))

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
