;;; SRFI 144 (flonums, in draft status)
;;;
;;; $Id$
;;;
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


(define-library (srfi 144)

  (export

   ;; Mathematical Constants

   fl-e
   fl-1/e
   fl-e-2
   fl-e-pi/4
   fl-log2-e
   fl-log10-e
   fl-log-2
   fl-1/log-2
   fl-log-3
   fl-log-pi
   fl-log-10
   fl-1/log-10
   fl-pi
   fl-1/pi
   fl-2pi
   fl-pi/2
   fl-pi/4
   fl-2/sqrt-pi
   fl-pi-squared
   fl-degree
   fl-2/pi
   fl-sqrt-2
   fl-sqrt-3
   fl-sqrt-5
   fl-sqrt-10
   fl-1/sqrt-2
   fl-cbrt-2
   fl-cbrt-3
   fl-4thrt-2
   fl-phi
   fl-log-phi
   fl-1/log-phi
   fl-euler
   fl-e-euler
   fl-sin-1
   fl-cos-1
   fl-gamma-1/2
   fl-gamma-1/3
   fl-gamma-2/3

   ;; Implementation Constants

   fl-greatest
   fl-least
   fl-epsilon
   fl-fast-fl+*
   fl-integer-exponent-zero
   fl-integer-exponent-nan

   ;; Constructors

   flonum
   fladjacent
   flcopysign
   make-flonum

   ;; Accessors

   flinteger-fraction
   flexponent
   flinteger-exponent
   flnormalized-fraction-exponent
   flsign-bit

   ;; Predicates

   flonum?
   fl=?
   fl<?
   fl>?
   fl<=?
   fl>=?
   flunordered?
   flmax
   flmin
   flinteger?
   flzero?
   flpositive?
   flnegative?
   flodd?
   fleven?
   flfinite?
   flinfinite?
   flnan?
   flnormalized?
   fldenormalized?

   ;; Arithmetic

   fl+
   fl*
   fl+*
   fl-
   fl/
   flabs
   flabsdiff
   flposdiff
   flsgn
   flnumerator
   fldenominator
   flfloor
   flceiling
   flround
   fltruncate

   ;; Exponents and logarithsm

   flexp
   flexp2
   flexp-1
   flsquare
   flsqrt
   flcbrt
   flhypot
   flexpt
   fllog
   fllog1+
   fllog2
   fllog10
   make-fllog-base

   ;; Trigonometric functions

   flsin
   flcos
   fltan
   flasin
   flacos
   flatan
   flsinh
   flcosh
   fltanh
   flasinh
   flacosh
   flatanh

   ;; Integer division

   flquotient
   flremainder
   flremquo

   ;; Special functions

   flgamma
   flloggamma
   flfirst-bessel
   flsecond-bessel
   flerf
   flerfc
   )

  (import (scheme base)
          (scheme inexact))

  ;; Use (rnrs arithmetic flonums) if that library is available.

  (cond-expand
   ((library (rnrs arithmetic flonums))
    (import (except (rnrs arithmetic flonums)
                    flmax flmin flnumerator fldenominator)
            (prefix (only (rnrs arithmetic flonums)
                          flnumerator fldenominator)
                    r6rs:)))
   (else
    (import (scheme complex))))

  ;; Use an FFI if one is available.

  (cond-expand
   (larceny
    (import (rename (primitives fl+* flfirst-bessel flsecond-bessel)
                    (fl+* fma)
                    (flfirst-bessel r5rs:flfirst-bessel)
                    (flsecond-bessel r5rs:flsecond-bessel))))
   ((and larceny i386 unix (or gnu-linux darwin))    ; FIXME: no longer used
    (import (rename (primitives r5rs:require)
                    (r5rs:require require))
            (primitives foreign-procedure)))
   (else))

  (include "144.constants.scm")
  (include "144.body0.scm")

  ;; If (rnrs arithmetic flonums) is not available, fake it.

  (cond-expand
   ((not (library (rnrs arithmetic flonums)))
    (include "144.r6rs.scm"))
   (else))

  (include "144.body.scm")
  (include "144.special.scm")

  ;; If the C library is available, use it.

  (cond-expand

   ;; Larceny now defines fl+*, flfirst-bessel, and flsecond-bessel
   ;; at the R5RS level.  See renaming import above.

   (larceny
    (begin (define c-functions-are-available #t)
           (define fl-fast-fl+* #f)
           (define (jn n x) (r5rs:flfirst-bessel n x))
           (define (yn n x) (r5rs:flsecond-bessel n x))))

   ((and larceny i386 unix (or gnu-linux darwin))   ; FIXME: no longer used
    (begin (define c-functions-are-available #t)
           (define fl-fast-fl+* #f))
    (include "144.ffi.scm"))

   (else
    (begin (define c-functions-are-available #f)
           (define fl-fast-fl+* #f)
           (define (fma x y z) (error "fma not defined"))
           (define (jn n x) (error "jn not defined"))
           (define (yn n x) (error "yn not defined")))))
  )

;;; eof
