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

;;; This is a Larceny-specific program that tests the accuracy and
;;; speed of SRFI 144 procedures, producing gnuplot-compatible
;;; data files that can be used to graph the accuracy of selected
;;; procedures.
;;;
;;; This program uses Larceny's FFI to access C library routines.
;;; Larceny's FFI is not implemented on all platforms, so
;;; platform-independent scripts should not run this test program.
;;;
;;; It should be possible to modify the FFI-dependent part of this
;;; test program so it will run on other systems that provide an FFI
;;; to C libraries.

(import (scheme base)
        (scheme inexact)
        (srfi 144)
        (scheme file)
        (scheme write)
        (scheme time)
        (rename (primitives r5rs:require)
                (r5rs:require require))
        (primitives foreign-procedure))

(define output-directory "/tmp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FFI-dependent definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'std-ffi)

(define nextafter (foreign-procedure "nextafter" '(double double) 'double))
(define copysign  (foreign-procedure "copysign"  '(double double) 'double))
(define ldexp     (foreign-procedure "ldexp"     '(double int)    'double))
(define modf      (foreign-procedure "modf"      '(double int)    'double))
(define logb      (foreign-procedure "logb"      '(double)        'double))
(define ilogb     (foreign-procedure "ilogb"     '(double)        'int))
(define frexp     (foreign-procedure "frexp"     '(double boxed)  'double))
;(define signbit     'MACRO)

;(define isfinite    'MACRO)
;(define isinf       'MACRO)
;(define isnan       'MACRO)
;(define isnormal    'MACRO)    ; fpclassify is also a macro
;(define issubnormal 'MACRO)    ; fpclassify is also a macro

(define fma      (foreign-procedure "fma" '(double double double) 'double))
(define fabs      (foreign-procedure "fabs"      '(double)        'double))
(define fdim      (foreign-procedure "fdim"      '(double double) 'double))
(define c:floor   (foreign-procedure "floor"     '(double)        'double))
(define ceil      (foreign-procedure "ceil"      '(double)        'double))
(define c:round   (foreign-procedure "round"     '(double)        'double))
(define trunc     (foreign-procedure "trunc"     '(double)        'double))

(define c:exp     (foreign-procedure "exp"       '(double)        'double))
(define exp2      (foreign-procedure "exp2"      '(double)        'double))
(define expm1     (foreign-procedure "expm1"     '(double)        'double))
(define c:sqrt    (foreign-procedure "sqrt"      '(double)        'double))
(define cbrt      (foreign-procedure "cbrt"      '(double)        'double))
(define hypot     (foreign-procedure "hypot"     '(double double) 'double))
(define pow       (foreign-procedure "pow"       '(double double) 'double))
(define c:log     (foreign-procedure "log"       '(double)        'double))
(define log1p     (foreign-procedure "log1p"     '(double)        'double))
(define log2      (foreign-procedure "log2"      '(double)        'double))
(define log10     (foreign-procedure "log10"     '(double)        'double))

(define c:sin     (foreign-procedure "sin"       '(double)        'double))
(define c:cos     (foreign-procedure "cos"       '(double)        'double))
(define c:tan     (foreign-procedure "tan"       '(double)        'double))
(define c:asin    (foreign-procedure "asin"      '(double)        'double))
(define c:acos    (foreign-procedure "acos"      '(double)        'double))
(define c:atan    (foreign-procedure "atan"      '(double)        'double))
(define atan2     (foreign-procedure "atan2"     '(double double) 'double))

(define sinh      (foreign-procedure "sinh"      '(double)        'double))
(define cosh      (foreign-procedure "cosh"      '(double)        'double))
(define tanh      (foreign-procedure "tanh"      '(double)        'double))
(define asinh     (foreign-procedure "asinh"     '(double)        'double))
(define acosh     (foreign-procedure "acosh"     '(double)        'double))
(define atanh     (foreign-procedure "atanh"     '(double)        'double))

(define tgamma    (foreign-procedure "tgamma"    '(double)        'double))
(define lgamma    (foreign-procedure "lgamma"    '(double)        'double))
(define jn        (foreign-procedure "jn"        '(int double)    'double))
(define yn        (foreign-procedure "yn"        '(int double)    'double))
(define erf       (foreign-procedure "erf"       '(double)        'double))
(define erfc      (foreign-procedure "erfc"      '(double)        'double))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of FFI-dependent definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generators of test inputs.
;;; Each generator should produce about 1024 test inputs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (zero-to-one)
  (let ((i 0))
    (lambda ()
      (if (> i 1024)
          #f
          (let ((result (/ i 1024.0)))
            (set! i (+ i 1))
            result)))))

(define (minus-one-to-one)
  (let ((i 0))
    (lambda ()
      (if (> i 1024)
          #f
          (let ((result (/ (- i 512) 512.0)))
            (set! i (+ i 1))
            result)))))

(define (minus-eight-to-eight)
  (let ((next (minus-one-to-one)))
    (lambda ()
      (let ((result (next)))
        (if result (* 8.0 result) result)))))

(define (minus-2pi-to-2pi)
  (let ((i 0))
    (lambda ()
      (if (> i 1024)
          #f
          (let ((result (fl/ (fl* fl-2pi (- i 512.0)) 512.0)))
            (set! i (+ i 1))
            result)))))

(define (zero-to-infinity)
  (let ((i 0))
    (lambda ()
      (if (> i 1025)
          #f
          (let ((result (flonum (expt 2.0 (- (+ i i) 1025)))))
            (set! i (+ i 1))
            result)))))

(define (negative-infinity-to-infinity)
  (let ((i 0))
    (lambda ()
      (if (> i 1024)
          #f
          (cond ((< i 512)
                 (let ((result (fl- (flonum (expt 2.0 (- 1024 i i i i))))))
                   (set! i (+ i 1))
                   result))
                ((= i 512)
                 (set! i (+ i 1))
                 0.0)
                (else
                 (let ((result (flonum (expt 2.0 (+ i i i i -3072)))))
                   (set! i (+ i 1))
                   result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accuracy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given the name of a function, two unary functions, a generator
;;; of test inputs, and (optionally) an output port, where the
;;; first unary function is assumed to be perfectly accurate,
;;; writes RMS error (over a small window) to the output port.

(define (write-absolute-error name f0 f1 next . rest)
  (let ((p (if (null? rest) (current-output-port) (car rest))))
    (define (loop x-3 x-2 x-1 x x+1 x+2 x+3
                  e-3 e-2 e-1 e e+1 e+2 e+3)
      (let ((rms (flsqrt (fl/ (fl+ e-3 e-2 e-1 e e+1 e+2 e+3) 7.0))))
        (write x p)
        (display "    " p)
        (write rms p)
        (if (not (flfinite? rms))
            (begin (display "    # " p)
                   (write (map f1 (list x-3 x-2 x-1 x x+1 x+2 x+3)) p)))
        (newline p)
        (let ((x+4 (next)))
          (if x+4
              (loop x-2 x-1 x x+1 x+2 x+3 x+4
                    e-2 e-1 e e+1 e+2 e+3 (square-error x+4))
              (display "\n\n" p)))))
    (define (square-error x)
      (let ((y0 (f0 x))
            (y1 (f1 x)))
        (cond ((fl=? y0 y1)
               0.0)
              ((or (flinfinite? y0) (flinfinite? y1))
               +inf.0)
              ((and (flnan? y0) (flnan? y1))
               0.0)
              (else
               (flsquare (fl- y1 y0))))))
    (display "# " p)
    (display name p)
    (newline p)
    (let* ((x-3 (next))
           (x-2 (next))
           (x-1 (next))
           (x   (next))
           (x+1 (next))
           (x+2 (next))
           (x+3 (next)))
      (if x+3
          (loop x-3 x-2 x-1 x x+1 x+2 x+3
                (square-error x-3)
                (square-error x-2)
                (square-error x-1)
                (square-error x)
                (square-error x+1)
                (square-error x+2)
                (square-error x+3))))))

(define (write-absolute-error-to-file name f0 f1 next filename)
  (let ((filename (string-append output-directory "/" filename)))
    (delete-file filename)
    (call-with-output-file
     filename
     (lambda (p)
       (write-absolute-error name f0 f1 next p)))))

(write-absolute-error-to-file "log" c:log fllog (zero-to-infinity)
                              "log.data")
(write-absolute-error-to-file "sin" c:sin flsin (minus-2pi-to-2pi)
                              "sin.data")
#;
(write-absolute-error-to-file "Gamma" tgamma flgamma
                              (let ((next (negative-infinity-to-infinity)))
                                (next)
                                next)
                              "gamma.data")
(write-absolute-error-to-file "Gamma" tgamma flgamma
                              (negative-infinity-to-infinity)
                              "gamma.data")
(write-absolute-error-to-file "Gamma" tgamma flgamma (minus-eight-to-eight)
                              "gamma-small.data")
(write-absolute-error-to-file "J_0"
                      (lambda (x) (jn 0 x))
                      (lambda (x) (flfirst-bessel x 0))
                      (negative-infinity-to-infinity)
                      "j0.data")
(write-absolute-error-to-file "J_0"
                      (lambda (x) (jn 0 x))
                      (lambda (x) (flfirst-bessel x 0))
                      (minus-eight-to-eight)
                      "j0-small.data")
(write-absolute-error-to-file "Y_0"
                      (lambda (x) (yn 0 x))
                      (lambda (x) (flsecond-bessel x 0))
                      (negative-infinity-to-infinity)
                      "y0.data")
(write-absolute-error-to-file "Y_0"
                      (lambda (x) (yn 0 x))
                      (lambda (x) (flsecond-bessel x 0))
                      (minus-eight-to-eight)
                      "y0-small.data")
(write-absolute-error-to-file "J_1"
                      (lambda (x) (jn 1 x))
                      (lambda (x) (flfirst-bessel x 1))
                      (negative-infinity-to-infinity)
                      "j1.data")
(write-absolute-error-to-file "J_1"
                      (lambda (x) (jn 1 x))
                      (lambda (x) (flfirst-bessel x 1))
                      (minus-eight-to-eight)
                      "j1-small.data")
(write-absolute-error-to-file "Y_1"
                      (lambda (x) (yn 1 x))
                      (lambda (x) (flsecond-bessel x 1))
                      (negative-infinity-to-infinity)
                      "y1.data")
(write-absolute-error-to-file "Y_1"
                      (lambda (x) (yn 1 x))
                      (lambda (x) (flsecond-bessel x 1))
                      (minus-eight-to-eight)
                      "y1-small.data")
(write-absolute-error-to-file "J_2"
                      (lambda (x) (jn 2 x))
                      (lambda (x) (flfirst-bessel x 2))
                      (negative-infinity-to-infinity)
                      "j2.data")
(write-absolute-error-to-file "J_2"
                      (lambda (x) (jn 2 x))
                      (lambda (x) (flfirst-bessel x 2))
                      (minus-eight-to-eight)
                      "j2-small.data")
(write-absolute-error-to-file "Y_2"
                      (lambda (x) (yn 2 x))
                      (lambda (x) (flsecond-bessel x 2))
                      (negative-infinity-to-infinity)
                      "y2.data")
(write-absolute-error-to-file "Y_2"
                      (lambda (x) (yn 2 x))
                      (lambda (x) (flsecond-bessel x 2))
                      (minus-eight-to-eight)
                      "y2-small.data")
(write-absolute-error-to-file "J_3"
                      (lambda (x) (jn 3 x))
                      (lambda (x) (flfirst-bessel x 3))
                      (negative-infinity-to-infinity)
                      "j3.data")
(write-absolute-error-to-file "J_3"
                      (lambda (x) (jn 3 x))
                      (lambda (x) (flfirst-bessel x 3))
                      (minus-eight-to-eight)
                      "j3-small.data")
(write-absolute-error-to-file "Y_3"
                      (lambda (x) (yn 3 x))
                      (lambda (x) (flsecond-bessel x 3))
                      (negative-infinity-to-infinity)
                      "y3.data")
(write-absolute-error-to-file "Y_3"
                      (lambda (x) (yn 3 x))
                      (lambda (x) (flsecond-bessel x 3))
                      (minus-eight-to-eight)
                      "y3-small.data")
(write-absolute-error-to-file "J_4"
                      (lambda (x) (jn 4 x))
                      (lambda (x) (flfirst-bessel x 4))
                      (negative-infinity-to-infinity)
                      "j4.data")
(write-absolute-error-to-file "J_4"
                      (lambda (x) (jn 4 x))
                      (lambda (x) (flfirst-bessel x 4))
                      (minus-eight-to-eight)
                      "j4-small.data")
(write-absolute-error-to-file "Y_4"
                      (lambda (x) (yn 4 x))
                      (lambda (x) (flsecond-bessel x 4))
                      (negative-infinity-to-infinity)
                      "y4.data")
(write-absolute-error-to-file "Y_4"
                      (lambda (x) (yn 4 x))
                      (lambda (x) (flsecond-bessel x 4))
                      (minus-eight-to-eight)
                      "y4-small.data")
(write-absolute-error-to-file "J_100"
                      (lambda (x) (jn 100 x))
                      (lambda (x) (flfirst-bessel x 100))
                      (negative-infinity-to-infinity)
                      "j100.data")
(write-absolute-error-to-file "J_100"
                      (lambda (x) (jn 100 x))
                      (lambda (x) (flfirst-bessel x 100))
                      (minus-eight-to-eight)
                      "j100-small.data")
(write-absolute-error-to-file "Y_100"
                      (lambda (x) (yn 100 x))
                      (lambda (x) (flsecond-bessel x 100))
                      (negative-infinity-to-infinity)
                      "y100.data")
(write-absolute-error-to-file "Y_100"
                      (lambda (x) (yn 100 x))
                      (lambda (x) (flsecond-bessel x 100))
                      (minus-eight-to-eight)
                      "y100-small.data")
(write-absolute-error-to-file "erf" erf flerf (negative-infinity-to-infinity)
                              "erf.data")
(write-absolute-error-to-file "erf" erf flerf (minus-eight-to-eight)
                              "erf-small.data")
(write-absolute-error-to-file "erfc" erfc flerfc (negative-infinity-to-infinity)
                              "erfc.data")
(write-absolute-error-to-file "erfc" erfc flerfc (minus-eight-to-eight)
                              "erfc-small.data")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Benchmarking portable definitions against the C definitions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define c-functions-are-available #t) ; FIXME

(define-syntax cmp
  (syntax-rules ()
   ((cmp (f1 x1 ...) (f2 x2 ...))
    (if (not c-functions-are-available)
        (compare 'f1 'f2 (lambda () (f1 x1 ...)) (lambda () (f2 x2 ...)))))))

(define (compare name1 name2 thunk1 thunk2)
  (define iterations 100000)
  (define (call-thunk thunk iters)
    (if (> iters 1)
        (begin (thunk) (call-thunk thunk (- iters 1)))
        (thunk)))
  (define (round1/ p q)
    (/ (round (/ (* 10.0 p) (max 1.0 q))) 10.0))
  (display "Comparing ")
  (write name1)
  (display " and ")
  (write name2)
  (newline)
  (let* ((t0 (current-jiffy))
         (r1 (call-thunk thunk1 iterations))
         (t1 (current-jiffy))
         (d1 (- t1 t0))
         (t0 (current-jiffy))
         (r2 (call-thunk thunk2 iterations))
         (t1 (current-jiffy))
         (d2 (- t1 t0)))
    (write r1) (newline)
    (write r2) (newline)
    (display (if (< d1 d2) "        " "******* "))
    (write (max (round1/ d2 d1) (round1/ d1 d2)))
    (display " times as ")
    (display (if (< d1 d2) "fast" "slow ********************************"))
    (newline) (newline)))

(cmp (fladjacent 3.4 5.6) (nextafter 3.4 5.6))
(cmp (flcopysign 3.4 5.6) (copysign 3.4 5.6))
(cmp (make-flonum 3.4 5) (ldexp 3.4 5))
#;
(let ((flinteger-fraction
       (lambda (x)
         (call-with-values (lambda () (flinteger-fraction x))
                           (lambda (x y) x)))))
  (cmp (flinteger-fraction 3.4) (modf 3.4 FIXME)))
(cmp (flexponent 3.4) (logb 3.4))
;(cmp (flinteger-exponent 3.4) (ilogb 3.4))
#;
(let ((flnormalized-fraction-exponent
       (lambda (x)
         (call-with-values (lambda () (flnormalized-fraction-exponent 3.4))
                           (lambda (x y) x)))))
  (cmp (flnormalized-fraction-exponent 3.4) (frexp 3.4 FIXME)))

(cmp (fl+* 3.4 5.6 7.8) (fma 3.4 5.6 7.8))
(cmp (flabs 3.4) (fabs 3.4))
(cmp (flposdiff 3.4 5.6) (fdim 3.4 5.6))
(cmp (flfloor 3.4) (c:floor 3.4))
(cmp (flceiling 3.4) (ceil 3.4))
(cmp (flround 3.4) (c:round 3.4))
(cmp (fltruncate 3.4) (trunc 3.4))

(cmp (flexp 3.4) (c:exp 3.4))
(cmp (flexp2 3.4) (exp2 3.4))
(cmp (flexp-1 3.4) (expm1 3.4))
(cmp (flsqrt 3.4) (c:sqrt 3.4))
(cmp (flcbrt 3.4) (cbrt 3.4))
(cmp (flhypot 3.4 5.6) (hypot 3.4 5.6))
(cmp (flexpt 3.4 5.6) (pow 3.4 5.6))
(cmp (fllog 3.4) (c:log 3.4))
(cmp (fllog1+ 3.4) (log1p 3.4))
(cmp (fllog2 3.4) (log2 3.4))
(cmp (fllog10 3.4) (log10 3.4))

(cmp (flsin .4) (c:sin .4))
(cmp (flcos .4) (c:cos .4))
(cmp (fltan 3.4) (c:tan 3.4))
(cmp (flasin 0.4) (c:asin 0.4))
(cmp (flacos 0.4) (c:acos 0.4))
(cmp (flatan 3.4) (c:atan 3.4))
(cmp (flatan 3.4 5.6) (atan2 3.4 5.6))

(cmp (flsinh 3.4) (sinh 3.4))
(cmp (flcosh 3.4) (cosh 3.4))
(cmp (fltanh 3.4) (tanh 3.4))
(cmp (flasinh 3.4) (asinh 3.4))
(cmp (flacosh 3.4) (acosh 3.4))
(cmp (flatanh 0.4) (atanh .4))

(cmp (flgamma 3.4) (tgamma 3.4))
(cmp (flloggamma 3.4) (lgamma 3.4))
(cmp (flfirst-bessel 3.4 0) (jn 0 3.4))
(cmp (flsecond-bessel 3.4 0) (yn 0 3.4))
(cmp (flfirst-bessel 6.8 0) (jn 0 6.8))
(cmp (flsecond-bessel 6.8 0) (yn 0 6.8))
(cmp (flerf 0.4) (erf 0.4))
(cmp (flerfc 0.4) (erfc 0.4))
(cmp (flerf 3.4) (erf 3.4))
(cmp (flerfc 3.4) (erfc 3.4))
