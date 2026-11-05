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

(define (negative-one-to-one)
  (let ((i 0))
    (lambda ()
      (if (> i 1024)
          #f
          (let ((result (/ (- i 512) 512.0)))
            (set! i (+ i 1))
            result)))))

(define (negative-eight-to-eight)
  (let ((next (negative-one-to-one)))
    (lambda ()
      (let ((result (next)))
        (if result (* 8.0 result) result)))))

(define (negative-2pi-to-2pi)
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

(define (write-relative-error name f0 f1 next . rest)
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
      (let retry ((y0 (f0 x))
                  (y1 (f1 x)))
        (cond ((fl=? y0 y1)
               0.0)
              ((and (flnan? y0) (flnan? y1))
               0.0)
              ((or (flnan? y0) (flnan? y1))
               +inf.0)
              ((flinfinite? y0)
               (retry fl-greatest y1))
              ((flinfinite? y1)
               (retry y0 fl-greatest))
              ((fl<? -1.0 (flabs y0) 1.0) ; if y0 < 1, report absolute error
               (flsquare (fl- y1 y0)))
              (else
               (flsquare (fl/ (fl- y1 y0) y0))))))
    (display "# " p)
    (display name p)
    (display " (relative error)" p)
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

(define (write-relative-error-to-file name f0 f1 next filename)
  (let ((filename (string-append output-directory "/" filename)))
    (delete-file filename)
    (call-with-output-file
     filename
     (lambda (p)
       (write-relative-error name f0 f1 next p)))))


(write-relative-error-to-file "log" c:log fllog (zero-to-infinity)
                              "log.data")
(write-relative-error-to-file "sin" c:sin flsin (negative-2pi-to-2pi)
                              "sin.data")
(write-relative-error-to-file "sinh"
                              sinh
                              flsinh
                              (negative-infinity-to-infinity)
                              "sinh.data")
(write-relative-error-to-file "cosh"
                              cosh
                              flcosh
                              (negative-infinity-to-infinity)
                              "cosh.data")
(write-relative-error-to-file "tanh"
                              tanh
                              fltanh
                              (negative-infinity-to-infinity)
                              "tanh.data")
(write-relative-error-to-file "asinh"
                              asinh
                              flasinh
                              (negative-infinity-to-infinity)
                              "asinh.data")
(write-relative-error-to-file "acosh"
                              acosh
                              flacosh
                              (zero-to-infinity)
                              "acosh.data")
(write-relative-error-to-file "atanh"
                              atanh
                              flatanh
                              (negative-one-to-one)
                              "atanh.data")
(write-relative-error-to-file "Gamma" tgamma flgamma
                              (negative-infinity-to-infinity)
                              "gamma.data")
(write-relative-error-to-file "Gamma" tgamma flgamma (negative-eight-to-eight)
                              "gamma-small.data")
(write-relative-error-to-file "J_0"
                      (lambda (x) (jn 0 x))
                      (lambda (x) (flfirst-bessel 0 x))
                      (negative-infinity-to-infinity)
                      "j0.data")
(write-relative-error-to-file "J_0"
                      (lambda (x) (jn 0 x))
                      (lambda (x) (flfirst-bessel 0 x))
                      (negative-eight-to-eight)
                      "j0-small.data")
(write-relative-error-to-file "Y_0"
                      (lambda (x) (yn 0 x))
                      (lambda (x) (flsecond-bessel 0 x))
                      (negative-infinity-to-infinity)
                      "y0.data")
(write-relative-error-to-file "Y_0"
                      (lambda (x) (yn 0 x))
                      (lambda (x) (flsecond-bessel 0 x))
                      (negative-eight-to-eight)
                      "y0-small.data")
(write-relative-error-to-file "J_1"
                      (lambda (x) (jn 1 x))
                      (lambda (x) (flfirst-bessel 1 x))
                      (negative-infinity-to-infinity)
                      "j1.data")
(write-relative-error-to-file "J_1"
                      (lambda (x) (jn 1 x))
                      (lambda (x) (flfirst-bessel 1 x))
                      (negative-eight-to-eight)
                      "j1-small.data")
(write-relative-error-to-file "Y_1"
                      (lambda (x) (yn 1 x))
                      (lambda (x) (flsecond-bessel 1 x))
                      (negative-infinity-to-infinity)
                      "y1.data")
(write-relative-error-to-file "Y_1"
                      (lambda (x) (yn 1 x))
                      (lambda (x) (flsecond-bessel 1 x))
                      (negative-eight-to-eight)
                      "y1-small.data")
(write-relative-error-to-file "J_2"
                      (lambda (x) (jn 2 x))
                      (lambda (x) (flfirst-bessel 2 x))
                      (negative-infinity-to-infinity)
                      "j2.data")
(write-relative-error-to-file "J_2"
                      (lambda (x) (jn 2 x))
                      (lambda (x) (flfirst-bessel 2 x))
                      (negative-eight-to-eight)
                      "j2-small.data")
(write-relative-error-to-file "Y_2"
                      (lambda (x) (yn 2 x))
                      (lambda (x) (flsecond-bessel 2 x))
                      (negative-infinity-to-infinity)
                      "y2.data")
(write-relative-error-to-file "Y_2"
                      (lambda (x) (yn 2 x))
                      (lambda (x) (flsecond-bessel 2 x))
                      (negative-eight-to-eight)
                      "y2-small.data")
(write-relative-error-to-file "J_3"
                      (lambda (x) (jn 3 x))
                      (lambda (x) (flfirst-bessel 3 x))
                      (negative-infinity-to-infinity)
                      "j3.data")
(write-relative-error-to-file "J_3"
                      (lambda (x) (jn 3 x))
                      (lambda (x) (flfirst-bessel 3 x))
                      (negative-eight-to-eight)
                      "j3-small.data")
(write-relative-error-to-file "Y_3"
                      (lambda (x) (yn 3 x))
                      (lambda (x) (flsecond-bessel 3 x))
                      (negative-infinity-to-infinity)
                      "y3.data")
(write-relative-error-to-file "Y_3"
                      (lambda (x) (yn 3 x))
                      (lambda (x) (flsecond-bessel 3 x))
                      (negative-eight-to-eight)
                      "y3-small.data")
(write-relative-error-to-file "J_4"
                      (lambda (x) (jn 4 x))
                      (lambda (x) (flfirst-bessel 4 x))
                      (negative-infinity-to-infinity)
                      "j4.data")
(write-relative-error-to-file "J_4"
                      (lambda (x) (jn 4 x))
                      (lambda (x) (flfirst-bessel 4 x))
                      (negative-eight-to-eight)
                      "j4-small.data")
(write-relative-error-to-file "Y_4"
                      (lambda (x) (yn 4 x))
                      (lambda (x) (flsecond-bessel 4 x))
                      (negative-infinity-to-infinity)
                      "y4.data")
(write-relative-error-to-file "Y_4"
                      (lambda (x) (yn 4 x))
                      (lambda (x) (flsecond-bessel 4 x))
                      (negative-eight-to-eight)
                      "y4-small.data")
(write-relative-error-to-file "J_100"
                      (lambda (x) (jn 100 x))
                      (lambda (x) (flfirst-bessel 100 x))
                      (negative-infinity-to-infinity)
                      "j100.data")
(write-relative-error-to-file "J_100"
                      (lambda (x) (jn 100 x))
                      (lambda (x) (flfirst-bessel 100 x))
                      (negative-eight-to-eight)
                      "j100-small.data")
(write-relative-error-to-file "Y_100"
                      (lambda (x) (yn 100 x))
                      (lambda (x) (flsecond-bessel 100 x))
                      (negative-infinity-to-infinity)
                      "y100.data")
(write-relative-error-to-file "Y_100"
                      (lambda (x) (yn 100 x))
                      (lambda (x) (flsecond-bessel 100 x))
                      (negative-eight-to-eight)
                      "y100-small.data")
(write-relative-error-to-file "erf" erf flerf (negative-infinity-to-infinity)
                              "erf.data")
(write-relative-error-to-file "erf" erf flerf (negative-eight-to-eight)
                              "erf-small.data")
(write-relative-error-to-file "erfc" erfc flerfc (negative-infinity-to-infinity)
                              "erfc.data")
(write-relative-error-to-file "erfc" erfc flerfc (negative-eight-to-eight)
                              "erfc-small.data")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Benchmarking portable definitions against the C definitions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax cmp
  (syntax-rules ()
   ((cmp f1 f2 g)
    (compare 'f1 'f2 f1 f2 g))))

;;; Given the names of two functions, the two unary functions themselves,
;;; and a thunk that returns a generator of test inputs, compares the
;;; speed of the functions and writes a summary to the current output port.

(define (compare name1 name2 f1 f2 thunk)
  (define xs0
    (let loop ((xs '())
               (g (thunk)))
      (let ((x (g)))
        (if x
            (loop (cons x xs) g)
            (reverse xs)))))
  (define test-time-in-jiffies
    (max 20 (quotient (jiffies-per-second) 2)))
  (define (iterations-per-test-time f)
    (let ((completion-time (+ (current-jiffy) test-time-in-jiffies)))
      (define (loop count xs)
        (cond ((not (null? xs))
               (f (car xs))
               (loop count (cdr xs)))
              ((< (current-jiffy) completion-time)
               (loop (+ 1 count) xs0))
              (else
               count)))
      (loop 0 xs0)))
  (define (round1/ p q)
    (/ (round (/ (* 10.0 p) (max 1.0 q))) 10.0))
  (display "Comparing ")
  (write name1)
  (display " and ")
  (write name2)
  (newline)
  (let* ((count1 (iterations-per-test-time f1))
         (count2 (iterations-per-test-time f2)))
;   (write (list count1 count2)) (newline)
    (display "        ")
    (write (max (round1/ count2 count1) (round1/ count1 count2)))
    (display " times as ")
    (display (if (< count1 count2)
                 "slow ********************************"
                 "fast"))
    (newline) (newline)))

(cmp (lambda (x) (fladjacent x 1.0))
     (lambda (x) (nextafter x 1.0))
     negative-infinity-to-infinity)

(cmp (lambda (x) (flcopysign x 5.6))
     (lambda (x) (copysign 3.4 5.6))
     negative-infinity-to-infinity)

(cmp (lambda (x) (make-flonum x 5))
     (lambda (x) (ldexp x 5))
     negative-infinity-to-infinity)

#;
(let ((flinteger-fraction
       (lambda (x)
         (call-with-values (lambda () (flinteger-fraction x))
                           (lambda (x y) x)))))
  (cmp (flinteger-fraction 3.4) (modf 3.4 FIXME)))

(cmp flexponent logb zero-to-infinity)

;(cmp (flinteger-exponent 3.4) (ilogb 3.4))
#;
(let ((flnormalized-fraction-exponent
       (lambda (x)
         (call-with-values (lambda () (flnormalized-fraction-exponent 3.4))
                           (lambda (x y) x)))))
  (cmp (flnormalized-fraction-exponent 3.4) (frexp 3.4 FIXME)))


(cmp (lambda (x) (fl+* x x x))
     (lambda (x) (fma x x x))
     negative-eight-to-eight)
(cmp (lambda (x) (fl+* x x x))
     (lambda (x) (+ (* x x) x))
     negative-eight-to-eight)
(cmp flabs fabs negative-infinity-to-infinity)
(cmp (lambda (x) (flposdiff x 5.6))
     (lambda (x) (fdim x 5.6))
     negative-infinity-to-infinity)
(cmp flfloor c:floor negative-infinity-to-infinity)
(cmp flceiling ceil negative-infinity-to-infinity)
(cmp flround c:round negative-infinity-to-infinity)
(cmp fltruncate trunc negative-infinity-to-infinity)

(cmp flexp c:exp negative-infinity-to-infinity)
(cmp flexp2 exp2 negative-infinity-to-infinity)
(cmp flexp-1 expm1 negative-infinity-to-infinity)
(cmp flsqrt c:sqrt zero-to-infinity)
(cmp flcbrt cbrt negative-infinity-to-infinity)
(cmp (lambda (x) (flhypot x x))
     (lambda (x) (hypot x x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flexpt x x))
     (lambda (x) (pow x x))
     zero-to-infinity)
(cmp fllog c:log zero-to-infinity)
(cmp fllog1+ log1p zero-to-infinity)
(cmp fllog2 log2 zero-to-infinity)
(cmp fllog10 log10 zero-to-infinity)

(cmp flsin c:sin negative-infinity-to-infinity)
(cmp flcos c:cos negative-infinity-to-infinity)
(cmp fltan c:tan negative-infinity-to-infinity)
(cmp flasin c:asin negative-one-to-one)
(cmp flacos c:acos negative-one-to-one)
(cmp flatan c:atan negative-infinity-to-infinity)
(cmp (lambda (y) (flatan y 5.6))
     (lambda (y) (atan2 y 5.6))
     negative-infinity-to-infinity)

(cmp flsinh sinh negative-eight-to-eight)
(cmp flcosh cosh negative-eight-to-eight)
(cmp fltanh tanh negative-2pi-to-2pi)
(cmp flasinh asinh zero-to-one)
(cmp flacosh acosh zero-to-infinity)
(cmp flatanh atanh negative-one-to-one)

(cmp flgamma tgamma negative-infinity-to-infinity)
(cmp flloggamma lgamma negative-infinity-to-infinity)

(cmp (lambda (x) (flfirst-bessel 0 x))
     (lambda (x) (jn 0 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flfirst-bessel 1 x))
     (lambda (x) (jn 1 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flfirst-bessel 2 x))
     (lambda (x) (jn 2 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flfirst-bessel 5 x))
     (lambda (x) (jn 5 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flfirst-bessel 100 x))
     (lambda (x) (jn 100 x))
     negative-infinity-to-infinity)

(cmp (lambda (x) (flsecond-bessel 0 x))
     (lambda (x) (yn 0 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flsecond-bessel 1 x))
     (lambda (x) (yn 1 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flsecond-bessel 2 x))
     (lambda (x) (yn 2 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flsecond-bessel 5 x))
     (lambda (x) (yn 5 x))
     negative-infinity-to-infinity)
(cmp (lambda (x) (flsecond-bessel 100 x))
     (lambda (x) (yn 100 x))
     negative-infinity-to-infinity)

(cmp flerf erf negative-eight-to-eight)
(cmp flerfc erfc negative-eight-to-eight)
