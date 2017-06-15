;;; For most of the procedures specified by SRFI 144, it's faster
;;; and just as accurate to use the portable definition instead
;;; of going through Larceny's FFI.
;;;
;;; Of the SRFI 144 procedures implemented so far, only two are
;;; substantially faster when implemented using Larceny's FFI:
;;;
;;;     fl+*
;;;     flfirst-bessel
;;;
;;; When the following procedures have been implemented, I expect
;;; they too will be faster when implemented using an FFI:
;;;
;;;     flsecond-bessel
;;;     flerf
;;;     flerfc

(define ignored-result-of-loading-ffi (require 'std-ffi))

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
;;; Benchmarking portable definitions against the C definitions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
