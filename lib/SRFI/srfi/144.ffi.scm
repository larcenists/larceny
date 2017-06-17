;;; For most of the procedures specified by SRFI 144, it's faster
;;; and just as accurate to use the portable definition instead
;;; of going through Larceny's FFI.
;;;
;;; Only three of the SRFI 144 procedures are substantially more
;;; accurate or faster when implemented using Larceny's FFI:
;;;
;;;     fl+*
;;;     flfirst-bessel
;;;     flsecond-bessel

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
