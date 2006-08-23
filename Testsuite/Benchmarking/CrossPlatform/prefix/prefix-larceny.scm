(define FLOATabs abs)
(define FLOATsin sin)
(define FLOATcos cos)
(define FLOATatan atan)
(define FLOATsqrt sqrt)
(define FLOATmin min)
(define FLOATmax max)
(define FLOATround round)
(define FLOATinexact->exact inexact->exact)

(define bitwise-or logior)
(define bitwise-and logand)
(define bitwise-not lognot)

(define (fatal-error . args)
  (apply error #f args))

; Macros...

; Flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-vector n 0.0))
    ((FLOATmake-vector n init) (make-vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+)         0.0)
    ((FLOAT+ x)       x)
    ((FLOAT+ x y ...) (fl+ x (FLOAT+ y ...)))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x)       (fl-- x))
    ((FLOAT- x y ...) (fl- x (FLOAT+ y ...)))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT*)         1.0)
    ((FLOAT* x)       x)
    ((FLOAT* x y ...) (fl* x (FLOAT* y ...)))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x)       (/ x))
    ((FLOAT/ x y ...) (/ x (FLOAT* y ...)))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (fl= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (fl< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (fl<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (fl> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (fl>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (fl< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (fl> x 0.0))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (fl= x 0.0))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (abs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

; Fixnum arithmetic everywhere else.

(define-syntax +
  (syntax-rules ()
    ((+)         0)
    ((+ x)       x)
    ((+ x y ...) (fx+ x (+ y ...)))))

(define-syntax -
  (syntax-rules ()
    ((- x)       (fx-- x))
    ((- x y ...) (fx- x (+ y ...)))))

(define-syntax *
  (syntax-rules ()
    ((*)         1)
    ((* x)       x)
    ((* x y ...) (fx* x (* y ...)))))

(define-syntax =
  (syntax-rules ()
    ((= x y) (fx= x y))))

(define-syntax <
  (syntax-rules ()
    ((< x y) (fx< x y))))

(define-syntax <=
  (syntax-rules ()
    ((<= x y) (fx<= x y))))

(define-syntax >
  (syntax-rules ()
    ((> x y) (fx> x y))))

(define-syntax >=
  (syntax-rules ()
    ((>= x y) (fx>= x y))))

(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (fxnegative? x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (fxpositive? x))))

(define-syntax zero?
  (syntax-rules ()
    ((zero? x) (fxzero? x))))

