;;; Compiler settings.  Change these as appropriate.

(fast-safe-code)                        ; Default setting
(issue-warnings #f)                     ; Or get very upset about nucleic.scm

;;;

(define fatal-error error)

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
    ((FLOAT+ x y ...) (+ x (FLOAT+ y ...)))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x)       (- x))
    ((FLOAT- x y ...) (- x (FLOAT+ y ...)))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT*)         1.0)
    ((FLOAT* x)       x)
    ((FLOAT* x y ...) (* x (FLOAT* y ...)))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x)       (/ x))
    ((FLOAT/ x y ...) (/ x (FLOAT* y ...)))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (> x 0.0))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (= x 0.0))))

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
; More fixnum macros can be found in prefix-chez.scm.

(define-syntax bitwise-or
  (syntax-rules ()
    ((bitwise-or x y) (logior x y))))

(define-syntax bitwise-and
  (syntax-rules ()
    ((bitwise-and x y) (logand x y))))

(define-syntax bitwise-not
  (syntax-rules ()
    ((bitwise-not x) (lognot x))))

; eof
