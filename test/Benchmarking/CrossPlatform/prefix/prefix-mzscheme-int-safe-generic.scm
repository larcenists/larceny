(require mzscheme)

(define (run-benchmark name count run ok?)
  (if (not (ok? (time (run-bench name count run ok?))))
    (begin
      (display "*** wrong result ***")
      (newline)))
  (exit 0))

(define (run-bench name count run ok?)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (fatal-error . args)
  (apply error #f args))

; Macros...

; Flonum arithmetic.

(define FLOATabs abs)
(define FLOATsin sin)
(define FLOATcos cos)
(define FLOATatan atan)
(define FLOATsqrt sqrt)
(define FLOATmin min)
(define FLOATmax max)
(define FLOATround round)
(define FLOATinexact->exact inexact->exact)

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


;; hack; work around openness in R5RS by deleting 
;; file before opening it.
(define call-with-output-file
  (let ((old-c/of call-with-output-file))
    (lambda (name fcn)
      (if (file-exists? name)
          (delete-file name))
      (old-c/of name fcn))))

