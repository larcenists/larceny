;INSERTCODE
;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline))))
  (exit 0))

(define (fatal-error . args)
  (apply error #f args))

 (define (call-with-output-file/truncate filename proc)
   (call-with-output-file filename proc 'truncate))

; Bitwise operations on exact integers.
; From the draft reference implementation of R6RS generic arithmetic.

(define (bitwise-or i j)
  (if (and (exact? i)
           (integer? i)
           (exact? j)
           (integer? j))
      (cond ((or (= i -1) (= j -1))
             -1)
            ((= i 0)
             j)
            ((= j 0)
             i)
            (else
             (let* ((i0 (if (odd? i) 1 0))
                    (j0 (if (odd? j) 1 0))
                    (i1 (- i i0))
                    (j1 (- j j0))
                    (i/2 (quotient i1 2))
                    (j/2 (quotient j1 2))
                    (hi (* 2 (bitwise-or i/2 j/2)))
                    (lo (if (= 0 (+ i0 j0)) 0 1)))
               (+ hi lo))))
      (error "illegal argument to bitwise-or" i j)))

(define (bitwise-and i j)
  (if (and (exact? i)
           (integer? i)
           (exact? j)
           (integer? j))
      (cond ((or (= i 0) (= j 0))
             0)
            ((= i -1)
             j)
            ((= j -1)
             i)
            (else
             (let* ((i0 (if (odd? i) 1 0))
                    (j0 (if (odd? j) 1 0))
                    (i1 (- i i0))
                    (j1 (- j j0))
                    (i/2 (quotient i1 2))
                    (j/2 (quotient j1 2))
                    (hi (* 2 (bitwise-and i/2 j/2)))
                    (lo (* i0 j0)))
               (+ hi lo))))
      (error "illegal argument to bitwise-and" i j)))

(define (bitwise-not i)
  (if (and (exact? i)
           (integer? i))
      (cond ((= i -1)
             0)
            ((= i 0)
             -1)
            (else
             (let* ((i0 (if (odd? i) 1 0))
                    (i1 (- i i0))
                    (i/2 (quotient i1 2))
                    (hi (* 2 (bitwise-not i/2)))
                    (lo (- 1 i0)))
               (+ hi lo))))
      (error "illegal argument to bitwise-not" i j)))

;------------------------------------------------------------------------------

; Macros...

(if-fixflo

(begin

; Specialize fixnum and flonum arithmetic.

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
    ((FLOAT+ x ...) (fl+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (fl- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (fl* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (fl/ x ...))))

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
    ((FLOATpositive? x) (fl< 0.0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (fl= 0.0 x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (flabs x))))

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

(define (GENERIC+ x y) (+ x y))
(define (GENERIC- x y) (- x y))
(define (GENERIC* x y) (* x y))
(define (GENERIC/ x y) (/ x y))
(define (GENERICquotient x y) (quotient x y))
(define (GENERICremainder x y) (remainder x y))
(define (GENERICmodulo x y) (modulo x y))
(define (GENERIC= x y) (= x y))
(define (GENERIC< x y) (< x y))
(define (GENERIC<= x y) (<= x y))
(define (GENERIC> x y) (> x y))
(define (GENERIC>= x y) (>= x y))
(define (GENERICexpt x y) (expt x y))

(define-syntax +
  (syntax-rules ()
    ((+ x ...) (fx+ x ...))))

(define-syntax -
  (syntax-rules ()
    ((- x ...) (fx- x ...))))

(define-syntax *
  (syntax-rules ()
    ((* x ...) (fx* x ...))))

(define-syntax quotient
  (syntax-rules ()
    ((quotient x ...) (fxquotient x ...))))

(define-syntax modulo
  (syntax-rules ()
    ((modulo x ...) (fxmodulo x ...))))

(define-syntax remainder
  (syntax-rules ()
    ((remainder x ...) (fxremainder x ...))))

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

(define-syntax odd?
  (syntax-rules ()
    ((odd? x) (fxodd? x))))

(define-syntax even?
  (syntax-rules ()
    ((even? x) (fxeven? x))))

; FIXME

;(define-syntax bitwise-or
;  (syntax-rules ()
;    ((bitwise-or x y) (fxior x y))))

;(define-syntax bitwise-and
;  (syntax-rules ()
;    ((bitwise-and x y) (fxand x y))))

;(define-syntax bitwise-not
;  (syntax-rules ()
;    ((bitwise-not x) (fxnot x))))
)

(begin

; Don't specialize fixnum and flonum arithmetic.

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
    ((FLOAT+ x ...) (+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (/ x ...))))

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
    ((FLOATnegative? x) (negative? x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (positive? x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (zero? x))))

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

; Generic arithmetic.

(define-syntax GENERIC+
  (syntax-rules ()
    ((GENERIC+ x ...) (+ x ...))))

(define-syntax GENERIC-
  (syntax-rules ()
    ((GENERIC- x ...) (- x ...))))

(define-syntax GENERIC*
  (syntax-rules ()
    ((GENERIC* x ...) (* x ...))))

(define-syntax GENERIC/
  (syntax-rules ()
    ((GENERIC/ x ...) (/ x ...))))

(define-syntax GENERICquotient
  (syntax-rules ()
    ((GENERICquotient x y) (quotient x y))))

(define-syntax GENERICremainder
  (syntax-rules ()
    ((GENERICremainder x y) (remainder x y))))

(define-syntax GENERICmodulo
  (syntax-rules ()
    ((GENERICmodulo x y) (modulo x y))))

(define-syntax GENERIC=
  (syntax-rules ()
    ((GENERIC= x y) (= x y))))

(define-syntax GENERIC<
  (syntax-rules ()
    ((GENERIC< x y) (< x y))))

(define-syntax GENERIC<=
  (syntax-rules ()
    ((GENERIC<= x y) (<= x y))))

(define-syntax GENERIC>
  (syntax-rules ()
    ((GENERIC> x y) (> x y))))

(define-syntax GENERIC>=
  (syntax-rules ()
    ((GENERIC>= x y) (>= x y))))

(define-syntax GENERICexpt
  (syntax-rules ()
    ((GENERICexpt x y) (expt x y))))
)
)

;------------------------------------------------------------------------------
