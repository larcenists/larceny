; Declarations to improve performance

(##declare
  (standard-bindings)
  (extended-bindings)
  (block)
;  (not safe)
;  (not interrupts-enabled)
  (fixnum)
  (inlining-limit 500)
)

;(set-gc-report 1)

(define (run-benchmark name count run ok?)
  (if (not (ok? (time (run-bench name count run ok?))))
    (begin
      (display "*** wrong result ***")
      (newline))))

(define (run-bench name count run ok?)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (fatal-error . args)
  (for-each display args)
  (newline)
  (exit 1))

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(begin
(def-macro (FLOATvector-const . lst)   `',(##list->f64vector lst))
(def-macro (FLOATvector? x)            `(##f64vector? ,x))
(def-macro (FLOATvector . lst)         `(##f64vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(##make-f64vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(##f64vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(##f64vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(##f64vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector
       (map (lambda (x)
              (if (vector? x)
                (##list->f64vector (vector->list x))
                x))
            lst)))
)

'(begin
(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(vector? ,x))
(def-macro (FLOATvector . lst)         `(vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector lst))
)

(def-macro (FLOAT+ . lst)
  (cond ((null? lst)       `0.0)
        ((null? (cdr lst)) (car lst))
        (else              `(##flonum.+ ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT- . lst)
  (cond ((null? lst)       (fatal-error "FLOAT- needs at least 1 argument"))
        ((null? (cdr lst)) `(##flonum.- ,(car lst)))
        (else              `(##flonum.- ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT* . lst)
  (cond ((null? lst)       `1.0)
        ((null? (cdr lst)) (car lst))
        (else              `(##flonum.* ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT/ . lst)
  (cond ((null? lst)       (fatal-error "FLOAT/ needs at least 1 argument"))
        ((null? (cdr lst)) `(##flonum./ ,(car lst)))
        (else              `(##flonum./ ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT= x y)
  `(##flonum.= ,x ,y))

(def-macro (FLOAT< x y)
  `(##flonum.< ,x ,y))

(def-macro (FLOAT<= x y)
  `(##flonum.<= ,x ,y))

(def-macro (FLOAT> x y)
  `(##flonum.> ,x ,y))

(def-macro (FLOAT>= x y)
  `(##flonum.>= ,x ,y))

(def-macro (FLOATnegative? x)
  `(##flonum.negative? ,x))

(def-macro (FLOATpositive? x)
  `(##flonum.positive? ,x))

(def-macro (FLOATzero? x)
  `(##flonum.zero? ,x))

(def-macro (FLOATabs x)
  `(##flonum.abs ,x))

(def-macro (FLOATsin x)
  `(##flonum.sin ,x))

(def-macro (FLOATcos x)
  `(##flonum.cos ,x))

(def-macro (FLOATatan x)
  `(##flonum.atan ,x))

(def-macro (FLOATsqrt x)
  `(##flonum.sqrt ,x))

(def-macro (FLOATmin x y)
  `(##flonum.min ,x ,y))

(def-macro (FLOATmax x y)
  `(##flonum.max ,x ,y))

(def-macro (FLOATround x)
  `(##flonum.round ,x))

(def-macro (FLOATinexact->exact x)
  `(##flonum.->fixnum ,x))

(def-macro (+ . lst)
  (cond ((null? lst)       `0)
        ((null? (cdr lst)) (car lst))
        (else              `(##fixnum.+ ,(car lst) (+ ,@(cdr lst))))))

(def-macro (- . lst)
  (cond ((null? lst)       (fatal-error "- needs at least 1 argument"))
        ((null? (cdr lst)) `(##fixnum.- ,(car lst)))
        (else              `(##fixnum.- ,(car lst) (+ ,@(cdr lst))))))

(def-macro (* . lst)
  (cond ((null? lst)       `1)
        ((null? (cdr lst)) (car lst))
        (else              `(##fixnum.* ,(car lst) (* ,@(cdr lst))))))

(def-macro (quotient x y)
  `(##fixnum.quotient ,x ,y))

(def-macro (remainder x y)
  `(##fixnum.remainder ,x ,y))

(def-macro (modulo x y)
  `(##fixnum.modulo ,x ,y))

(def-macro (= x y)
  `(##fixnum.= ,x ,y))

(def-macro (< x y)
  `(##fixnum.< ,x ,y))

(def-macro (<= x y)
  `(##fixnum.<= ,x ,y))

(def-macro (> x y)
  `(##fixnum.> ,x ,y))

(def-macro (>= x y)
  `(##fixnum.>= ,x ,y))

(def-macro (negative? x)
  `(##fixnum.negative? ,x))

(def-macro (positive? x)
  `(##fixnum.positive? ,x))

(def-macro (zero? x)
  `(##fixnum.zero? ,x))

(def-macro (odd? x)
  `(##fixnum.odd? ,x))

(def-macro (even? x)
  `(##fixnum.even? ,x))

(def-macro (bitwise-or x y)
  `(##fixnum.logior ,x ,y))

(def-macro (bitwise-and x y)
  `(##fixnum.logand ,x ,y))

(def-macro (bitwise-not x)
  `(##fixnum.lognot ,x))
