; Declarations to improve performance

(##declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
;  (not interrupts-enabled)
;  (fixnum)
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

'(begin
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

(begin
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
        (else              `(+ ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT- . lst)
  (cond ((null? lst)       (fatal-error "FLOAT- needs at least 1 argument"))
        ((null? (cdr lst)) `(- ,(car lst)))
        (else              `(- ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT* . lst)
  (cond ((null? lst)       `1.0)
        ((null? (cdr lst)) (car lst))
        (else              `(* ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT/ . lst)
  (cond ((null? lst)       (fatal-error "FLOAT/ needs at least 1 argument"))
        ((null? (cdr lst)) `(/ ,(car lst)))
        (else              `(/ ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT= x y)
  `(= ,x ,y))

(def-macro (FLOAT< x y)
  `(< ,x ,y))

(def-macro (FLOAT<= x y)
  `(<= ,x ,y))

(def-macro (FLOAT> x y)
  `(> ,x ,y))

(def-macro (FLOAT>= x y)
  `(>= ,x ,y))

(def-macro (FLOATnegative? x)
  `(negative? ,x))

(def-macro (FLOATpositive? x)
  `(positive? ,x))

(def-macro (FLOATzero? x)
  `(zero? ,x))

(def-macro (FLOATabs x)
  `(abs ,x))

(def-macro (FLOATsin x)
  `(sin ,x))

(def-macro (FLOATcos x)
  `(cos ,x))

(def-macro (FLOATatan x)
  `(atan ,x))

(def-macro (FLOATsqrt x)
  `(sqrt ,x))

(def-macro (FLOATmin x y)
  `(min ,x ,y))

(def-macro (FLOATmax x y)
  `(max ,x ,y))

(def-macro (FLOATround x)
  `(round ,x))

(def-macro (FLOATinexact->exact x)
  `(->fixnum ,x))


(def-macro (bitwise-or x y)
  `(##fixnum.logior ,x ,y))

(def-macro (bitwise-and x y)
  `(##fixnum.logand ,x ,y))

(def-macro (bitwise-not x)
  `(##fixnum.lognot ,x))
