(unsafe-code #t)
(catch-undefined-globals #f)

; Macros...
; When hygienic macros are supported, get these out of src/prefix-chez.scm.

; Flonum arithmetic.

(define-macro 'FLOATvector-const
  (lambda (x)
    (list 'quote (list->vector (cdr x)))))
;   ((FLOATvector-const x ...) '#(x ...))))

(define-macro 'FLOATvector?
  (lambda (x)
    (cons 'vector? (cdr x))))
;   ((FLOATvector? x) (vector? x))))

(define-macro 'FLOATvector
  (lambda (x)
    (cons 'vector (cdr x))))
;   ((FLOATvector x ...) (vector x ...))))

(define-macro 'FLOATmake-vector
  (lambda (x)
    (if (= (length x) 2)
        `(make-vector ,(cadr x) 0.0)
        (cons 'make-vector (cdr x)))))
;   ((FLOATmake-vector n) (make-vector n 0.0))
;   ((FLOATmake-vector n init) (make-vector n init))))

(define-macro 'FLOATvector-ref
  (lambda (x)
    (cons 'vector-ref (cdr x))))
;   ((FLOATvector-ref v i) (vector-ref v i))))

(define-macro 'FLOATvector-set!
  (lambda (x)
    (cons 'vector-set! (cdr x))))
;   ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-macro 'FLOATvector-length
  (lambda (x)
    (cons 'vector-length (cdr x))))
;   ((FLOATvector-length v) (vector-length v))))

(define-macro 'nuc-const
  (lambda (x)
    (list 'quote (list->vector (cdr x)))))
;   ((FLOATnuc-const x ...) '#(x ...))))

(define-macro 'FLOAT+
  (lambda (x)
    (cons '+ (cdr x))))
;   ((FLOAT+)         0.0)
;   ((FLOAT+ x)       x)
;   ((FLOAT+ x y ...) (fl+ x (FLOAT+ y ...)))))

(define-macro 'FLOAT-
  (lambda (x)
    (cons '- (cdr x))))
;   ((FLOAT- x)       (fl- x))
;   ((FLOAT- x y ...) (fl- x (FLOAT+ y ...)))))

(define-macro 'FLOAT*
  (lambda (x)
    (cons '* (cdr x))))
;   ((FLOAT*)         1.0)
;   ((FLOAT* x)       x)
;   ((FLOAT* x y ...) (fl* x (FLOAT* y ...)))))

(define-macro 'FLOAT/
  (lambda (x)
    (cons '/ (cdr x))))
;   ((FLOAT/ x)       (fl/ x))
;   ((FLOAT/ x y ...) (fl/ x (FLOAT* y ...)))))

(define-macro 'FLOAT=
  (lambda (x)
    (cons '= (cdr x))))
;   ((FLOAT= x y) (fl= x y))))

(define-macro 'FLOAT<
  (lambda (x)
    (cons '< (cdr x))))
;   ((FLOAT< x y) (fl< x y))))

(define-macro 'FLOAT<=
  (lambda (x)
    (cons '<= (cdr x))))
;   ((FLOAT<= x y) (fl<= x y))))

(define-macro 'FLOAT>
  (lambda (x)
    (cons '> (cdr x))))
;   ((FLOAT> x y) (fl> x y))))

(define-macro 'FLOAT>=
  (lambda (x)
    (cons '>= (cdr x))))
;   ((FLOAT>= x y) (fl>= x y))))

(define-macro 'FLOATnegative?
  (lambda (x)
    `(< ,(cadr x) 0.0)))
;   ((FLOATnegative? x) (fl< x 0.0))))

(define-macro 'FLOATpositive?
  (lambda (x)
    `(> ,(cadr x) 0.0)))
;   ((FLOATpositive? x) (fl> x 0.0))))

(define-macro 'FLOATzero?
  (lambda (x)
    `(= ,(cadr x) 0.0)))
;   ((FLOATzero? x) (fl= x 0.0))))

