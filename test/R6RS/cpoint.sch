; This R6RS program uses the syntactic layer of ERR5RS
; records.  It is a fairly literal translation of the
; examples in R6RS library section 6.2.

(import (rnrs base)
        (rnrs io simple)
        (err5rs records inspection)
        (err5rs records syntactic))

(define-record-type point make-point point?
  (x point-x)
  (y point-y set-point-y!))

(define-record-type (cpoint point) make-raw-cpoint cpoint?
  (rgb cpoint-rgb cpoint-rgb-set!))

(define (make-cpoint x y c)
  (make-raw-cpoint x y (color->rgb c)))

(define (color->rgb c)
  (cons 'rgb c))

(define p1 (make-point 1 2))
(define p2 (make-cpoint 3 4 'red))

(define (show x) (write x) (newline))

(for-each show
          (list (point? p1)
                (point? p2)
                (point? (vector))
                (point? (cons 'a 'b))
                (cpoint? p1)
                (cpoint? p2)
                (point-x p1)
                (point-y p1)
                (point-x p2)
                (point-y p2)
                (cpoint-rgb p2)))

(set-point-y! p1 17)
(show (point-y p1))

(show (record-rtd p1))

(define-record-type ex1 make-raw-ex1 ex1?
  (f ex1-f))

(define (make-ex1 . a)
  (make-raw-ex1 a))

(define ex1-i1 (make-ex1 1 2 3))
(show (ex1-f ex1-i1))

(define-record-type ex2 make-raw-ex2 ex2?
  (a ex2-a)
  (b ex2-b))

(define (make-ex2 a . b) (make-raw-ex2 a b))

(define ex2-i1 (make-ex2 1 2 3))
(show (ex2-a ex2-i1))
(show (ex2-b ex2-i1))

; Implicit naming and immutability.

(define-record-type unit-vector make-raw-unit-vector #t x y z)

(define (make-unit-vector x y z)
  (let ((length 
         (sqrt (+ (* x x)
                  (* y y)
                  (* z z)))))
    (make-raw-unit-vector (/ x length)
                          (/ y length)
                          (/ z length))))

(define *ex3-instance* #f)


; Implicit naming and mutability.

(define-record-type (ex3 cpoint) make-raw-ex3 #t (thickness))

(define (make-ex3 x y t)
  (let ((r (make-raw-ex3 x y 'red t)))
    (set! *ex3-instance* r)
    r))

(define ex3-i1 (make-ex3 1 2 17))
(show (ex3? ex3-i1))
(show (cpoint-rgb ex3-i1))
(show (ex3-thickness ex3-i1))
(ex3-thickness-set! ex3-i1 18)
(show (ex3-thickness ex3-i1))

(show *ex3-instance*)

; ERR5RS records do not support the sealed and opaque
; features of the R6RS.  Hence this next example
; evalutes to #t.
;
; Note that Larceny supports the sealed and opaque
; features in its procedural layer as an extension to
; ERR5RS, but does not support those features in its
; syntactic layer.

(show (record? ex3-i1))
