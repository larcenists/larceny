; This R6RS program uses the syntactic layer.
; It is the main example in R6RS library section 6.2.

(import (rnrs base)
        (rnrs io simple)
        (rnrs records inspection)
        (rnrs records syntactic))

(define-record-type (point make-point point?)
  (fields (immutable x point-x)
          (mutable y point-y set-point-y!))
  (nongenerative
    point-4893d957-e00b-11d9-817f-00111175eb9e))

(define-record-type (cpoint make-cpoint cpoint?)
  (parent point)
  (protocol
   (lambda (n)
     (lambda (x y c) 
       ((n x y) (color->rgb c)))))
  (fields
    (mutable rgb cpoint-rgb cpoint-rgb-set!)))

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

(define-record-type (ex1 make-ex1 ex1?)
  (protocol (lambda (p) (lambda a (p a))))
  (fields (immutable f ex1-f)))

(define ex1-i1 (make-ex1 1 2 3))
(show (ex1-f ex1-i1))

(define-record-type (ex2 make-ex2 ex2?)
  (protocol
    (lambda (p) (lambda (a . b) (p a b))))
  (fields (immutable a ex2-a)
          (immutable b ex2-b)))

(define ex2-i1 (make-ex2 1 2 3))
(show (ex2-a ex2-i1))
(show (ex2-b ex2-i1))

; Implicit naming and immutability.

(define-record-type (unit-vector
                     make-unit-vector
                     unit-vector?)
  (protocol
   (lambda (p)
     (lambda (x y z)
       (let ((length 
               (sqrt (+ (* x x)
                        (* y y)
                        (* z z)))))
         (p (/ x length)
            (/ y length)
            (/ z length))))))
  (fields (immutable x unit-vector-x)
          (immutable y unit-vector-y)
          (immutable z unit-vector-z)))

(define *ex3-instance* #f)

; Implicit naming and mutability.

(define-record-type ex3
  (parent cpoint)
  (protocol
   (lambda (n)
     (lambda (x y t)
       (let ((r ((n x y 'red) t)))
         (set! *ex3-instance* r)
         r))))
  (fields 
   (mutable thickness))
  (sealed #t) (opaque #t))

(define ex3-i1 (make-ex3 1 2 17))
(show (ex3? ex3-i1))
(show (cpoint-rgb ex3-i1))
(show (ex3-thickness ex3-i1))
(ex3-thickness-set! ex3-i1 18)
(show (ex3-thickness ex3-i1))

(show *ex3-instance*)

(show (record? ex3-i1))
