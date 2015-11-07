;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This benchmark tests the R6RS equal? predicate on some fairly
; large structures of various shapes.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

; Returns a list with n elements, all equal to x.

(define (make-test-list1 n x)
  (if (zero? n)
      '()
      (cons x (make-test-list1 (- n 1) x))))

; Returns a list of n lists, each consisting of n x's.
; The n elements of the outer list are actually the same list.

(define (make-test-tree1 n)
  (if (zero? n)
      '()
      (make-test-list1 n (make-test-tree1 (- n 1)))))

; Returns a list of n elements, as returned by the thunk.

(define (make-test-list2 n thunk)
  (if (zero? n)
      '()
      (cons (thunk) (make-test-list2 (- n 1) thunk))))

; Returns a balanced tree of height n, with the branching factor
; at each level equal to the height of the tree at that level.
; The subtrees do not share structure.

(define (make-test-tree2 n)
  (if (zero? n)
      '()
      (make-test-list2 n (lambda () (make-test-tree2 (- n 1))))))

; Returns an extremely unbalanced tree of height n.

(define (make-test-tree5 n)
  (if (zero? n)
      '()
      (cons (make-test-tree5 (- n 1))
            'a)))

; Calls the thunk n times.

(define (iterate n thunk)
  (cond ((= n 1)
         (thunk))
        ((> n 1)
         (thunk)
         (iterate (- n 1) thunk))
        (else #f)))      

; A simple circular list is a worst case for R5RS equal?.

(define (equality-benchmark0 n)
  (let ((x (vector->list (make-vector n 'a))))
    (set-cdr! (list-tail x (- n 1)) x)
    (iterate n (hide n (lambda () (equal? x (cdr x)))))))

; DAG with much sharing.
; 10 is a good parameter for n.

(define (equality-benchmark1 n)
  (let ((x (make-test-tree1 n))
        (y (make-test-tree1 n)))
    (iterate n (hide n (lambda () (equal? x y))))))

; Tree with no sharing.
; 8 is a good parameter for n.

(define (equality-benchmark2 n)
  (let ((x (make-test-tree2 n))
        (y (make-test-tree2 n)))
    (iterate n (hide n (lambda () (equal? x y))))))

; Flat vectors.
; 1000 might be a good parameter for n.

(define (equality-benchmark3 n)
  (let* ((x (make-vector n 'a))
         (y (make-vector n 'a)))
    (iterate n (hide n (lambda () (equal? x y))))))

; Shallow lists.
; 300 might be a good parameter for n.

(define (equality-benchmark4 n)
  (let* ((x (vector->list (make-vector n (make-test-tree2 3))))
         (y (vector->list (make-vector n (make-test-tree2 3)))))
    (iterate n (hide n (lambda () (equal? x y))))))

; No sharing, no proper lists,
; and deep following car chains instead of cdr.

(define (equality-benchmark5 n . rest)
  (let* ((x (make-test-tree5 n))
         (y (make-test-tree5 n))
         (iterations (if (null? rest) n (car rest))))
    (iterate iterations (hide n (lambda () (equal? x y))))))

; A shorter form of the benchmark above.

(define (equality-benchmark5short n)
  (equality-benchmark5 n 100))

(define (equality-benchmarks n0 n1 n2 n3 n4 n5)
  (and (equality-benchmark0 n0)
       (equality-benchmark1 n1)
       (equality-benchmark2 n2)
       (equality-benchmark3 n3)
       (equality-benchmark4 n4)
       (equality-benchmark5 n5)))

(define (main)
  (let* ((input0 (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (input4 (read))
         (input5 (read))
         (output (read))
         (s5 (number->string input5))
         (s4 (number->string input4))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (s0 (number->string input0))
         (name "equal"))
    (run-r7rs-benchmark
     (string-append name ":" s0 ":" s1 ":" s2 ":" s3 ":" s4 ":" s5)
     1
     (lambda ()
       (equality-benchmarks (hide input0 input0)
                            (hide input0 input1)
                            (hide input0 input2)
                            (hide input0 input3)
                            (hide input0 input4)
                            (hide input0 input5)))
     (lambda (result) (eq? result #t)))))
