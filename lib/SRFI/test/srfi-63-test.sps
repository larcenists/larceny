; Test suite for SRFI 63
;
; $Id$

(import (except (rnrs base) equal?)
        (rnrs io simple)
        (rnrs control)
        (srfi :63 arrays))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (array? '#())
    (fail 'array?:0))

(or (array? '#(a b c 4.0))
    (fail 'array?:1))

(or (not (array? '()))
    (fail 'array?:3))

(or (not (array? '(a b c 4.0)))
    (fail 'array?:4))

(or (equal? 'a 'a)
    (fail 'equal?:2))

(or (equal? '(a) '(a))
    (fail 'equal?:3))

(or (equal? '(a (b) c)
            '(a (b) c))
    (fail 'equal?:4))

(or (equal? "abc" "abc")
    (fail 'equal?:5))

(or (equal? 2 2)
    (fail 'equal?:6))

(or (equal? (make-vector 5 'a)
            (make-vector 5 'a))
    (fail 'equal?:7))

(or (equal? (make-array (A:fixN32b 4) 5 3)
            (make-array (A:fixN32b 4) 5 3))
    (fail 'equal?:8))

(or (equal? (make-array '#(foo) 3 3)
            (make-array '#(foo) 3 3))
    (fail 'equal?:9))

(or (not (equal? '() 'b))
    (fail 'equal?:10))

(or (not (equal? 4 4.0))
    (fail 'equal?:11))

(or (not (equal? 'a 'b))
    (fail 'equal?:12))

(or (not (equal? '() '(a)))
    (fail 'equal?:13))

(or (not (equal? '(a (d) c)
                 '(a (b) c)))
    (fail 'equal?:14))

(or (not (equal? "abc" "abcd"))
    (fail 'equal?:15))

(or (not (equal? 2 2.0))
    (fail 'equal?:16))

(or (not (equal? (make-vector 5 'a)
                 (make-vector 6 'a)))
    (fail 'equal?:17))

(or (not (equal? (make-array (A:fixN32b 4) 5 3)
                 (make-array (A:fixN32b 2) 5 3)))
    (fail 'equal?:18))

(or (not (equal? (make-array '#(foo) 3 3)
                 (make-array '#(baz) 3 3)))
    (fail 'equal?:19))

(or (equal? 0 (array-rank 4.5))
    (fail 'array-rank:0))

(or (equal? 2 (array-rank (make-array '#(foo) 3 3)))
    (fail 'array-rank:1))

(or (equal? '(3 7) (array-dimensions (make-array '#(foo) 3 7)))
    (fail 'array-dimensions:1))

(let ()
  (define fred (make-array '#(#f) 8 8))
  (define freds-diagonal
    (make-shared-array fred (lambda (i) (list i i)) 8))
  (define ignored1 (array-set! freds-diagonal 'foo 3))
  (define ignored2 (or (equal? 'foo (array-ref fred 3 3))
                       (fail 'make-shared-array:0)))
  (define freds-center
    (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j)))
                       2 2))
  (or (equal? 'foo (array-ref freds-center 0 0))
      (fail 'make-shared-array:1)))

(or (equal? (array->list (list->array 2 '#() '((1 2) (3 4))))
            '((1 2) (3 4)))
    (fail 'list->array:1))

(or (equal? (array->list (list->array 0 '#() 3))
            3)
    (fail 'list->array:2))

(or (equal? (array->list (vector->array '#(1 2 3 4) '#() 2 2))
            '((1 2) (3 4)))
    (fail 'vector->array))

(or (equal? (array->vector (vector->array '#(1 2 3 4) '#() 2 2))
            '#(1 2 3 4))
    (fail 'array->vector))

(or (equal? #f (array-in-bounds? (make-array '#(#f) 0 0) 0))
    (fail 'array-in-bounds?:0))

(or (equal? #f (array-in-bounds? (make-array '#(#f) 1 0) 0 0))
    (fail 'array-in-bounds?:1))

(or (equal? #f (array-in-bounds? (make-array '#(#f) 0 1) 0 0))
    (fail 'array-in-bounds?:2))

(or (equal? #t (array-in-bounds? (make-array '#(#f) 4 5) 3 4))
    (fail 'array-in-bounds?:3))

(or (equal? #f (array-in-bounds? (make-array '#(#f) 4 5) 3 5))
    (fail 'array-in-bounds?:3))

(or (equal? #f (array-in-bounds? (make-array '#(#f) 4 5) 4 4))
    (fail 'array-in-bounds?:3))

(let ((m 4)
      (n 5)
      (o 6))
  (define (f i j k)
    (* (+ i 1) (+ j j 3) (+ k k k 5)))
  (define a (make-array '#() m n o))
  (do ((i 0 (+ i 1)))
      ((= i m))
    (do ((j 0 (+ j 1)))
        ((= j n))
      (do ((k 0 (+ k 1)))
          ((= k o))
        (array-set! a (f i j k) i j k))))
  (do ((k 0 (+ k 1)))
      ((= k o))
    (do ((j 0 (+ j 1)))
        ((= j n))
      (do ((i 0 (+ i 1)))
          ((= i m))
        (or (= (f i j k)
               (array-ref a i j k))
            (fail 'array-ref))))))








(let ((prototypes
       (list
        (A:floC128b 3+4i)
        (A:floC128b)
        (A:floC64b 3.14159+0i)
        (A:floC64b)
        (A:floC32b 3.0)
        (A:floC32b)
        (A:floC16b 3)
        (A:floC16b)
        (A:floR128b 45.0)
        (A:floR128b)
        (A:floR64b 45)
        (A:floR64b)
        (A:floR32b 0)
        (A:floR32b)
        (A:floR16b 255)
        (A:floR16b)
        (A:floR128d (expt 2.0 126))
        (A:floR128d)
        (A:floR64d 4.5)
        (A:floR64d)
        (A:floR32d 4)
        (A:floR32d)
        (A:fixZ64b (expt 2 60))
        (A:fixZ64b)
        (A:fixZ32b (expt 2 30))
        (A:fixZ32b)
        (A:fixZ16b 60000)
        (A:fixZ16b)
        (A:fixZ8b 255)
        (A:fixZ8b)
        (A:fixN64b (expt 2 60))
        (A:fixN64b)
        (A:fixN32b (expt 2 30))
        (A:fixN32b)
        (A:fixN16b 60000)
        (A:fixN16b)
        (A:fixN8b 255)
        (A:fixN8b)
        (A:bool #t)
        (A:bool))))
  (for-each (lambda (a)
              (or (array? a)
                   (fail 'prototype)))
            prototypes))

(writeln "Done.")
