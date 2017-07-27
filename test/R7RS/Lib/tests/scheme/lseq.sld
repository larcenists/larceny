;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; The copyright notice above is taken from srfi-127-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme lseq) procedures:
;;;
;;;     generator->lseq
;;;     lseq?
;;;     lseq=?
;;;     lseq-car
;;;     lseq-first
;;;     lseq-cdr
;;;     lseq-rest
;;;     lseq-ref
;;;     lseq-take
;;;     lseq-drop
;;;     lseq-realize
;;;     lseq->generator
;;;     lseq-length
;;;     lseq-append
;;;     lseq-zip
;;;     lseq-map
;;;     lseq-for-each
;;;     lseq-filter
;;;     lseq-remove
;;;     lseq-find
;;;     lseq-find-tail
;;;     lseq-take-while
;;;     lseq-drop-while
;;;     lseq-any
;;;     lseq-every
;;;     lseq-index
;;;     lseq-member
;;;     lseq-memq
;;;     lseq-memv


(define-library (tests scheme lseq)
  (export run-lseq-tests)
  (import (scheme base)
          (scheme lseq)
          (tests scheme test))

  ;; Adapted from srfi-127-test.sps7

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define (run-lseq-tests)

     ;; Make-generator for tests cloned from SRFI 121

     (define (make-generator . args)
       (lambda () (if (null? args)
                      (eof-object)
                      (let ((next (car args)))
                        (set! args (cdr args))
                        next))))

     ;; Make-lseq creates an lseq, like list,
     ;; but guarantees the use of a generator

     (define (make-lseq . args)
       (generator->lseq (apply make-generator args)))

     ;; test-group "lseqs"

     ;; test-group "lseqs/constructor"

     (let ()
       (define one23 (make-lseq 1 2 3))
       (test (car one23) 1)
       (test-assert (procedure? (cdr one23)))
       (test (lseq-realize one23) '(1 2 3)))

     ;; test-group "lseqs/predicates"

     (test-assert (lseq? '()))
     (test-assert (lseq? '(1 2 3)))
     (test-assert (lseq? (make-lseq 1 2 3)))
     (test-assert (lseq? (cons 'x (lambda () 'x))))

     (test-assert (lseq=? = '() '()))
     (test-assert (lseq=? = '(1 2 3) '(1 2 3)))
     (test-assert (lseq=? = (make-lseq 1 2 3)
                          (make-lseq 1 2 3)))
     (test-assert (lseq=? = (make-lseq 1 2 3) '(1 2 3)))

     ;; test-group "lseqs/selectors"

     (test-error (lseq-car (make-generator)))
     (test (lseq-car (make-lseq 1 2 3)) 1)
     (test (lseq-car '(1 2 3)) 1)
     (test-error (lseq-car 2))

     (test-error (lseq-first (make-generator)))
     (test (lseq-first (make-lseq 1 2 3)) 1)
     (test (lseq-first '(1 2 3)) 1)
     (test-error (lseq-first 2))

     (test-error (lseq-cdr (make-generator)))
     (test (lseq-cdr '(1 . 2)) 2)
     (test (lseq-car (lseq-cdr '(1 2 3))) 2)
     (test (lseq-car (lseq-cdr (make-lseq 1 2 3))) 2)

     (test-error (lseq-rest (make-generator)))
     (test (lseq-rest '(1 . 2)) 2)
     (test (lseq-car (lseq-rest '(1 2 3))) 2)
     (test (lseq-car (lseq-rest (make-lseq 1 2 3))) 2)
     (test-error (lseq-rest 2))

     (test-error (lseq-ref '() 0))
     (test (lseq-ref '(1) 0) 1)
     (test (lseq-ref '(1 2) 1) 2)
     (test-error (lseq-ref (make-lseq) 0))
     (test (lseq-ref (make-lseq 1) 0) 1)
     (test (lseq-ref (make-lseq 1 2) 0) 1)
     (test (lseq-ref (make-lseq 1 2) 1) 2)

     (test-error (lseq-take '() 1))
     (test-error (lseq-take (make-lseq) 1))
     (test-assert (procedure? (cdr (lseq-take '(1 2 3 4 5) 3)))) ; laziness
     (test (lseq-realize (lseq-take '(1 2 3 4 5) 3)) '(1 2 3))

     (test-error (lseq-drop '() 1))
     (test-error (lseq-drop (make-lseq 1) 2))
     (test (lseq-realize (lseq-drop '(1 2 3 4 5) 2)) '(3 4 5))
     (test (lseq-realize (lseq-drop (make-lseq 1 2 3 4 5) 2)) '(3 4 5))

     ;; test-group "lseqs/whole"

     (test (lseq-realize '()) '())
     (test (lseq-realize '(1 2 3)) '(1 2 3))
     (test (lseq-realize (make-lseq)) '())
     (test (lseq-realize (make-lseq 1 2 3)) '(1 2 3))

     (let ((g (lseq->generator '(1 2 3))))
       (test (g) 1)
       (test (g) 2)
       (test (g) 3)
       (test-assert (eof-object? (g))))
     (let ((g (lseq->generator (make-lseq 1 2 3))))
       (test (g) 1)
       (test (g) 2)
       (test (g) 3)
       (test-assert (eof-object? (g))))

     (test (lseq-length '()) 0)
     (test (lseq-length '(1 2 3)) 3)
     (test (lseq-length (make-lseq 1 2 3)) 3)

     (test (lseq-realize (lseq-append '(1 2 3) '(a b c))) '(1 2 3 a b c))
     (let ((one23abc (lseq-append (make-lseq 1 2 3) (make-lseq 'a 'b 'c))))
       (test-assert (procedure? (cdr one23abc)))
       (test-assert (and (lseq-realize one23abc) #t)))

     (let ()
       (define one2345 (make-lseq 1 2 3 4 5))
       (define oddeven
         (make-lseq 'odd 'even 'odd 'even 'odd 'even 'odd 'even))
       (test (lseq-realize (lseq-zip '(one two three) one2345 oddeven))
             '((one 1 odd) (two 2 even) (three 3 odd))))

     ;; test-group "lseqs/mapping"

     (test (lseq-map - '()) '())
     (test (lseq-realize (lseq-map - '(1 2 3))) '(-1 -2 -3))
     (test (lseq-realize (lseq-map - (make-lseq 1 2 3))) '(-1 -2 -3))
     (test-assert (procedure? (cdr (lseq-map - '(1 2 3)))))

     (let ()
       (define output '())
       (define out! (lambda (x) (set! output (cons x output))))
       (lseq-for-each out! '())
       (test output '())
       (lseq-for-each out! '(a b c))
       (test output '(c b a))
       (lseq-for-each out! (make-lseq 1 2 3))
       (test output '(3 2 1 c b a)))

     (test '() (lseq-filter odd? '()))
     (let ()
       (define odds (lseq-filter odd? '(1 2 3 4 5)))
       (test-assert (procedure? (cdr odds)))
       (test (lseq-realize odds) '(1 3 5))
       (test (lseq-realize (lseq-filter odd? (make-lseq 1 2 3 4 5)))
             '(1 3 5)))

     (test (lseq-remove even? '()) '())
     (let ()
       (define odds (lseq-remove even? '(1 2 3 4 5)))
       (test-assert (procedure? (cdr odds)))
       (test (lseq-realize odds) '(1 3 5))
       (test (lseq-realize (lseq-remove even? (make-lseq 1 2 3 4 5)))
             '(1 3 5)))

     ;; test-group "lseqs/searching"

     (test (lseq-find even? '(3 1 4 1 5 9 2 6)) 4)
     (test (lseq-find even? (make-lseq 3 1 4 1 5 9 2 6)) 4)
     (test (lseq-find negative? (make-lseq 1 2 3 4 5)) #f)

     (test (lseq-realize (lseq-find-tail even? '(3 1 37 -8 -5 0 0)))
           '(-8 -5 0 0))
     (test (lseq-realize (lseq-find-tail even?
                                         (make-lseq 3 1 37 -8 -5 0 0)))
           '(-8 -5 0 0))
     (test (lseq-find-tail even? '()) #f)
     (test (lseq-find-tail negative? (make-lseq 1 2 3 4 5)) #f)

     (test (lseq-realize (lseq-take-while even? '(2 18 3 10 22 9))) '(2 18))
     (test (lseq-realize (lseq-take-while even?
                                          (make-lseq 2 18 3 10 22 9)))
           '(2 18))
     (test (lseq-realize (lseq-take-while even?
                                          (make-lseq 2 18 3 10 22 9)))
           '(2 18))

     (test (lseq-drop-while even? '(2 18 3 10 22 9)) '(3 10 22 9))
     (test (lseq-realize (lseq-drop-while even?
                                          (make-lseq 2 18 3 10 22 9)))
           '(3 10 22 9))

     (test (lseq-any integer? '(a 3 b 2.7)) #t)
     (test (lseq-any integer? (make-lseq 'a 3 'b 2.7)) #t)
     (test (lseq-any integer? '(a 3.1 b 2.7)) #f)
     (test (lseq-any integer? (make-lseq 'a 3.1 'b 2.7)) #f)
     (test (lseq-any < '(3 1 4 1 5) '(2 7 1 8 2)) #t)

     (let ()
       (define (factorial n)
         (cond
          ((< n 0) #f)
          ((= n 0) 1)
          (else (* n (factorial (- n 1))))))
       (test (lseq-any factorial '(-1 -2 3 4)) 6)
       (test (lseq-any factorial (make-lseq -1 -2 3 4)) 6)

       (test (lseq-every factorial '(1 2 3 4)) 24)
       (test (lseq-every factorial (make-lseq 1 2 3 4)) 24))

     (test (lseq-index even? '(3 1 4 1 5 9)) 2)
     (test (lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) 1)
     (test (lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) #f)

     (test (lseq-realize (lseq-memq 'a '(a b c))) '(a b c))
     (test (lseq-realize (lseq-memq 'a (make-lseq 'a 'b 'c))) '(a b c))
     (test (lseq-memq 'a (make-lseq 'b 'c 'd)) #f)
     (test (lseq-memq (list 'a) '(b c d)) #f)
     (test (lseq-memq (list 'a) (make-lseq 'b 'c 'd)) #f)

     (test (lseq-realize (lseq-memv 101 (make-lseq 100 101 102)))
           '(101 102))

     (test (lseq-realize (lseq-member (list 'a) (make-lseq 'b '(a) 'c)))
           '((a) c))
     (test (lseq-realize (lseq-member 2.0 (make-lseq 1 2 3) =))
           '(2 3)))))
