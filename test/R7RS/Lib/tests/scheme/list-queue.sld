;;; Copyright © John Cowan, 2014. All Rights Reserved.
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

;;; The copyright notice above is taken from srfi-117-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme list-queue) procedures:
;;;
;;;     make-list-queue
;;;     list-queue
;;;     list-queue-copy
;;;     list-queue-unfold
;;;     list-queue-unfold-right
;;;     list-queue?
;;;     list-queue-empty?
;;;     list-queue-front
;;;     list-queue-back
;;;     list-queue-list
;;;     list-queue-first-last
;;;     list-queue-add-front!
;;;     list-queue-add-back!
;;;     list-queue-remove-front!
;;;     list-queue-remove-back!
;;;     list-queue-remove-all!
;;;     list-queue-set-list!
;;;     list-queue-append
;;;     list-queue-append!
;;;     list-queue-concatenate
;;;     list-queue-map
;;;     list-queue-map!
;;;     list-queue-for-each

(define-library (tests scheme list-queue)
  (export run-list-queue-tests)
  (import (scheme base)
          (scheme list-queue)
          (tests scheme test))

  ;; Adapted from srfi-117-test.sps

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

   (define (run-list-queue-tests)

     ;; test-group "list-queues"

     ;; test-group "list-queues/simple"

     (let ()
       (define x (list-queue 1 2 3))
       (define x1 (list 1 2 3))
       (define x2 (make-list-queue x1 (cddr x1)))
       (define y (list-queue 4 5))
       (define z (list-queue-append x y))
       (define z2 (list-queue))
       (test (list-queue-list (make-list-queue '(1 1 1))) '(1 1 1))
       (test (list-queue-list x) '(1 2 3))
       (test (list-queue-back x2) 3)
       (test-assert (list-queue? y))
       (test (list-queue-list z) '(1 2 3 4 5))
       (set! z2 (list-queue-append! x (list-queue-copy y)))
       (test (list-queue-list z2) '(1 2 3 4 5))
       (test (list-queue-front z) 1)
       (test (list-queue-back z) 5)
       (list-queue-remove-front! y)
       (test (list-queue-list y) '(5))
       (list-queue-remove-back! y)
       (test-assert (list-queue-empty? y))
       (test-error (list-queue-remove-front! y))
       (test-error (list-queue-remove-back! y))
       (test (list-queue-list z) '(1 2 3 4 5))
       (test (list-queue-remove-all! z2) '(1 2 3 4 5))
       (test-assert (list-queue-empty? z2))
       (list-queue-remove-all! z)
       (list-queue-add-front! z 1)
       (list-queue-add-front! z 0)
       (list-queue-add-back! z 2)
       (list-queue-add-back! z 3)
       (test (list-queue-list z) '(0 1 2 3))
       )

     ;; test-group "list-queues/whole"

     (let ()
       (define a (list-queue 1 2 3))
       (define b (list-queue-copy a))
       (define c (list-queue))
       (test (list-queue-list b) '(1 2 3))
       (list-queue-add-front! b 0)
       (test (list-queue-list a) '(1 2 3))
       (test (length (list-queue-list b)) 4)
       (set! c (list-queue-concatenate (list a b)))
       (test (list-queue-list c) '(1 2 3 0 1 2 3))
       )

     ;; test-group "list-queues/map"

     (let ()
       (define r (list-queue 1 2 3))
       (define s (list-queue-map (lambda (x) (* x 10)) r))
       (define sum 0)
       (test (list-queue-list s) '(10 20 30))
       (list-queue-map! (lambda (x) (+ x 1)) r)
       (test (list-queue-list r) '(2 3 4))
       (list-queue-for-each (lambda (x) (set! sum (+ sum x))) s)
       (test sum 60)
       )

     ;; test-group "list-queues/conversion"

     (let ()
       (define n (list-queue 5 6))
       (define d (list 1 2 3))
       (define e (cddr d))
       (define f (make-list-queue d e))
       (define-values (dx ex) (list-queue-first-last f))
       (define g (list-queue))
       (define h (list-queue))
       (list-queue-set-list! n (list 1 2))
       (test (list-queue-list n) '(1 2))
       (test-assert (eq? d dx))
       (test-assert (eq? e ex))
       (test (list-queue-list f) '(1 2 3))
       (list-queue-add-front! f 0)
       (list-queue-add-back! f 4)
       (test (list-queue-list f) '(0 1 2 3 4))
       (set! g (make-list-queue d e))
       (test (list-queue-list g) '(1 2 3 4))
       (set! h (list-queue 5 6))
       (list-queue-set-list! h d e)
       (test (list-queue-list h) '(1 2 3 4))
       )

     ;; test-group "list-queues/unfold"

     (let ()
       (define (double x) (* x 2))
       (define (done? x) (> x 3))
       (define (add1 x) (+ x 1))
       (define x (list-queue-unfold done? double add1 0))
       (define y (list-queue-unfold-right done? double add1 0))
       (define x0 (list-queue 8))
       (define x1 (list-queue-unfold done? double add1 0 x0))
       (define y0 (list-queue 8))
       (define y1 (list-queue-unfold-right done? double add1 0 y0))
       (test (list-queue-list x) '(0 2 4 6))
       (test (list-queue-list y) '(6 4 2 0))
       (test (list-queue-list x1) '(0 2 4 6 8))
       (test (list-queue-list y1) '(8 6 4 2 0))
       )

     ;; Regression tests for Larceny (ticket #776).

     ;; test-group "list-queues/regression"

     (let ()
       (define x (list-queue 1 2 3))
       (define y (list-queue))
;      (define z (list-queue-append x y))
;      (define z2 (list-queue-append! x (list-queue-copy y)))
       (define z (list-queue-append y x))
       (define z2 (list-queue-append! y (list-queue-copy x)))
       (test (list-queue-list z) '(1 2 3))
       (test (list-queue-list z2) '(1 2 3))
       (test (list-queue-front z) 1)
       (test (list-queue-back z) 3)
       (test (list-queue-front z2) 1)
       (test (list-queue-back z2) 3)
       ))))
