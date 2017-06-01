;;; Copyright (C) Shiro Kawai, John Cowan, Thomas Gilray (2015).
;;; All Rights Reserved.
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

;;; The copyright notice above is taken from srfi-121-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme generator) procedures:
;;;
;;;     generator
;;;     make-iota-generator
;;;     make-range-generator 
;;;     make-coroutine-generator
;;;     list->generator
;;;     vector->generator
;;;     reverse-vector->generator
;;;     string->generator
;;;     bytevector->generator
;;;     make-for-each-generator
;;;     make-unfold-generator
;;;     gcons*
;;;     gappend
;;;     gcombine
;;;     gfilter
;;;     gremove 
;;;     gtake
;;;     gdrop
;;;     gtake-while
;;;     gdrop-while
;;;     gdelete
;;;     gdelete-neighbor-dups
;;;     gindex
;;;     gselect
;;;     generator->list
;;;     generator->reverse-list
;;;     generator->vector
;;;     generator->vector!
;;;     generator->string
;;;     generator-fold
;;;     generator-for-each
;;;     generator-find
;;;     generator-count
;;;     generator-any
;;;     generator-every
;;;     generator-unfold

(define-library (tests scheme generator)
  (export run-generator-tests)
  (import (scheme base)
          (scheme generator)
          (tests scheme test)
          (scheme read)
          (only (srfi 1) unfold))


  ;; Adapted from srfi-121-test.sps

  (begin

   (define (run-generator-tests)

     ;; test-group "generators"
     ;; test-group "generators/constructors"

     (test (generator->list (generator)) '())
     (test (generator->list (generator 1 2 3)) '(1 2 3))
     (test (generator->list (make-iota-generator 3 8)) '(8 9 10))
     (test (generator->list (make-iota-generator 3 8 2)) '(8 10 12))
     (test (generator->list (make-range-generator 3) 4) '(3 4 5 6))
     (test (generator->list (make-range-generator 3 8)) '(3 4 5 6 7))
     (test (generator->list (make-range-generator 3 8 2)) '(3 5 7))
     (let ((g
            (make-coroutine-generator
             (lambda (yield) (let loop ((i 0))
                         (when (< i 3) (yield i) (loop (+ i 1))))))))
       (test (generator->list g) '(0 1 2)))
     (test (generator->list (list->generator '(1 2 3 4 5))) '(1 2 3 4 5))
     (test (generator->list (vector->generator '#(1 2 3 4 5))) '(1 2 3 4 5))
     (test (generator->list (reverse-vector->generator '#(1 2 3 4 5)))
           '(5 4 3 2 1))
     (test (generator->list (string->generator "abcde")) '(#\a #\b #\c #\d #\e))
     (test (generator->list (bytevector->generator #u8(10 20 30))) '(10 20 30))
     (test (let ((vals '()))
             (generator-for-each (lambda (n) (set! vals (cons n vals)))
                                 (generator 5 4 3 2 1))
             vals)
           '(1 2 3 4 5))           
     (test (generator->list (make-unfold-generator
                             (lambda (s) (> s 5))
                             (lambda (s) (* s 2))
                             (lambda (s) (+ s 1))
                             0))
           '(0 2 4 6 8 10))

     ;; test-group "generators/operators"

     (test (generator->list (gcons* 'a 'b (make-range-generator 0 2)))
           '(a b 0 1))
     (test (generator->list (gappend (make-range-generator 0 3)
                                     (make-range-generator 0 2)))
           '(0 1 2 0 1))
     (test (generator->list (gappend)) '())
     (let ()
       (define g1 (generator 1 2 3))
       (define g2 (generator 4 5 6 7))
       (define (proc . args) (values (apply + args) (apply + args)))
       (test (generator->list (gcombine proc 10 g1 g2)) '(15 22 31))
       (test (generator->list (gfilter odd? (make-range-generator 1 11)))
             '(1 3 5 7 9))
       (test (generator->list (gremove odd? (make-range-generator 1 11)))
             '(2 4 6 8 10)))
     (let ((g (make-range-generator 1 5)))
       (test (generator->list (gtake g 3)) '(1 2 3))
       (test (generator->list g) '(4)))
     (test (generator->list (gtake (make-range-generator 1 3) 3)) '(1 2))
     (test (generator->list (gtake (make-range-generator 1 3) 3 0)) '(1 2 0))
     (test (generator->list (gdrop (make-range-generator 1 5) 2)) '(3 4))
     (let ((g (make-range-generator 1 5)))
       (define (small? x) (< x 3))
       (test (generator->list (gtake-while small? g)) '(1 2))
       (let ((g (make-range-generator 1 5)))
         (test (generator->list (gdrop-while small? g)) '(3 4))))
     (test (generator->list (gdrop-while (lambda args #t) (generator 1 2 3)))
           '())
     (test (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2)))
           '(0.0 1.0 0 2))
     (test (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2) =))
           '(0.0 0 2))
     (test (generator->list (gindex (list->generator '(a b c d e f))
                                    (list->generator '(0 2 4))))
           '(a c e))
     (test (generator->list (gselect (list->generator '(a b c d e f))
                                     (list->generator '(#t #f #f #t #t #f))))
           '(a d e))
     (test (generator->list (gdelete-neighbor-dups (generator 1 1 2 3 3 3) =))
           '(1 2 3))
     (test (generator->list (gdelete-neighbor-dups (generator 1 2 3)
                                                   (lambda args #t)))
           '(1))

     ;; test-group "generators/consumers"

     ;; no test for plain generator->list (used throughout)

     (test (generator->list (generator 1 2 3 4 5) 3) '(1 2 3))
     (test (generator->reverse-list (generator 1 2 3 4 5)) '(5 4 3 2 1))
     (test (generator->vector (generator 1 2 3 4 5)) '#(1 2 3 4 5))
     (test (generator->vector (generator 1 2 3 4 5) 3) '#(1 2 3))
     (test (generator->string (generator #\a #\b #\c)) "abc")
     (test (let* ((p (open-input-string "a b c d e"))
                  (read (lambda () (read p))))
             (generator-fold cons 'z read))
           '(e d c b a . z))
     (let ((n 0))
       (generator-for-each (lambda values (set! n (apply + values)))
                           (generator 1) (generator 2) (generator 3))
       (test n 6))
     (test (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5)) 3)
     (test (generator-count odd? (make-range-generator 1 5)) 2)
     (let ((g (make-range-generator 2 5)))
       (test (generator-any odd? g) #t)
       (test (generator->list g) '(4)))
     (let ((g (make-range-generator 2 5)))
       (test (generator-every odd? g) #f)
       (test (generator->list g) '(3 4)))
     (test (generator-unfold (make-for-each-generator string-for-each "abc")
                             unfold)
           '(#\a #\b #\c)))

     ))
