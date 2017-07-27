;;; Copyright © William D Clinger (2016).
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

;;; Embeds Olin's test harness.  Here is his copyright notice:

;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah. Don't you think source files should contain more lines
;;; of code than copyright notice?

;;; The copyright notices above are taken from srfi-132-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme sort) procedures:
;;;
;;;     list-sorted?               vector-sorted?
;;;     list-sort                  vector-sort
;;;     list-stable-sort           vector-stable-sort
;;;     list-sort!                 vector-sort!
;;;     list-stable-sort!          vector-stable-sort!
;;;     list-merge                 vector-merge
;;;     list-merge!                vector-merge!
;;;     list-delete-neighbor-dups  vector-delete-neighbor-dups
;;;     list-delete-neighbor-dups! vector-delete-neighbor-dups!
;;;
;;;     vector-find-median
;;;     vector-find-median!
;;;     vector-select!
;;;     vector-separate!


(define-library (tests scheme sort)
  (export run-sort-tests)
  (import (scheme base)
          (scheme sort)
          (tests scheme test)
          (only (srfi 27) random-integer))                ; FIXME

  ;; Adapted from srfi-132-test.sps7

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

   (define (run-sort-tests)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; Tests written for SRFI 132.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test-assert (list-sorted? > '()))

     (test-assert (list-sorted? > '(987)))

     (test-assert (list-sorted? > '(9 8 7)))

     (test-assert (vector-sorted? > '#()))

     (test-assert (vector-sorted? > '#(987)))

     (test-assert (vector-sorted? > '#(9 8 7 6 5)))

     (test-assert (vector-sorted? > '#() 0))

     (test-assert (vector-sorted? > '#(987) 1))

     (test-assert (vector-sorted? > '#(9 8 7 6 5) 1))

     (test-assert (vector-sorted? > '#() 0 0))

     (test-assert (vector-sorted? > '#(987) 1 1))

     (test-assert (vector-sorted? > '#(9 8 7 6 5) 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (list-sort > (list))
           '())

     (test (list-sort > (list 987))
           '(987))

     (test (list-sort > (list 987 654))
           '(987 654))

     (test (list-sort > (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 7 6 5 4 3 2 1 0))

     (test (list-stable-sort > (list))
           '())

     (test (list-stable-sort > (list 987))
           '(987))

     (test (list-stable-sort > (list 987 654))
           '(987 654))

     (test (list-stable-sort > (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 7 6 5 4 3 2 1 0))

     (test (list-stable-sort (lambda (x y)
                               (> (quotient x 2)
                                  (quotient y 2)))
                             (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 6 7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort > v))
           '#())

     (test (let ((v (vector 987)))
             (vector-sort > (vector 987)))
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-sort > v))
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort > v))
           '#(9 8 7 6 5 4 3 2 1 0))

     (test (let ((v (vector)))
             (vector-stable-sort > v))
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort > (vector 987)))
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-stable-sort > v))
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort > v))
           '#(9 8 7 6 5 4 3 2 1 0))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort (lambda (x y)
                                   (> (quotient x 2)
                                      (quotient y 2)))
                                 v))
           '#(9 8 6 7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort > v 0))
           '#())

     (test (let ((v (vector 987)))
             (vector-sort > (vector 987) 1))
           '#())

     (test (let ((v (vector 987 654)))
             (vector-sort > v 1))
           '#(654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort > v 3))
           '#(7 5 4 3 2 1 0))

     (test (let ((v (vector)))
             (vector-stable-sort > v 0))
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort > (vector 987) 1))
           '#())

     (test (let ((v (vector 987 654)))
             (vector-stable-sort < v 0 2))
           '#(654 987))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort > v 3))
           '#(7 5 4 3 2 1 0))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort (lambda (x y)
                                   (> (quotient x 2)
                                      (quotient y 2)))
                                 v
                                 3))
           '#(7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort > v 0 0))
           '#())

     (test (let ((v (vector 987)))
             (vector-sort > (vector 987) 1 1))
           '#())

     (test (let ((v (vector 987 654)))
             (vector-sort > v 1 2))
           '#(654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort > v 4 8))
           '#(5 4 2 0))

     (test (let ((v (vector)))
             (vector-stable-sort > v 0 0))
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort > (vector 987) 1 1))
           '#())

     (test (let ((v (vector 987 654)))
             (vector-stable-sort > v 1 2))
           '#(654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort > v 2 6))
           '#(6 4 3 0))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort (lambda (x y)
                                   (> (quotient x 2)
                                      (quotient y 2)))
                                 v
                                 1
                                 8))
           '#(8 6 4 5 3 2 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (list-sort! > (list))
           '())

     (test (list-sort! > (list 987))
           '(987))

     (test (list-sort! > (list 987 654))
           '(987 654))

     (test (list-sort! > (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 7 6 5 4 3 2 1 0))

     (test (list-stable-sort! > (list))
           '())

     (test (list-stable-sort! > (list 987))
           '(987))

     (test (list-stable-sort! > (list 987 654))
           '(987 654))

     (test (list-stable-sort! > (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 7 6 5 4 3 2 1 0))

     (test (list-stable-sort! (lambda (x y)
                                (> (quotient x 2)
                                   (quotient y 2)))
                              (list 9 8 6 3 0 4 2 5 7 1))
           '(9 8 6 7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort! > v)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-sort! > (vector 987))
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-sort! > v)
             v)
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort! > v)
             v)
           '#(9 8 7 6 5 4 3 2 1 0))

     (test (let ((v (vector)))
             (vector-stable-sort! > v)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort! > (vector 987))
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-stable-sort! > v)
             v)
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! > v)
             v)
           '#(9 8 7 6 5 4 3 2 1 0))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! (lambda (x y)
                                    (> (quotient x 2)
                                       (quotient y 2)))
                                  v)
             v)
           '#(9 8 6 7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort! > v 0)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-sort! > (vector 987) 1)
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-sort! > v 1)
             v)
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort! > v 3)
             v)
           '#(9 8 6 7 5 4 3 2 1 0))

     (test (let ((v (vector)))
             (vector-stable-sort! > v 0)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort! > (vector 987) 1)
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-stable-sort! < v 0 2)
             v)
           '#(654 987))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! > v 3)
             v)
           '#(9 8 6 7 5 4 3 2 1 0))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! (lambda (x y)
                                    (> (quotient x 2)
                                       (quotient y 2)))
                                  v
                                  3)
             v)
           '#(9 8 6 7 4 5 3 2 0 1))

     (test (let ((v (vector)))
             (vector-sort! > v 0 0)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-sort! > (vector 987) 1 1)
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-sort! > v 1 2)
             v)
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-sort! > v 4 8)
             v)
           '#(9 8 6 3 5 4 2 0 7 1))

     (test (let ((v (vector)))
             (vector-stable-sort! > v 0 0)
             v)
           '#())

     (test (let ((v (vector 987)))
             (vector-stable-sort! > (vector 987) 1 1)
             v)
           '#(987))

     (test (let ((v (vector 987 654)))
             (vector-stable-sort! > v 1 2)
             v)
           '#(987 654))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! > v 2 6)
             v)
           '#(9 8 6 4 3 0 2 5 7 1))

     (test (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
             (vector-stable-sort! (lambda (x y)
                                    (> (quotient x 2)
                                       (quotient y 2)))
                                  v
                                  1
                                  8)
             v)
           '#(9 8 6 4 5 3 2 0 7 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (list-merge > (list) (list))
           '())

     (test (list-merge > (list) (list 9 6 3 0))
           '(9 6 3 0))

     (test (list-merge > (list 9 7 5 3 1) (list))
           '(9 7 5 3 1))

     (test (list-merge > (list 9 7 5 3 1) (list 9 6 3 0))
           '(9 9 7 6 5 3 3 1 0))

     (test (list-merge! > (list) (list))
           '())

     (test (list-merge! > (list) (list 9 6 3 0))
           '(9 6 3 0))

     (test (list-merge! > (list 9 7 5 3 1) (list))
           '(9 7 5 3 1))

     (test (list-merge! > (list 9 7 5 3 1) (list 9 6 3 0))
           '(9 9 7 6 5 3 3 1 0))

     (test (vector-merge > (vector) (vector))
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0))
           '#(9 6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector))
           '#(9 7 5 3 1))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0))
           '#(9 9 7 6 5 3 3 1 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector))
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0))
             v)
           '#( 9  6  3  0 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector))
             v)
           '#( 9  7  5  3  1 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0))
             v)
           '#( 9  9  7  6  5  3  3  1  0 #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 0)
             v)
           '#( 9  6  3  0 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 0)
             v)
           '#( 9  7  5  3  1 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 0)
             v)
           '#( 9  9  7  6  5  3  3  1  0 #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2)
             v)
           '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2)
             v)
           '#(#f #f  9  7  5  3  1 #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
             v)
           '#(#f #f 9  9  7  6  5  3  3  1  0 #f))

     (test (vector-merge > (vector) (vector) 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0)
           '#(9 6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2)
           '#(5 3 1))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
           '#(9 6 5 3 3 1 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0)
             v)
           '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2)
             v)
           '#(#f #f 5  3  1 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2)
             v)
           '#(#f #f  9   6  5  3  3  1  0 #f #f #f))

     (test (vector-merge > (vector) (vector) 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0)
           '#(9 6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 5)
           '#(5 3 1))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 5)
           '#(9 6 5 3 3 1 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
             v)
           '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 5)
             v)
           '#(#f #f 5  3  1 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 5)
             v)
           '#(#f #f  9  6  5  3  3  1  0 #f #f #f))

     ;; Some tests are duplicated to make the pattern easier to discern.

     (test (vector-merge > (vector) (vector) 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0)
           '#(9 6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 4)
           '#(5 3))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4)
           '#(9 6 5 3 3 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
             v)
           '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4)
             v)
           '#(#f #f 5  3 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4)
             v)
           '#(#f #f  9  6  5  3  3  0 #f #f #f #f))

     (test (vector-merge > (vector) (vector) 0 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0 0)
           '#(9 6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
           '#(5 3))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 0)
           '#(9 6 5 3 3 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 0)
             v)
           '#(#f #f  9  6  3  0 #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
             v)
           '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 0)
             v)
           '#(#f #f  9  6  5  3  3  0 #f #f #f #f))

     (test (vector-merge > (vector) (vector) 0 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0 1)
           '#(6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
           '#(5 3))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1)
           '#(6 5 3 3 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1)
             v)
           '#(#f #f  6  3  0 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
             v)
           '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1)
             v)
           '#(#f #f  6  5  3  3  0 #f #f #f #f #f))

     (test (vector-merge > (vector) (vector) 0 0 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 4)
           '#(6 3 0))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
           '#(5 3))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 4)
           '#(6 5 3 3 0))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 4)
             v)
           '#(#f #f  6  3  0 #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
             v)
           '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 4)
             v)
           '#(#f #f  6  5  3  3  0 #f #f #f #f #f))

     (test (vector-merge > (vector) (vector) 0 0 0 0)
           '#())

     (test (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 2)
           '#(6))

     (test (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
           '#(5 3))

     (test (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 2)
           '#(6 5 3))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector) 2 0 0 0 0)
             v)
           '#(#f #f #f #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 2)
             v)
           '#(#f #f  6 #f #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
             v)
           '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

     (test (let ((v (make-vector 12 #f)))
             (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 2)
             v)
           '#(#f #f  6  5  3 #f #f #f #f #f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (list-delete-neighbor-dups char=? (list))
           '())

     (test (list-delete-neighbor-dups char=? (list #\a))
           '(#\a))

     (test (list-delete-neighbor-dups char=? (list #\a #\a #\a #\b #\b #\a))
           '(#\a #\b #\a))

     (test (list-delete-neighbor-dups! char=? (list))
           '())

     (test (list-delete-neighbor-dups! char=? (list #\a))
           '(#\a))

     (test (list-delete-neighbor-dups! char=? (list #\a #\a #\a #\b #\b #\a))
           '(#\a #\b #\a))

     (test (let ((v (vector)))
             (vector-delete-neighbor-dups char=? v))
           '#())

     (test (let ((v (vector #\a)))
             (vector-delete-neighbor-dups char=? v))
           '#(#\a))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (vector-delete-neighbor-dups char=? v))
           '#(#\a #\b #\a))

     (test (let ((v (vector)))
             (list (vector-delete-neighbor-dups! char=? v) v))
           '(0 #()))

     (test (let ((v (vector #\a)))
             (list (vector-delete-neighbor-dups! char=? v) v))
           '(1 #(#\a)))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (list (vector-delete-neighbor-dups! char=? v) v))
           '(3 #(#\a #\b #\a #\b #\b #\a)))

     (test (let ((v (vector)))
             (vector-delete-neighbor-dups char=? v 0))
           '#())

     (test (let ((v (vector #\a)))
             (vector-delete-neighbor-dups char=? v 0))
           '#(#\a))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (vector-delete-neighbor-dups char=? v 0))
           '#(#\a #\b #\a))

     (test (let ((v (vector)))
             (list (vector-delete-neighbor-dups! char=? v 0) v))
           '(0 #()))

     (test (let ((v (vector #\a)))
             (list (vector-delete-neighbor-dups! char=? v 0) v))
           '(1 #(#\a)))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (list (vector-delete-neighbor-dups! char=? v 0) v))
           '(3 #(#\a #\b #\a #\b #\b #\a)))

     (test (let ((v (vector)))
             (vector-delete-neighbor-dups char=? v 0))
           '#())

     (test (let ((v (vector #\a)))
             (vector-delete-neighbor-dups char=? v 1))
           '#())

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (vector-delete-neighbor-dups char=? v 3))
           '#(#\b #\a))

     (test (let ((v (vector)))
             (list (vector-delete-neighbor-dups! char=? v 0) v))
           '(0 #()))

     (test (let ((v (vector #\a)))
             (list (vector-delete-neighbor-dups! char=? v 1) v))
           '(1 #(#\a)))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (list (vector-delete-neighbor-dups! char=? v 3) v))
           '(5 #(#\a #\a #\a #\b #\a #\a)))

     (test (let ((v (vector)))
             (vector-delete-neighbor-dups char=? v 0 0))
           '#())

     (test (let ((v (vector #\a)))
             (vector-delete-neighbor-dups char=? v 1 1))
           '#())

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (vector-delete-neighbor-dups char=? v 3 5))
           '#(#\b))

     (test (let ((v (vector)))
             (list (vector-delete-neighbor-dups! char=? v 0 0) v))
           '(0 #()))

     (test (let ((v (vector #\a)))
             (list (vector-delete-neighbor-dups! char=? v 0 1) v))
           '(1 #(#\a)))

     (test (let ((v (vector #\a)))
             (list (vector-delete-neighbor-dups! char=? v 1 1) v))
           '(1 #(#\a)))

     (test (let ((v (vector #\a #\a #\a #\b #\b #\a)))
             (list (vector-delete-neighbor-dups! char=? v 3 5) v))
           '(4 #(#\a #\a #\a #\b #\b #\a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (vector-find-median < (vector) "knil")
           "knil")

     (test (vector-find-median < (vector 17) "knil")
           17)

     (test (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil")
           12)

     (test (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil")
           23/2)

     (test (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil" list)
           (list 12 12))

     (test (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil" list)
           (list 11 12))

     (test (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil")
           7)

     (test (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil" list)
           7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (let ((v (vector 19)))
             (vector-select! < v 0))
           19)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0))
           3)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2))
           9)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 8))
           22)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 9))
           23)

     (test (let ((v (vector 19)))
             (vector-select! < v 0 0))
           19)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0 0))
           3)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2 0))
           9)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 8 0))
           22)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 9 0))
           23)

     (test (let ((v (vector 19)))
             (vector-select! < v 0 0 1))
           19)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0 0 10))
           3)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2 0 10))
           9)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 8 0 10))
           22)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 9 0 10))
           23)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0 4 10))
           3)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2 4 10))
           13)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 4 4 10))
           21)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 5 4 10))
           23)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0 4 10))
           3)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2 4 10))
           13)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 3 4 10))
           13)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 4 4 10))
           21)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 5 4 10))
           23)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 0 4 8))
           9)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 1 4 8))
           13)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 2 4 8))
           13)

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-select! < v 3 4 8))
           21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (let ((v (vector)))
             (vector-separate! < v 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 19)))
             (vector-separate! < v 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 19)))
             (vector-separate! < v 1)
             (vector-sort < (vector-copy v 0 1)))
           '#(19))

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 3)
             (vector-sort < (vector-copy v 0 3)))
           '#(3 8 9))

     (test (let ((v (vector)))
             (vector-separate! < v 0 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 19)))
             (vector-separate! < v 0 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 19)))
             (vector-separate! < v 1 0)
             (vector-sort < (vector-copy v 0 1)))
           '#(19))

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 0 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 3 0)
             (vector-sort < (vector-copy v 0 3)))
           '#(3 8 9))

     (test (let ((v (vector 19)))
             (vector-separate! < v 0 1)
             (vector-sort < (vector-copy v 1 1)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 0 2)
             (vector-sort < (vector-copy v 2 2)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 3 2)
             (vector-sort < (vector-copy v 2 5)))
           '#(3 9 13))

     (test (let ((v (vector)))
             (vector-separate! < v 0 0 0)
             (vector-sort < (vector-copy v 0 0)))
           '#())

     (test (let ((v (vector 19)))
             (vector-separate! < v 0 1 1)
             (vector-sort < (vector-copy v 1 1)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 0 2 8)
             (vector-sort < (vector-copy v 2 2)))
           '#())

     (test (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
             (vector-separate! < v 3 2 8)
             (vector-sort < (vector-copy v 2 5)))
           '#(9 13 13))

     (for-each test-all-sorts
               '( 3  5 10 10 10 20 20 10 10 10 10 10  10  10  10  10  10)
               '( 0  1  2  3  4  5 10 20 30 40 50 99 100 101 499 500 501))

     )

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Sorting routines often have internal boundary cases or
   ;; randomness, so it's prudent to run a lot of tests with
   ;; different lengths.
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define (test-all-sorts m n)
     (if (> m 0)
         (let* ((v (random-vector n))
                (v2 (vector-copy v))
                (lst (vector->list v))
                (ans (vector-sort < v2))
                (med (cond ((= n 0) -97)
                           ((odd? n)
                            (vector-ref ans (quotient n 2)))
                           (else
                            (/ (+ (vector-ref ans (- (quotient n 2) 1))
                                  (vector-ref ans (quotient n 2)))
                               2)))))
           (define (dsort vsort!)
             (let ((v2 (vector-copy v)))
               (vsort! < v2)
               v2))
           (test (list->vector (list-sort < lst)) ans)
           (test (list->vector (list-stable-sort < lst)) ans)
           (test (list->vector (list-sort! < (list-copy lst))) ans)
           (test (list->vector (list-stable-sort! < (list-copy lst))) ans)
           (test (vector-sort < v2) ans)
           (test (vector-stable-sort < v2) ans)
           (test (dsort vector-sort!) ans)
           (test (dsort vector-stable-sort!) ans)
           (test (vector-find-median < v2 -97) med)
           (test v2 v)
           (test (vector->list v) lst)
           (test (vector-find-median! < v2 -97) med)
           (test v2 ans)
           (test-all-sorts (- m 1) n))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Adapted from Olin's code.
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; Little test harness, 'cause I'm paraoid about tricky code.

   ;; This code is
   ;;
   ;;     Copyright (c) 1998 by Olin Shivers.
   ;;
   ;; The terms are: You may do as you please with this code, as long as
   ;; you do not delete this notice or hold me responsible for any outcome
   ;; related to its use.
   ;;
   ;; Blah blah blah. Don't you think source files should contain more lines
   ;; of code than copyright notice?

   (define (random-vector size)
     (let ((v (make-vector size)))
       (fill-vector-randomly! v (* 10 size))
       v))

   (define (fill-vector-randomly! v range)
     (let ((half (quotient range 2)))
       (do ((i (- (vector-length v) 1) (- i 1)))
           ((< i 0))
         (vector-set! v i (- (random-integer range) half)))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; End of Olin's code.
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ))
