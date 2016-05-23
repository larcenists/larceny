;;; Copyright (C) William D Clinger (2016).
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for SRFI 130.
;;;
;;; To run in Larceny or Sagittarius, cd to the directory containing
;;; this file and incant:
;;;
;;;     larceny --r7rs --path . --program srfi-130-test.scm
;;;     sagittarius -r7 -L . srfi-130-test.scm
;;;
;;; Both Larceny and Sagittarius will look for the (srfi 130) library
;;; in the srfi subdirectory.  The implementations contained within
;;; the foof and srfi-130 directories can be tested by renaming those
;;; directories to srfi.
;;;
;;; FIXME: These tests are incomplete because there's  a combinatorial
;;; explosion of possibilities for optional arguments that could be
;;; either indexes or cursors.  Consider string-prefix-length, for
;;; example.  For each test that calls that procedure with all four
;;; optional arguments, there are 16 possible combinations of indexes
;;; and cursors.  Beginning with string-take, the optional arguments
;;; tested are indexes rather than cursors.

(import (scheme base)
        (scheme write)
        (only (scheme char) char-whitespace?)
        (srfi 130))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (newline)
  (writeln "Error: test failed: " token)
  (display " ")
  (write current-test)
  (newline)
  #f)

;;; To display tests that fail, change "(or " to "(OR ".
;;; To display tests before they are run, uncomment the write below.

(define current-test #f)

(define-syntax OR
  (syntax-rules ()
   ((_ expr1 expr ...)
    (begin (set! current-test 'expr1)
;          (write current-test)
;          (newline)
           (or expr1 expr ...)))))

;;; Unicode is the main motivation for string cursors, so we ought
;;; to use at least some non-ASCII strings for testing.
;;; Some systems would blow up if this file were to contain non-ASCII
;;; characters, however, so we have to be careful here.

(cond-expand (full-unicode
              (define ABC
                (list->string (map integer->char
                                   '(#x3b1 #x3b2 #x3b3))))
              (define ABCDEF
                (list->string (map integer->char
                                   '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066)))))
             (else
              (define ABC "abc")
              (define ABCDEF "abcdef")))

;;; Cursor operations

(or (string-cursor? (string-index->cursor "" 0))
    (fail 'string-cursor?))

(or (not (string-cursor? #f))
    (fail 'string-cursor?))

(or (string-cursor? (string-index->cursor (make-string 10000) 9999))
    (fail 'string-cursor?))

(or (= 0
       (string-cursor->index "" (string-cursor-start "")))
    (fail 'string-cursor-start))

(or (= 0
       (string-cursor->index ABC (string-cursor-start ABC)))
    (fail 'string-cursor-start))

(or (= 0
       (string-cursor->index "" (string-cursor-end "")))
    (fail 'string-cursor-end))

(or (= 3
       (string-cursor->index ABC (string-cursor-end ABC)))
    (fail 'string-cursor-end))

(or (= 1
       (string-cursor->index ABC (string-cursor-next ABC 0)))
    (fail 'string-cursor-next))

(or (= 2
       (string-cursor->index ABC (string-cursor-next ABC 1)))
    (fail 'string-cursor-next))

(or (= 3
       (string-cursor->index ABC (string-cursor-next ABC 2)))
    (fail 'string-cursor-next))

(or (= 0
       (string-cursor->index ABC (string-cursor-prev ABC 1)))
    (fail 'string-cursor-prev))

(or (= 1
       (string-cursor->index ABC (string-cursor-prev ABC 2)))
    (fail 'string-cursor-prev))

(or (= 2
       (string-cursor->index ABC (string-cursor-prev ABC 3)))
    (fail 'string-cursor-prev))

(or (= 0
       (string-cursor->index ABC (string-cursor-forward ABC 0 0)))
    (fail 'string-cursor-forward))

(or (= 2
       (string-cursor->index ABC (string-cursor-forward ABC 0 2)))
    (fail 'string-cursor-forward))

(or (= 3
       (string-cursor->index ABC (string-cursor-forward ABC 1 2)))
    (fail 'string-cursor-forward))

(or (= 3
       (string-cursor->index ABC (string-cursor-forward ABC 3 0)))
    (fail 'string-cursor-forward))

(or (= 0
       (string-cursor->index ABC (string-cursor-back ABC 0 0)))
    (fail 'string-cursor-back))

(or (= 0
       (string-cursor->index ABC (string-cursor-back ABC 2 2)))
    (fail 'string-cursor-back))

(or (= 1
       (string-cursor->index ABC (string-cursor-back ABC 3 2)))
    (fail 'string-cursor-back))

(or (= 3
       (string-cursor->index ABC (string-cursor-back ABC 3 0)))
    (fail 'string-cursor-back))


;;; These are supposed to work on both indexes and cursors.

(or (string-cursor=? 0 0)
    (fail 'string-cursor=?))

(or (not (string-cursor=? 0 1))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 0 2))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 0 3))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 1 0))
    (fail 'string-cursor=?))

(or (string-cursor=? 1 1)
    (fail 'string-cursor=?))

(or (not (string-cursor=? 1 2))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 1 3))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 2 0))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 2 1))
    (fail 'string-cursor=?))

(or (string-cursor=? 2 2)
    (fail 'string-cursor=?))

(or (not (string-cursor=? 2 3))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 3 0))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 3 1))
    (fail 'string-cursor=?))

(or (not (string-cursor=? 3 2))
    (fail 'string-cursor=?))

(or (string-cursor=? 3 3)
    (fail 'string-cursor=?))


(or (not (string-cursor<? 0 0))
    (fail 'string-cursor<?))

(or (string-cursor<? 0 1)
    (fail 'string-cursor<?))

(or (string-cursor<? 0 2)
    (fail 'string-cursor<?))

(or (string-cursor<? 0 3)
    (fail 'string-cursor<?))

(or (not (string-cursor<? 1 0))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 1 1))
    (fail 'string-cursor<?))

(or (string-cursor<? 1 2)
    (fail 'string-cursor<?))

(or (string-cursor<? 1 3)
    (fail 'string-cursor<?))

(or (not (string-cursor<? 2 0))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 2 1))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 2 2))
    (fail 'string-cursor<?))

(or (string-cursor<? 2 3)
    (fail 'string-cursor<?))

(or (not (string-cursor<? 3 0))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 3 1))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 3 2))
    (fail 'string-cursor<?))

(or (not (string-cursor<? 3 3))
    (fail 'string-cursor<?))


(or (not (string-cursor>? 0 0))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 0 1))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 0 2))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 0 3))
    (fail 'string-cursor>?))

(or (string-cursor>? 1 0)
    (fail 'string-cursor>?))

(or (not (string-cursor>? 1 1))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 1 2))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 1 3))
    (fail 'string-cursor>?))

(or (string-cursor>? 2 0)
    (fail 'string-cursor>?))

(or (string-cursor>? 2 1)
    (fail 'string-cursor>?))

(or (not (string-cursor>? 2 2))
    (fail 'string-cursor>?))

(or (not (string-cursor>? 2 3))
    (fail 'string-cursor>?))

(or (string-cursor>? 3 0)
    (fail 'string-cursor>?))

(or (string-cursor>? 3 1)
    (fail 'string-cursor>?))

(or (string-cursor>? 3 2)
    (fail 'string-cursor>?))

(or (not (string-cursor>? 3 3))
    (fail 'string-cursor>?))


(or (string-cursor<=? 0 0)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 0 1)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 0 2)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 0 3)
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 1 0))
    (fail 'string-cursor<=?))

(or (string-cursor<=? 1 1)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 1 2)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 1 3)
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 2 0))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 2 1))
    (fail 'string-cursor<=?))

(or (string-cursor<=? 2 2)
    (fail 'string-cursor<=?))

(or (string-cursor<=? 2 3)
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 3 0))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 3 1))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? 3 2))
    (fail 'string-cursor<=?))

(or (string-cursor<=? 3 3)
    (fail 'string-cursor<=?))


(or (string-cursor>=? 0 0)
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 0 1))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 0 2))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 0 3))
    (fail 'string-cursor>=?))

(or (string-cursor>=? 1 0)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 1 1)
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 1 2))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 1 3))
    (fail 'string-cursor>=?))

(or (string-cursor>=? 2 0)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 2 1)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 2 2)
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? 2 3))
    (fail 'string-cursor>=?))

(or (string-cursor>=? 3 0)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 3 1)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 3 2)
    (fail 'string-cursor>=?))

(or (string-cursor>=? 3 3)
    (fail 'string-cursor>=?))


(or (string-cursor=? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(or (string-cursor=? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(or (string-cursor=? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(or (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(or (string-cursor=? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor=?))


(or (not (string-cursor<? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor<?))

(or (string-cursor<? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor<?))

(or (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor<?))


(or (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor>?))

(or (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor>?))

(or (not (string-cursor>? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))


(or (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor<=?))

(or (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor<=?))

(or (string-cursor<=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))


(or (string-cursor>=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor>=?))

(or (not (string-cursor>=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor>=?))

(or (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor>=?))


(or (= 0 (string-cursor-diff ""
                             (string-index->cursor ABC 0)
                             (string-index->cursor ABC 0)))
    (fail 'string-cursor-diff))

(or (= 3 (string-cursor-diff ABC
                             (string-index->cursor ABC 0)
                             (string-index->cursor ABC 3)))
    (fail 'string-cursor-diff))

(or (= 0 (string-cursor->index "" (string-index->cursor "" 0)))
    (fail 'string-cursor->index))

(or (= 3 (string-cursor->index ABC (string-index->cursor ABC 3)))
    (fail 'string-cursor->index))

(or (= 0 (string-index->cursor "" (string-index->cursor "" 0)))
    (fail 'string-index->cursor))

(or (= 3 (string-index->cursor ABC (string-index->cursor ABC 3)))
    (fail 'string-index->cursor))

;;; Predicates

(or (string-null? "")
    (fail 'string-null))

(or (not (string-null? "abc"))
    (fail 'string-null))

(or (eqv? #t (string-every (lambda (c) (if (char? c) c #f)) ""))
    (fail 'string-every))

(or (eqv? #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))
    (fail 'string-every))

(or (eqv? #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))
    (fail 'string-every))

(or (eqv? #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
    (fail 'string-every))

(or (eqv? #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))
    (fail 'string-every))

(or (eqv? #f (string-any (lambda (c) (if (char? c) c #f)) ""))
    (fail 'string-any))

(or (eqv? #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))
    (fail 'string-any))

(or (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))
    (fail 'string-any))

(or (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
    (fail 'string-any))

(or (eqv? #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))
    (fail 'string-any))

;;; Constructors

(or (equal? ""
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             0))
    (fail 'string-tabulate))

(or (equal? "abc"
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             3))
    (fail 'string-tabulate))

(or (equal? "abc"
            (let ((p (open-input-string "abc")))
              (string-unfold eof-object?
                             values
                             (lambda (x) (read-char p))
                             (read-char p))))
    (fail 'string-unfold))

(or (equal? "" (string-unfold null? car cdr '()))
    (fail 'string-unfold))

(or (equal? "abc" (string-unfold null? car cdr (string->list "abc")))
    (fail 'string-unfold))

(or (equal? "def" (string-unfold null? car cdr '() "def"))
    (fail 'string-unfold))

(or (equal? "defabcG"
            (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           "def"
                           (lambda (x) (and (null? x) "G"))))
    (fail 'string-unfold))

(or (equal? "" (string-unfold-right null? car cdr '()))
    (fail 'string-unfold-right))

(or (equal? "cba" (string-unfold-right null? car cdr (string->list "abc")))
    (fail 'string-unfold-right))

(or (equal? "def" (string-unfold-right null? car cdr '() "def"))
    (fail 'string-unfold-right))

(or (equal? "Gcbadef"
            (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 "def"
                                 (lambda (x) (and (null? x) "G"))))
    (fail 'string-unfold-right))

;;; Conversion

(or (equal? '() (string->list/cursors ""))
    (fail 'string->list/cursors))

(or (equal? '() (string->list/cursors "" 0))
    (fail 'string->list/cursors))

(or (equal? '() (string->list/cursors "" 0 0))
    (fail 'string->list/cursors))

(or (equal? '(#\a #\b #\c) (string->list/cursors "abc"))
    (fail 'string->list/cursors))

(or (equal? '() (string->list/cursors "abc" 3))
    (fail 'string->list/cursors))

(or (equal? '(#\b #\c) (string->list/cursors "abc" 1 3))
    (fail 'string->list/cursors))

(or (equal? '(#\b #\c)
            (string->list/cursors "abc"
                                  (string-index->cursor "abc" 1)
                                  (string-index->cursor "abc" 3)))
    (fail 'string->list/cursors))

(or (equal? '#() (string->vector/cursors ""))
    (fail 'string->vector/cursors))

(or (equal? '#() (string->vector/cursors "" 0))
    (fail 'string->vector/cursors))

(or (equal? '#() (string->vector/cursors "" 0 0))
    (fail 'string->vector/cursors))

(or (equal? '#(#\a #\b #\c) (string->vector/cursors "abc"))
    (fail 'string->vector/cursors))

(or (equal? '#() (string->vector/cursors "abc" 3))
    (fail 'string->vector/cursors))

(or (equal? '#(#\b #\c) (string->vector/cursors "abc" 1 3))
    (fail 'string->vector/cursors))

(or (equal? '#(#\b #\c)
            (string->vector/cursors "abc"
                                  (string-index->cursor "abc" 1)
                                  (string-index->cursor "abc" 3)))
    (fail 'string->vector/cursors))

(or (equal? "" (reverse-list->string '()))
    (fail 'reverse-list->string))

(or (equal? "cba" (reverse-list->string '(#\a #\b #\c)))
    (fail 'reverse-list->string))

(or (equal? "" (string-join '()))
    (fail 'string-join))

(or (equal? " ab cd  e f "
            (string-join '("" "ab" "cd" "" "e" "f" "")))
    (fail 'string-join))

(or (equal? "" (string-join '() ""))
    (fail 'string-join))

(or (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") ""))
    (fail 'string-join))

(or (equal? "" (string-join '() "xyz"))
    (fail 'string-join))

(or (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
    (fail 'string-join))

(or (equal? "" (string-join '() "" 'infix))
    (fail 'string-join))

(or (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
    (fail 'string-join))

(or (equal? "" (string-join '() "xyz" 'infix))
    (fail 'string-join))

(or (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))
    (fail 'string-join))

(or (equal? 'horror
            (guard (exn (#t 'horror))
             (string-join '() "" 'strict-infix)))
    (fail 'string-join))

(or (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
    (fail 'string-join))

(or (equal? 'wham
            (guard (exn (else 'wham))
             (string-join '() "xyz" 'strict-infix)))
    (fail 'string-join))

(or (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
    (fail 'string-join))

(or (equal? "" (string-join '() "" 'suffix))
    (fail 'string-join))

(or (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
    (fail 'string-join))

(or (equal? "" (string-join '() "xyz" 'suffix))
    (fail 'string-join))

(or (equal? "xyzabxyzcdxyzxyzexyzfxyzxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
    (fail 'string-join))

(or (equal? "" (string-join '() "" 'prefix))
    (fail 'string-join))

(or (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
    (fail 'string-join))

(or (equal? "" (string-join '() "xyz" 'prefix))
    (fail 'string-join))

(or (equal? "xyzxyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))
    (fail 'string-join))

;;; Selection

(or (char=? #\a (string-ref/cursor "abc" 0))
    (fail 'string-ref/cursor))

(or (char=? #\c (string-ref/cursor "abc" 2))
    (fail 'string-ref/cursor))

(or (char=? #\a (string-ref/cursor "abc" (string-index->cursor "abc" 0)))
    (fail 'string-ref/cursor))

(or (char=? #\c (string-ref/cursor "abc" (string-index->cursor "abc" 2)))
    (fail 'string-ref/cursor))

(or (string=? "" (substring/cursors "" 0 0))
    (fail 'substring/cursors))

(or (string=? "" (substring/cursors "abc" 0 0))
    (fail 'substring/cursors))

(or (string=? "" (substring/cursors "abc" 3 3))
    (fail 'substring/cursors))

(or (string=? ABC (substring/cursors ABC 0 3))
    (fail 'substring/cursors))

(or (string=? ABC
              (substring/cursors ABC
                                 (string-index->cursor "abc" 0)
                                 (string-index->cursor "abc" 3)))
    (fail 'substring/cursors))

(or (string=? "b" (substring/cursors "abc" 1 2))
    (fail 'substring/cursors))

(or (string=? "" (string-copy/cursors ""))
    (fail 'string-copy/cursors))

(or (string=? "abc" (string-copy/cursors "abc"))
    (fail 'string-copy/cursors))

(or (string=? "" (string-copy/cursors "abc" 3))
    (fail 'string-copy/cursors))

(or (string=? "c" (string-copy/cursors "abc" 2))
    (fail 'string-copy/cursors))

(or (string=? "abc" (string-copy/cursors "abc" 0))
    (fail 'string-copy/cursors))

(or (string=? "b" (string-copy/cursors "abc" 1 2))
    (fail 'string-copy/cursors))

(or (string=? "" (string-copy/cursors "" 0 0))
    (fail 'string-copy/cursors))

(or (string=? "" (string-copy/cursors "abc" 0 0))
    (fail 'string-copy/cursors))

(or (string=? "" (string-copy/cursors "abc" 3 3))
    (fail 'string-copy/cursors))

(or (string=? "abc" (string-copy/cursors "abc" 0 3))
    (fail 'string-copy/cursors))

(or (string=? "b" (string-copy/cursors "abc" 1 2))
    (fail 'string-copy/cursors))

(or (string=? (substring ABC 1 2)
              (string-copy/cursors ABC
                                   (string-index->cursor "abc" 1)
                                   (string-index->cursor "abc" 2)))
    (fail 'string-copy/cursors))

(or (string=? "" (string-take "" 0))
    (fail 'string-take))

(or (string=? "" (string-take "abcdef" 0))
    (fail 'string-take))

(or (string=? "ab" (string-take "abcdef" 2))
    (fail 'string-take))

(or (string=? "" (string-drop "" 0))
    (fail 'string-drop))

(or (string=? "abcdef" (string-drop "abcdef" 0))
    (fail 'string-drop))

(or (string=? "cdef" (string-drop "abcdef" 2))
    (fail 'string-drop))

(or (string=? "" (string-take-right "" 0))
    (fail 'string-take-right))

(or (string=? "" (string-take-right "abcdef" 0))
    (fail 'string-take-right))

(or (string=? "ef" (string-take-right "abcdef" 2))
    (fail 'string-take-right))

(or (string=? "" (string-drop-right "" 0))
    (fail 'string-drop-right))

(or (string=? "abcdef" (string-drop-right "abcdef" 0))
    (fail 'string-drop-right))

(or (string=? "abcd" (string-drop-right "abcdef" 2))
    (fail 'string-drop-right))

(or (string=? "" (string-pad "" 0))
    (fail 'string-pad))

(or (string=? "     " (string-pad "" 5))
    (fail 'string-pad))

(or (string=? "  325" (string-pad "325" 5))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "71325" 5))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "8871325" 5))
    (fail 'string-pad))

(or (string=? "" (string-pad "" 0 #\*))
    (fail 'string-pad))

(or (string=? "*****" (string-pad "" 5 #\*))
    (fail 'string-pad))

(or (string=? "**325" (string-pad "325" 5 #\*))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "71325" 5 #\*))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "8871325" 5 #\*))
    (fail 'string-pad))

(or (string=? "" (string-pad "" 0 #\* 0))
    (fail 'string-pad))

(or (string=? "*****" (string-pad "" 5 #\* 0))
    (fail 'string-pad))

(or (string=? "**325" (string-pad "325" 5 #\* 0))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "71325" 5 #\* 0))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "8871325" 5 #\* 0))
    (fail 'string-pad))

(or (string=? "***25" (string-pad "325" 5 #\* 1))
    (fail 'string-pad))

(or (string=? "*1325" (string-pad "71325" 5 #\* 1))
    (fail 'string-pad))

(or (string=? "71325" (string-pad "8871325" 5 #\* 1))
    (fail 'string-pad))

(or (string=? "" (string-pad "" 0 #\* 0 0))
    (fail 'string-pad))

(or (string=? "*****" (string-pad "" 5 #\* 0 0))
    (fail 'string-pad))

(or (string=? "**325" (string-pad "325" 5 #\* 0 3))
    (fail 'string-pad))

(or (string=? "**713" (string-pad "71325" 5 #\* 0 3))
    (fail 'string-pad))

(or (string=? "**887" (string-pad "8871325" 5 #\* 0 3))
    (fail 'string-pad))

(or (string=? "***25" (string-pad "325" 5 #\* 1 3))
    (fail 'string-pad))

(or (string=? "**132" (string-pad "71325" 5 #\* 1 4))
    (fail 'string-pad))

(or (string=? "*8713" (string-pad "8871325" 5 #\* 1 5))
    (fail 'string-pad))

(or (string=? "" (string-pad-right "" 0))
    (fail 'string-pad-right))

(or (string=? "     " (string-pad-right "" 5))
    (fail 'string-pad-right))

(or (string=? "325  " (string-pad-right "325" 5))
    (fail 'string-pad-right))

(or (string=? "71325" (string-pad-right "71325" 5))
    (fail 'string-pad-right))

(or (string=? "88713" (string-pad-right "8871325" 5))
    (fail 'string-pad-right))

(or (string=? "" (string-pad-right "" 0 #\*))
    (fail 'string-pad-right))

(or (string=? "*****" (string-pad-right "" 5 #\*))
    (fail 'string-pad-right))

(or (string=? "325**" (string-pad-right "325" 5 #\*))
    (fail 'string-pad-right))

(or (string=? "71325" (string-pad-right "71325" 5 #\*))
    (fail 'string-pad-right))

(or (string=? "88713" (string-pad-right "8871325" 5 #\*))
    (fail 'string-pad-right))

(or (string=? "" (string-pad-right "" 0 #\* 0))
    (fail 'string-pad-right))

(or (string=? "*****" (string-pad-right "" 5 #\* 0))
    (fail 'string-pad-right))

(or (string=? "325**" (string-pad-right "325" 5 #\* 0))
    (fail 'string-pad-right))

(or (string=? "71325" (string-pad-right "71325" 5 #\* 0))
    (fail 'string-pad-right))

(or (string=? "88713" (string-pad-right "8871325" 5 #\* 0))
    (fail 'string-pad-right))

(or (string=? "25***" (string-pad-right "325" 5 #\* 1))
    (fail 'string-pad-right))

(or (string=? "1325*" (string-pad-right "71325" 5 #\* 1))
    (fail 'string-pad-right))

(or (string=? "87132" (string-pad-right "8871325" 5 #\* 1))
    (fail 'string-pad-right))

(or (string=? "" (string-pad-right "" 0 #\* 0 0))
    (fail 'string-pad-right))

(or (string=? "*****" (string-pad-right "" 5 #\* 0 0))
    (fail 'string-pad-right))

(or (string=? "325**" (string-pad-right "325" 5 #\* 0 3))
    (fail 'string-pad-right))

(or (string=? "713**" (string-pad-right "71325" 5 #\* 0 3))
    (fail 'string-pad-right))

(or (string=? "887**" (string-pad-right "8871325" 5 #\* 0 3))
    (fail 'string-pad-right))

(or (string=? "25***" (string-pad-right "325" 5 #\* 1 3))
    (fail 'string-pad-right))

(or (string=? "132**" (string-pad-right "71325" 5 #\* 1 4))
    (fail 'string-pad-right))

(or (string=? "8713*" (string-pad-right "8871325" 5 #\* 1 5))
    (fail 'string-pad-right))


(or (string=? "" (string-trim ""))
    (fail 'string-trim))

(or (string=? "a  b  c  " (string-trim "  a  b  c  "))
    (fail 'string-trim))

(or (string=? "" (string-trim "" char-whitespace?))
    (fail 'string-trim))

(or (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace?))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char?))
    (fail 'string-trim))

(or (string=? "" (string-trim "" char-whitespace? 0))
    (fail 'string-trim))

(or (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 0))
    (fail 'string-trim))

(or (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 3))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 0 11))
    (fail 'string-trim))

(or (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 3 11))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 0 8))
    (fail 'string-trim))

(or (string=? "b  " (string-trim "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim))

(or (string=? "" (string-trim "  a  b  c  " char? 3 8))
    (fail 'string-trim))


(or (string=? "" (string-trim-right ""))
    (fail 'string-trim-right))

(or (string=? "  a  b  c" (string-trim-right "  a  b  c  "))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "" char-whitespace?))
    (fail 'string-trim-right))

(or (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace?))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char?))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "" char-whitespace? 0))
    (fail 'string-trim-right))

(or (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 0))
    (fail 'string-trim-right))

(or (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 3))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 0 11))
    (fail 'string-trim-right))

(or (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 3 11))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 0 8))
    (fail 'string-trim-right))

(or (string=? "  b" (string-trim-right "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim-right))

(or (string=? "" (string-trim-right "  a  b  c  " char? 3 8))
    (fail 'string-trim-right))


(or (string=? "" (string-trim-both ""))
    (fail 'string-trim-both))

(or (string=? "a  b  c" (string-trim-both "  a  b  c  "))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "" char-whitespace?))
    (fail 'string-trim-both))

(or (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace?))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char?))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "" char-whitespace? 0))
    (fail 'string-trim-both))

(or (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 0))
    (fail 'string-trim-both))

(or (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 3))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 0 11))
    (fail 'string-trim-both))

(or (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 3 11))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 0 8))
    (fail 'string-trim-both))

(or (string=? "b" (string-trim-both "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim-both))

(or (string=? "" (string-trim-both "  a  b  c  " char? 3 8))
    (fail 'string-trim-both))


(or (= 0 (string-prefix-length "" ""))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee"))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" ""))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee"))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "aisle" "aabbccddee"))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "bail" "aabbccddee"))
    (fail 'string-prefix-length))

(or (= 4 (string-prefix-length "prefix" "preface"))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "" 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee" 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 0))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "aisle" "aabbccddee" 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "bail" "aabbccddee" 0))
    (fail 'string-prefix-length))

(or (= 4 (string-prefix-length "prefix" "preface" 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 1))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "bail" "aabbccddee" 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "prefix" "preface" 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "" 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee" 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 0 4))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "aisle" "aabbccddee" 0 4))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "bail" "aabbccddee" 0 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 1 4))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "bail" "aabbccddee" 1 4))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "prefix" "preface" 1 5))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "" 0 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee" 0 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 0 4 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "prefix" "preface" 0 5 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 1 4 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))
    (fail 'string-prefix-length))

(or (= 3 (string-prefix-length "prefix" "preface" 1 5 1))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "" 0 0 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 0 4 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))
    (fail 'string-prefix-length))

(or (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "prefix" "preface" 0 5 1 6))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "" 1 4 0 0))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))
    (fail 'string-prefix-length))

(or (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))
    (fail 'string-prefix-length))

(or (= 3 (string-prefix-length "prefix" "preface" 1 5 1 7))
    (fail 'string-prefix-length))


(or (= 0 (string-suffix-length "" ""))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee"))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" ""))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee"))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "aisle" "aabbccddee"))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee"))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface"))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "" 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee" 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 0))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "aisle" "aabbccddee" 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 0))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 1))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "aisle" "aabbccddee" 1))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 1))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 1))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "" 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee" 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 0 4))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 0 1))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 1 4))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "aisle" "aabbccddee" 1 5))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 1 4))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 1 5))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "" 0 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee" 0 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 0 4 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 0 5 1))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 1 4 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 1 5 1))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "" 0 0 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 0 4 0 0))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))
    (fail 'string-suffix-length))

(or (= 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "place" "preface" 0 5 1 6))
    (fail 'string-suffix-length))

(or (= 2 (string-suffix-length "place" "preface" 0 4 1 6))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "" 1 4 0 0))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))
    (fail 'string-suffix-length))

(or (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))
    (fail 'string-suffix-length))

(or (= 3 (string-suffix-length "place" "preface" 1 5 1 7))
    (fail 'string-suffix-length))


(or (eq? #t (string-prefix? "" ""))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "" "abc"))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc"))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc"))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc"))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc"))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "abc" "abc"))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" ""))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "" "abc"))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "a" "abc"))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc"))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc"))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc"))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc"))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "" "" 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "abc" "abc" 0))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" "" 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "a" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 0))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "ab" "abc" 2))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ac" "abc" 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 2))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "ac" "abc" 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 2))
    (fail 'string-suffix?))


(or (eq? #t (string-prefix? "" "" 0 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "" "abc" 0 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc" 0 1))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 1))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0 2))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "abc" "abc" 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" "" 0 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "" "abc" 0 0))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "a" "abc" 0 1))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0 1))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "ac" "abc" 1 2))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc" 0 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "ab" "abc" 2 2))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ac" "abc" 2 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 2 3))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "ac" "abc" 2 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 2 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 2 3))
    (fail 'string-suffix?))


(or (eq? #t (string-prefix? "" "" 0 0 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "" "abc" 0 0 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc" 0 1 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 1 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 2 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0 2 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "abc" "abc" 0 3 0))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" "" 0 0 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "" "abc" 0 0 0))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "a" "abc" 0 1 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0 1 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "ac" "abc" 1 2 0))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc" 0 2 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0 2 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 0 3 0))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "ab" "abc" 2 2 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ac" "abc" 2 2 0))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 2 3 0))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "ac" "abc" 2 2 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 2 2 0))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 2 3 0))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "" "abc" 0 0 1))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0 1))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "c" "abc" 0 1 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ab" "abc" 0 1 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ab" "abc" 0 2 1))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0 2 1))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 0 3 1))
    (fail 'string-prefix?))

(or (eq? #f (string-suffix? "a" "abc" 0 1 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0 1 1))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "ac" "abc" 1 2 2))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0 2 1))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "bc" "abc" 0 2 2))
    (fail 'string-suffix?))


(or (eq? #t (string-prefix? "" "" 0 0 0 0))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "" "abc" 0 0 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0 0 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc" 0 1 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 1 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 2 0 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0 2 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "abc" "abc" 0 3 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" "abc" 0 0 0 3))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "a" "abc" 0 1 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0 1 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "ac" "abc" 1 2 0 3))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc" 0 2 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0 2 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 0 3 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "ab" "abc" 2 2 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ac" "abc" 2 2 0 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 2 3 0 3))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "ac" "abc" 2 2 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 2 2 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "abc" "abc" 2 3 0 3))
    (fail 'string-suffix?))

(or (eq? #t (string-prefix? "" "abc" 0 0 1 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0 1 3))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "c" "abc" 0 1 2 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ab" "abc" 0 1 2 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ab" "abc" 0 2 1 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "ac" "abc" 0 2 1 3))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 0 3 1 3))
    (fail 'string-prefix?))

(or (eq? #f (string-suffix? "a" "abc" 0 1 2 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "c" "abc" 0 1 1 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "ac" "abc" 1 2 2 3))
    (fail 'string-suffix?))

(or (eq? #t (string-suffix? "bc" "abc" 0 2 1 3))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "bc" "abc" 0 2 2 3))
    (fail 'string-suffix?))


(or (eq? #t (string-prefix? "" "abc" 0 0 0 2))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "a" "abc" 0 0 0 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "c" "abc" 0 1 0 2))
    (fail 'string-prefix?))

(or (eq? #t (string-prefix? "ab" "abc" 0 1 0 2))
    (fail 'string-prefix?))

(or (eq? #f (string-prefix? "abc" "abc" 0 3 0 2))
    (fail 'string-prefix?))

(or (eq? #t (string-suffix? "" "abc" 0 0 0 2))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "c" "abc" 0 1 0 2))
    (fail 'string-suffix?))

(or (eq? #f (string-suffix? "ac" "abc" 1 2 0 2))
    (fail 'string-suffix?))

;;; Searching

(or (= 0
       (string-cursor->index ""
                             (string-index "" char?)))
    (fail 'string-index))

(or (= 0
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char?)))
    (fail 'string-index))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)))))
    (fail 'string-index))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char-whitespace?)))
    (fail 'string-index))

(or (= 0
       (string-cursor->index "abcdef"
                             (string-index-right "" char?)))
    (fail 'string-index-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef" char?)))
    (fail 'string-index-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c) (char>? c #\d)))))
    (fail 'string-index-right))

(or (= 0
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef" char-whitespace?)))
    (fail 'string-index-right))

(or (= 0
       (string-cursor->index "" (string-skip "" string?)))
    (fail 'string-skip))

(or (= 0
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" string?)))
    (fail 'string-skip))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)))))
    (fail 'string-skip))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" char?)))
    (fail 'string-skip))

(or (= 0
       (string-cursor->index "" (string-skip-right "" string?)))
    (fail 'string-skip-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" string?)))
    (fail 'string-skip-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c) (char<=? c #\d)))))
    (fail 'string-skip-right))

(or (= 0
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" char?)))
    (fail 'string-skip-right))


(or (= 2
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char? 2)))
    (fail 'string-index))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2)))
    (fail 'string-index))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char-whitespace? 2)))
    (fail 'string-index))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef" char? 2)))
    (fail 'string-index-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2)))
    (fail 'string-index-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef" char-whitespace? 2)))
    (fail 'string-index-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" string? 2)))
    (fail 'string-skip))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c)
                                            (char<=? c #\d)) 2)))
    (fail 'string-skip))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" char? 2)))
    (fail 'string-skip))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" string? 2)))
    (fail 'string-skip-right))

(or (= 6
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2)))
    (fail 'string-skip-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" char? 2)))
    (fail 'string-skip-right))


(or (= 2
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char? 2 5)))
    (fail 'string-index))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2 5)))
    (fail 'string-index))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-index "abcdef" char-whitespace? 2 5)))
    (fail 'string-index))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef" char? 2 5)))
    (fail 'string-index-right))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2 5)))
    (fail 'string-index-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-index-right "abcdef"
                                                 char-whitespace? 2 5)))
    (fail 'string-index-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" string? 2 5)))
    (fail 'string-skip))

(or (= 4
       (string-cursor->index "abcdef"
                             (string-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)) 2 5)))
    (fail 'string-skip))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-skip "abcdef" char? 2 5)))
    (fail 'string-skip))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" string? 2 5)))
    (fail 'string-skip-right))

(or (= 5
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2 5)))
    (fail 'string-skip-right))

(or (= 2
       (string-cursor->index "abcdef"
                             (string-skip-right "abcdef" char? 2 5)))
    (fail 'string-skip-right))


(or (eqv? 0
          (string-cursor->index ""
                                (string-contains "" "")))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "")))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "a")))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "ff")))
    (fail 'string-contains))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "eff")))
    (fail 'string-contains))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "foo")))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "efffoo")))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index ""
                                (string-contains-right "" "")))
    (fail 'string-contains-right))

(or (eqv? 11
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "")))
    (fail 'string-contains-right))

(or (eqv? 0
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "a")))
    (fail 'string-contains-right))

(or (eqv? 7
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "ff")))
    (fail 'string-contains-right))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "eff")))
    (fail 'string-contains-right))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo" "foo")))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo")))
    (fail 'string-contains-right))


(or (eqv? 0
          (string-cursor->index ""
                                (string-contains "" "" 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "" 2)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "a" 2)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "ff" 2)))
    (fail 'string-contains))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "eff" 2)))
    (fail 'string-contains))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "foo" 2)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo" "efffoo" 2)))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index ""
                                (string-contains-right "" "" 0)))
    (fail 'string-contains-right))

(or (eqv? 11
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2)))
    (fail 'string-contains-right))

(or (eqv? 7
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2)))
    (fail 'string-contains-right))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2)))
    (fail 'string-contains-right))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2)))
    (fail 'string-contains-right))


(or (eqv? 0
          (string-cursor->index ""
                                (string-contains "" "" 0 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10)))
    (fail 'string-contains))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10)))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index ""
                                (string-contains-right "" "" 0 0)))
    (fail 'string-contains-right))

(or (eqv? 10
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10)))
    (fail 'string-contains-right))

(or (eqv? 7
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10)))
    (fail 'string-contains-right))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10)))
    (fail 'string-contains-right))


(or (eqv? 0
          (string-cursor->index ""
                                (string-contains "" "" 0 0 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10 1)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10 1)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10 1)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10 1)))
    (fail 'string-contains))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10 1)))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index ""
                                (string-contains-right "" "" 0 0 0)))
    (fail 'string-contains-right))

(or (eqv? 10
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10 0)))
    (fail 'string-contains-right))

(or (eqv? 10
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10 1)))
    (fail 'string-contains-right))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1)))
    (fail 'string-contains-right))

(or (eqv? 7
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1)))
    (fail 'string-contains-right))

(or (eqv? #f
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1)))
    (fail 'string-contains-right))


(or (eqv? 0
          (string-cursor->index ""
                                (string-contains "" "" 0 0 0 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "" 2 10 0 0)))
    (fail 'string-contains))

(or (eqv? 2
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "a" 2 10 1 1)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "ff" 2 10 1 2)))
    (fail 'string-contains))

(or (eqv? 5
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "eff" 2 10 1 2)))
    (fail 'string-contains))

(or (eqv? 9
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "foo" 2 10 1 2)))
    (fail 'string-contains))

(or (eqv? 4
          (string-cursor->index "abcdeffffoo"
                                (string-contains "abcdeffffoo"
                                                 "efffoo" 2 10 0 2)))
    (fail 'string-contains))

(or (eqv? 0
          (string-cursor->index ""
                                (string-contains-right "" "" 0 0 0 0)))
    (fail 'string-contains-right))

(or (eqv? 10
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "" 2 10 0 0)))
    (fail 'string-contains-right))

(or (eqv? 10
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "a" 2 10 1 1)))
    (fail 'string-contains-right))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1 2)))
    (fail 'string-contains-right))

(or (eqv? 8
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1 2)))
    (fail 'string-contains-right))

(or (eqv? 9
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1 2)))
    (fail 'string-contains-right))

(or (eqv? 7
          (string-cursor->index "abcdeffffoo"
                                (string-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1 3)))
    (fail 'string-contains-right))

;;; The whole string

(or (string=? "" (string-reverse ""))
    (fail 'string-reverse))

(or (string=? "fedcba" (string-reverse "abcdef"))
    (fail 'string-reverse))

(or (string=? "" (string-reverse "" 0))
    (fail 'string-reverse))

(or (string=? "fedcba" (string-reverse "abcdef" 0))
    (fail 'string-reverse))

(or (string=? "fedc" (string-reverse "abcdef" 2))
    (fail 'string-reverse))

(or (string=? "" (string-reverse "" 0 0))
    (fail 'string-reverse))

(or (string=? "fedcba" (string-reverse "abcdef" 0 6))
    (fail 'string-reverse))

(or (string=? "edc" (string-reverse "abcdef" 2 5))
    (fail 'string-reverse))


(or (string=? "" (string-concatenate '()))
    (fail 'string-concatenate))

(or (string=? "abcdef" (string-concatenate '("" "a" "bcd" "" "ef" "" "")))
    (fail 'string-concatenate))

(or (string=? "" (string-concatenate-reverse '()))
    (fail 'string-concatenate-reverse))

(or (string=? "efbcda"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))
    (fail 'string-concatenate-reverse))

(or (string=? "huh?" (string-concatenate-reverse '() "huh?"))
    (fail 'string-concatenate-reverse))

(or (string=? "efbcdaxy"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
    (fail 'string-concatenate-reverse))

(or (string=? "huh" (string-concatenate-reverse '() "huh?" 3))
    (fail 'string-concatenate-reverse))

(or (string=? "efbcdax"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1))
    (fail 'string-concatenate-reverse))


(or (= 8
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "))
    (fail 'string-fold))

(or (= 7
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1))
    (fail 'string-fold))

(or (= 6
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1
                    32))
    (fail 'string-fold))

(or (equal? (string->list "abcdef")
            (string-fold-right cons '() "abcdef"))
    (fail 'string-fold-right))

(or (equal? (string->list "def")
            (string-fold-right cons '() "abcdef" 3))
    (fail 'string-fold-right))

(or (equal? (string->list "cde")
            (string-fold-right cons '() "abcdef" 2 5))
    (fail 'string-fold-right))

(or (string=? "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (string-fold (lambda (c sum)
                                             (+ sum (if (char=? c #\a) 2 1)))
                                           0 s))
                     (ans (make-string ans-len)))
                (string-fold (lambda (c i)
                               (let ((i (if (char=? c #\a)
                                            (begin (string-set! ans i #\a)
                                                   (+ i 1))
                                                   i)))
                                 (string-set! ans i c)
                             (+ i 1)))
                             0 s)
                ans))
    (fail 'string-fold))


(or (equal? '(101 100 99 98 97)
            (let ((s "abcde") (v '()))
              (string-for-each-cursor
               (lambda (cur)
                 (set! v (cons (char->integer (string-ref/cursor s cur)) v)))
               s)
              v))
    (fail 'string-for-each-cursor))


(or (string=? "cdefabcdefabcd"
              (string-replicate "abcdef" -4 10))
    (fail 'string-replicate))

(or (string=? "bcdefbcdefbcd"
              (string-replicate "abcdef" 90 103 1))
    (fail 'string-replicate))

(or (string=? "ecdecdecde"
              (string-replicate "abcdef" -13 -3 2 5))
    (fail 'string-replicate))


(or (= 6 (string-count "abcdef" char?))
    (fail 'string-count))

(or (= 4 (string-count "counting  whitespace, again " char-whitespace? 5))
    (fail 'string-count))

(or (= 3 (string-count "abcdefwxyz"
                       (lambda (c) (odd? (char->integer c)))
                       2 8))
    (fail 'string-count))


(or (string=? "It's lots of fun to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "lots of fun"
                              5 9))
    (fail 'string-replace))

(or (string=? "The miserable perl programmer endured daily ridicule."
              (string-replace "The TCL programmer endured daily ridicule."
                              "another miserable perl drone"
                              4 7 8 22))
    (fail 'string-replace))

(or (string=? "It's really easy to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "really "
                              5 5))
    (fail 'string-replace))


(or (equal? '() (string-split "" ""))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" ""))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " "))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***"))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'infix))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'infix))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix))
    (fail 'string-split))

(or (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix)))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'prefix))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'prefix))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix))
    (fail 'string-split))

(or (equal? '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'suffix))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'suffix))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix))
    (fail 'string-split))


(or (equal? '() (string-split "" "" 'infix #f))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'infix #f))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix #f))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix #f))
    (fail 'string-split))

(or (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix #f)))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix #f))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix #f))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix #f))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'prefix #f))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'prefix #f))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix #f))
    (fail 'string-split))

(or (equal? '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix #f))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'suffix #f))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'suffix #f))
    (fail 'string-split))

(or (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix #f))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix #f))
    (fail 'string-split))


(or (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3)))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix 3))
    (fail 'string-split))

(or (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'strict-infix 3))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'prefix 3))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'prefix 3))
    (fail 'string-split))

(or (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3))
    (fail 'string-split))

(or (equal? '("there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'prefix 3))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'suffix 3))
    (fail 'string-split))

(or (equal? '("a" "b" "c") (string-split "abc" "" 'suffix 3))
    (fail 'string-split))

(or (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3))
    (fail 'string-split))

(or (equal? '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'suffix 3))
    (fail 'string-split))


(or (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3 0)))
    (fail 'string-split))

(or (equal? '("b" "c") (string-split "abc" "" 'strict-infix 3 1))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3 1))
    (fail 'string-split))

(or (equal? '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix 3 1))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'prefix 3 0))
    (fail 'string-split))

(or (equal? '("b" "c") (string-split "abc" "" 'prefix 3 1))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3 1))
    (fail 'string-split))

(or (equal? '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix 3 1))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'suffix 3 0))
    (fail 'string-split))

(or (equal? '("b" "c") (string-split "abc" "" 'suffix 3 1))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3 1))
    (fail 'string-split))

(or (equal? '("**there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix 3 1))
    (fail 'string-split))


(or (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3 0 0)))
    (fail 'string-split))

(or (equal? '("b") (string-split "abc" "" 'strict-infix 3 1 2))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'strict-infix 3 1 11))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'prefix 3 0 0))
    (fail 'string-split))

(or (equal? '("b") (string-split "abc" "" 'prefix 3 1 2))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'prefix 3 1 11))
    (fail 'string-split))

(or (equal? '() (string-split "" "" 'suffix 3 0 0))
    (fail 'string-split))

(or (equal? '("b") (string-split "abc" "" 'suffix 3 1 2))
    (fail 'string-split))

(or (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'suffix 3 1 11))
    (fail 'string-split))


(or (string=? "aiueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"))
    (fail 'string-filter))

(or (string=? "And wmn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"))
    (fail 'string-remove))

(or (string=? "iueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             4))
    (fail 'string-filter))

(or (string=? "mn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"
                             6))
    (fail 'string-remove))

(or (string=? "aaao"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             16 32))
    (fail 'string-filter))

(or (string=? "And woman, that sh may know"
              (string-remove (lambda (c) (memv c (string->list "eiu")))
                             "And woman, that she may know number?"
                             0 28))
    (fail 'string-remove))



(writeln "Done.")
