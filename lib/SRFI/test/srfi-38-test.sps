; Test suite for SRFI-38
; 2004-01-02 / lth
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (rnrs mutable-pairs)
        (srfi :6 basic-string-ports)
        (srfi :38 with-shared-structure))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? "#1=(val1 . #1#)" 
            (let ((a (cons 'val1 'val2)))
              (set-cdr! a a)
              (let ((s (open-output-string)))
                (write-with-shared-structure a s)
                (get-output-string s))))
    (fail 'write-shared:1))

(or (equal? "#1=#(val1 #1#)" 
            (let ((a (vector 'val1 'val2)))
              (vector-set! a 1 a)
              (let ((s (open-output-string)))
                (write-with-shared-structure a s)
                (get-output-string s))))
    (fail 'write-shared:2))

(or (equal? "#(#1=\"abc\" #1# #1#)"
            (let* ((d (string #\a #\b #\c))
                   (a (vector d d d))
                   (s (open-output-string)))
              (write-with-shared-structure a s)
              (get-output-string s)))
    (fail 'write-shared:3))

(or (let* ((s (open-input-string "#1=(val1 . #1#)"))
           (a (read-with-shared-structure s)))
      (and (pair? a)
           (eq? (car a) 'val1)
           (eq? (cdr a) a)))
    (fail 'read-shared:1))

(or (let* ((a (cons 1 2))
           (b (cons 1 2))
           (c (cons 3 4))
           (d (list a b c a b c))
           (p (open-output-string))
           (s (begin (write-with-shared-structure d p)
                     (get-output-string p)))
           (p (open-input-string s))
           (e (read-with-shared-structure p)))
      (and (equal? d e)
           (string=? s "(#1=(1 . 2) #2=(1 . 2) #3=(3 . 4) #1# #2# #3#)")))
    (fail 'multiple-sharing))

(writeln "Done.")

