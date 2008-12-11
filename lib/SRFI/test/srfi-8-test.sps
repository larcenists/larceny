; Test suite for SRFI-8
; 2004-01-01 / lth
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (rnrs exceptions)
        (srfi :8 receive))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define (valuez . args)
  (apply values args))

(or (call-with-current-continuation
     (lambda (k)
       (guard (c (#t (k #f)))
         (receive (a b) (valuez 1 2)
          (or (and (equal? a 1)
                   (equal? b 2))
              (fail 'receive:1)))
         (receive (a) 1
          (or (equal? a 1)
              (fail 'receive:2)))
         (receive (a) (valuez 1)
          (or (equal? a 1)
              (fail 'receive:3)))
         (receive () (valuez) #t)
         (receive (a . rest) (valuez 1 2 3)
          (or (and (equal? a 1)
                   (equal? rest '(2 3)))
              (fail 'receive:4)))
         (receive a (valuez 1 2 3)
          (or (equal? a '(1 2 3))
              (fail 'receive:5)))
         #t)))
    (fail 'receive:0))                        ; Syntax or the 0-values form

(writeln "Done.")
