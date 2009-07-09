; Test suite for SRFI-23
; 2004-01-01 / lth
;
; $Id$

(import (except (rnrs base) error)
        (rnrs exceptions)
        (rnrs conditions)
        (rnrs io simple)
        (primitives display-condition)
        (srfi :23 error))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (condition?
     (call-with-current-continuation
      (lambda (abort)
        (guard (c (#t #;(display-condition c) (abort c)))
          (error "This is an error" "with info" 37)
          #f))))
    (fail 'error))

(writeln "Done.")