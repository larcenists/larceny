; Test suite for SRFI-30
; 2004-01-01 / lth

(cond-expand (srfi-30))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

; Test basic functionality
#|
(fail 'comment-content-not-ignored:1)
|#

; Test nesting/unnesting

#|
#||# (fail 'comment-content-not-ignored:2) |#

; Test whitespace property?  Don't know how to do that.

(writeln "Done.")
