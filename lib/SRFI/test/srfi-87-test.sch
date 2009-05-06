; Test suite for SRFI-87
;
; $Id$

(cond-expand (srfi-87))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? (case (length (map sqrt '(1 4 9 16)))
              ((true) #t)
              ((false) #f)
              (else => (lambda (x) x)))
            4)
    (fail 'srfi-87))

(or (equal? (case (* 2 3)
             ((2 3 5 7) 'prime)
             ((1 4 6 8 9) 'composite))
            'composite)
    (fail 'srfi-87:composite))

(or (equal? (case (car '(c d))
             ((a) 'a)
             ((b) 'b))
            (if #f #f))
    (fail 'srfi-87:unspecified))

(or (equal? (case (car '(c d))
             ((a e i o u) 'vowel)
             ((w y) 'semivowel)
             (else 'consonant))
            'consonant)
    (fail 'srfi-87:consonant))

(writeln "Done.")
