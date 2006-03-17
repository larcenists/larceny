; Test suite for SRFI-7
; 2004-01-01 / lth
;
; This program will load additional files and must be run from the
; srfi-test directory.

(cond-expand (srfi-7))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(program (requires larceny)
	 (feature-cond (bigloo (files "file-does-not-exist.sch"))
		       (else   (files "srfi-7-helper1.sch")))
	 (requires srfi-7 srfi-8)
	 (code
	  (define srfi-7-main 37))
	 (files "srfi-7-helper2.sch" "srfi-7-helper3.sch")
	 (files))

(or (equal? srfi-7-main 37)
    (fail 'srfi-7-main))
(or (equal? srfi-7-helper1 37)
    (fail 'srfi-7-helper1))
(or (equal? srfi-7-helper2 37)
    (fail 'srfi-7-helper2))
(or (equal? srfi-7-helper3 37)
    (fail 'srfi-7-helper3))

; Tests that SRFI-8 was loaded properly
(receive (a b) (values 1 2)
  (or (and (equal? a 1)
	   (equal? b 2))
      (fail 'srfi-8-loaded)))

(writeln "Done.")

      


