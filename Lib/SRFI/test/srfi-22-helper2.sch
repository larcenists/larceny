; Will be loaded by srfi-22-test script.

(cond-expand (srfi-22))			; Does nothing, but must work

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define (main arguments)
  (display "Command line arguments: ")
  (write arguments)
  (newline)
  (or (= srfi-22-value-1 37)
      (fail 'srfi-22-value-1))
  (or (= srfi-22-value-2 3737)
      (fail 'srfi-22-value-2))
  (writeln "Done.")
  13)
