; Test suite for SRFI-28
; 2004-01-01 / lth

(cond-expand ((and srfi-28 srfi-6)))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define testdatum '(fnord "foo" #\a))

(or (equal? "" (format ""))
    (fail 'format-empty:1))

(or (equal? (format "~a...~a" testdatum testdatum)
	    (let ((s (open-output-string)))
	      (display testdatum s)
	      (display "..." s)
	      (display testdatum s)
	      (get-output-string s)))
    (fail 'format-a:1))

(or (equal? (format "~s...~s" testdatum testdatum) 
	    (let ((s (open-output-string)))
	      (write testdatum s)
	      (display "..." s)
	      (write testdatum s)
	      (get-output-string s)))
    (fail 'format-s:1))

(or (equal? (format "~~~%") (string #\~ #\newline))
    (fail 'format-other:1))

(writeln "Done.")
