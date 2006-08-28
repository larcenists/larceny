; Test suite for SRFI-38
; 2004-01-02 / lth

(cond-expand ((and srfi-38 srfi-6)))

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
		(with-output-to-port s
		  (lambda ()
		    (write-with-shared-structure a)))
		(get-output-string s))))
    (fail 'write-shared:2))

(or (equal? "#(#1=\"abc\" #1# #1#)"
	    (let* ((d (string #\a #\b #\c))
		   (a (vector d d d))
		   (s (open-output-string)))
	      (with-output-to-port s
		(lambda ()
		  (write-with-shared-structure a)))
	      (get-output-string s)))
    (fail 'write-shared:3))

(or (let* ((s (open-input-string "#1=(val1 . #1#)"))
	   (a (read-with-shared-structure s)))
      (and (pair? a)
	   (eq? (car a) 'val1)
	   (eq? (cdr a) a)))
    (fail 'read-shared:1))

(writeln "Done.")

