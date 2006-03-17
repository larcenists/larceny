; Test suite for SRFI-6
; 2004-01-01 / lth

(cond-expand (srfi-6))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (input-port? (open-input-string "foo"))
    (fail 'open-input-string:1))

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
		         (k #f))))
         (close-input-port (open-input-string "foo"))
	 #t)))
    (fail 'close-input-port:1))

(or (char-ready? (open-input-string "foo"))
    (fail 'char-ready?:1))
(or (char-ready? (open-input-string ""))
    (fail 'char-ready?:2))

(or (char=? #\f (peek-char (open-input-string "foo")))
    (fail 'peek-char:1))
(or (eof-object? (peek-char (open-input-string "")))
    (fail 'peek-char:2))

(or (char=? #\f (read-char (open-input-string "foo")))
    (fail 'read-char:1))
(or (eof-object? (read-char (open-input-string "")))
    (fail 'read-char:2))

(or (eq? 'foo (read (open-input-string "foo")))
    (fail 'read:1))

(or (output-port? (open-output-string))
    (fail 'open-output-string:1))

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #f))))
         (close-output-port (open-output-string))
	 #t)))
    (fail 'close-output-port:1))

(let ((p (open-output-string)))
  (write-char #\f p)
  (write-char #\o p)
  (write-char #\o p)
  (write-char #\space p)
  (write 'abracadabra p)

  (let ((s (get-output-string p)))
    (or (string? s)
	(fail 'get-output-string:1))
    (or (string=? s "foo abracadabra")
	(fail 'get-output-string:2))))

(writeln "Done.")
