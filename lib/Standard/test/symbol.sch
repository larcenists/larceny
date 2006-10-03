; Tests for lib/symbol.sch
; 2002-03-18 / lth

(require 'symbol)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(let ((s "|| |a| b|c|d |abra cadabra ()|")
      (t (map string->symbol
	      '("" "a" "b" "c" "d" "abra cadabra ()"))))
  (or (equal? (with-input-from-port (open-input-string s)
		(lambda ()
		  (do ((x (read) (read))
		       (l '()    (cons x l)))
		      ((eof-object? x) (reverse l)))))
	      t)
      (fail 'symbol-syntax)))
