(define (compare-lop-files f1 f2)
  (let ((p1 (open-input-file f1))
	(p2 (open-input-file f2)))
    (do ((i 0 (+ i 1))
	 (a (read p1) (read p1))
	 (b (read p2) (read p2)))
	((or (eof-object? a) (eof-object? b)))
      (if (not (equal? a b))
	  (begin
	    (display "Files differ at item #")
	    (display i)
	    (newline))))))

; eof


	 