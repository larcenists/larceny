; Fixme: this needs to install a global-name-resolver that mimics
; the real one but does not in fact link.

(define (compare-fasl-files f1 f2)
  (let ((old-resolver (global-name-resolver)))
    (dynamic-wind
     (lambda ()
       (global-name-resolver (lambda (sym) sym)))
     (lambda ()
       (let ((p1 (open-input-file f1))
	     (p2 (open-input-file f2)))
	 (do ((i 0 (+ i 1))
	      (a (read p1) (read p1))
	      (b (read p2) (read p2)))
	     ((or (and (eof-object? a) (eof-object? b))
		  (not (equal? a b)))
	      (if (not (equal? a b))
		  (begin
		    (display "Files differ at item #")
		    (display i)
		    (newline))
		  (begin
		    (display "Files are equivalent.")
		    (newline)))))))
     (lambda ()
       (global-name-resolver old-resolver)))))

; eof


	 