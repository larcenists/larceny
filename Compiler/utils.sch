(define (t . f)
  (if (null? f)
      (disassemble313 "/tmp/larceny")
      (disassemble313 "/tmp/larceny" (car f))))

(define (readify-file f . o)

  (define (doit)
    (let ((i (open-input-file f)))
      (let loop ((x (read i)))
	(if (not (eof-object? x))
	    (begin (pretty-print (readify-lap x))
		   (loop (read i)))))))

  (if (null? o)
      (doit)
      (begin (delete-file (car o))
	     (with-output-to-file (car o) doit))))


(define (redo . f)
  (for-each 
   (lambda (f)
     (compile313 (string-append f))
     (assemble313 (string-append (substring f 
					    0 
					    (- (string-length f) 4)) ".lap")))
   f)
  (dump-the-heap "../yow.heap"))

