;; Program which expands all macros in the input file.
;;
;; $Id$
;;
;; We use this to get rid of quasiquotations to help the current version of
;; twobit. To be used in emergencies only. :-)

(define (expand313 infn)
  (let ((outfn (if (and (>= (string-length infn) 4)
			(string=? (substring infn 
					     (- (string-length infn) 4)
					     (string-length infn)) 
				  ".raw"))
		   (string-append (substring infn 0 (- (string-length infn) 4))
				  ".sch")
		   (string-append infn ".sch"))))
    (call-with-input-file infn
      (lambda (inp)
	(delete-file outfn)
	(call-with-output-file outfn
	  (lambda (outp)
	    (let loop ((item (read inp)))
	      (if (eof-object? item)
		  #t
		  (begin (pretty-print (rewrite item) outp)
			 (newline outp)
			 (loop (read inp)))))))))))

; eof
