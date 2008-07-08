;; A MaybeReadResult is one of:
;; - (list 'sexp Sexp Nat)  :  read N chars to create S
;; - (list 'eof)            :  reached end of file before whole Sexp
;; - (list 'error Any)      :  error while attempting to read

;; maybe-read : TextualPort -> Maybe
(define (maybe-read orig-port)
  (call-with-current-continuation
   (lambda (escape)
     (let* ((idx 0)
	    (read! (lambda (output-string start count)
		     (let ((c (get-char orig-port)))
		       (cond ((eof-object? c) (escape '(eof)))
			     (else (set! idx (+ idx 1))
				   (string-set! output-string start c)
				   1)))))
	    (custom-textual-port
	     (make-custom-textual-input-port "REPL PORT" read! #f #f #f))
	    (read-result
	     (call-with-error-handler 
	      (lambda args (escape `(error ,args)))
	      (lambda () (read custom-textual-port)))))
       `(sexp ,read-result ,idx)))))
