; Test suite for SRFI-37
; 2004-01-02 / lth

(cond-expand (srfi-37))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define (optproc opt name args s1 s2)
  (values (cons (cons name args) s1) s2))

; Probably does not test nearly everything but at least provides
; an example of how the package can be used...

(let-values (((options args)
	      (args-fold '("-a" "--help" "-b" "barf" "--" "more" "trouble")
			 (list (option '(#\a)    #f #t optproc)
			       (option '("help") #f #f optproc)
			       (option '(#\b)    #t #t optproc))
			 (lambda args (writeln "oops " args))
			 (lambda (x s1 s2) 
			   (values s1 (append s2 (list x))))
			 '()
			 '())))
  (or (let ((probe (assv #\a options)))
	(and probe
	     (not (cdr probe))))
      (fail 'option-a))
  (or (let ((probe (assoc "help" options)))
	(and probe
	     (not (cdr probe))))
      (fail 'option-help))
  (or (let ((probe (assv #\b options)))
	(and probe
	     (equal? (cdr probe) "barf")))
      (fail 'option-b))
  (or (equal? '("more" "trouble") args)
      (fail 'arguments)))

(writeln "Done.")
