; This prints the information in machine-readable form, and makes
; nesting of calls to run-benchmark work sensibly.

(define done #f)

(define (run-benchmark name thunk . rest)
  (let ((n (if (null? rest) 1 (car rest))))
    
    (define (loop n)
      (if (zero? n)
	  #t
	  (begin (thunk)
		 (loop (- n 1)))))

    (if (not done)
	(begin 
	  (newline)
	  (newline)
	  (display "; **************************************************\n")
	  (display "; **************************************************\n")
	  (write (command-line-arguments)) (newline)
	  (set! done #t)))
    (newline)
    (display "(")
    (display "RUNNING ")
    (write name)
    (newline)
    (run-with-machine-readable-stats (lambda () (loop n)))
    (display ")")
    (newline)))

(define (run-with-machine-readable-stats thunk)
  
  (define (print-stats s1 s2)
    (display
     `((allocated   ,(- (vector-ref s2 0) (vector-ref s1 0)))
       (reclaimed   ,(- (vector-ref s2 1) (vector-ref s1 1)))
       (elapsed     ,(- (vector-ref s2 21) (vector-ref s1 21)))
       (user        ,(- (vector-ref s2 23) (vector-ref s1 23)))
       (system      ,(- (vector-ref s2 22) (vector-ref s1 22)))
       (gctime      ,(- (vector-ref s2 3) (vector-ref s1 3)))
       (currentheap ,(vector-ref s2 13))
       (maxheap     ,(vector-ref s2 27))
       ))
    (newline))

  (let* ((s1 (memstats))
	 (r  (thunk))
	 (s2 (memstats)))
    (print-stats s1 s2)
    r))

(error-handler
 (lambda args
   (display "ERROR: ")
   (for-each display args)
   (newline)
   (exit 1)))

