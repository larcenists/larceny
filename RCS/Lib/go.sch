; the `go' procedure for testing the printer

(define (go symlist)
  (display "installing millicode support") (newline)
  (install-millicode-support)
  (display "installing symbol table") (newline)
  (install-symbols symlist 521)
;  (testprint)
  (display "installing reader") (newline)
  (install-reader)
  (display "done") (newline)
  (runfib)
;  (testread)
  ; need to flush all output ports here (or "exit" should do that.)
  (exit))

(define (testprint)
  (write-char #\@) (write-char #\newline)
  (write "Hello, world.") (newline)              ; string
  (write 'hello) (newline)                       ; symbol
  (write '()) (newline)                          ; null
  (write #t) (newline)                           ; bool
  (write #f) (newline)                           ; ditto
  (write '(hello world)) (newline)               ; proper list
  (write '(hello . world)) (newline)             ; pair
  (write '#(hello world)) (newline)              ; vector
  (write (make-bytevector 5)) (newline)          ; bytevector
  (write (make-vector 10 #t)) (newline)          ; vector again
  (write (current-output-port)) (newline)        ; port
  (write testprint) (newline)                    ; procedure
  (write 4) (newline)                            ; single-digit fixnum
  (write 37) (newline)                           ; multi-digit fixnum

  )

(define (runfib)
  (let loop ()
    (display "Enter a positive number for 'fib'; 0 to quit; -1 for 'tak': ")
    (flush-output-port)
    (let ((i (read)))
      (cond ((> i 0)
	     (let* ((t1 (getrusage))
		    (f  (fib i))
		    (t2 (getrusage)))
	       (display "(fib ") 
	       (display i)
	       (display ") = ")
	       (display f)
	       (display " in ")
	       (display (- t2 t1))
	       (display " milliseconds.")
	       (newline)
	       (loop)))
	    ((< i 0)
	     (let* ((t1 (getrusage))
		    (t  (tak 18 12 6))
		    (t2 (getrusage)))
	       (display "(tak 18 12 6) = ")
	       (display t)
	       (display " in ")
	       (display (- t2 t1))
	       (display " milliseconds.")
	       (newline)
	       (loop)))
	    (else
	     (display "Bye.")
	     (newline))))))

; (define (fib n)
;   (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (tak x y z)
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(define (testread)
;  (let loop ((c (read-char)))
;    (write c)
;    (break)
;    (if (not (char=? c #\newline))
;	(loop (read-char))))
;  (display "$")
;  (newline)
  (display "$ ")
  (flush-output-port)
  (let loop ((item (read)))
    (break)
    (if (not (eof-object? item))
	(begin (write item)
	       (newline)
	       (display "$ ")
	       (flush-output-port)
	       (loop (read))))))


