; The application "main" file; in this case for testing.
;
; $Id: main.sch,v 1.1 92/02/10 11:26:46 lth Exp Locker: lth $

(define (main)
  ;(runfib)
  (numtest))

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
    (if (not (eof-object? item))
	(begin (write item)
	       (newline)
	       (display "$ ")
	       (flush-output-port)
	       (loop (read))))))

(define (numtest)
  (display "1=fixnums, 2=flonums, 3=bignums, 4=strings, 0=quit: ")
  (flush-output-port)
  (let ((choice (read)))
    (case choice
      ((1) (fixtest) (numtest))
      ((2) (flotest) (numtest))
      ((3) (bigtest) (numtest))
      ((4) (strtest) (numtest))
      ((0) #t)
      (else (numtest)))))

(define (strtest)
  (display "Enter two quoted strings") (newline)
  (let* ((s1 (read))
	 (s2 (read)))
    (display "=    ") (display (string=? s1 s2)) (newline)
    (display ">=   ") (display (string>=? s1 s2)) (newline)
    (display "<=   ") (display (string<=? s1 s2)) (newline)
    (display ">    ") (display (string>? s1 s2)) (newline)
    (display "<    ") (display (string<? s1 s2)) (newline)
    (display "ci=  ") (display (string-ci=? s1 s2)) (newline)
    (display "ci<= ") (display (string-ci<=? s1 s2)) (newline)
    (display "ci>= ") (display (string-ci>=? s1 s2)) (newline)
    (display "ci<  ") (display (string-ci<? s1 s2)) (newline)
    (display "ci>  ") (display (string-ci>? s1 s2)) (newline)))


(define (fixtest)
  (write (modulo 13 4)) (newline)
  (write (remainder 13 4)) (newline)
  (write (modulo -13 4)) (newline)
  (write (remainder -13 4)) (newline)
  (write (modulo 13 -4)) (newline)
  (write (remainder 13 -4)) (newline)
  (write (modulo -13 -4)) (newline)
  (write (remainder -13 -4)) (newline)) 

(define (bigtest)
  (display "Enter two bignums") (newline)
  (let ((b1 (read))
	(b2 (read)))
    (if (not (and (bignum? b1) (bignum? b2)))
	(begin (display ">sigh<")
	       (newline)
	       (bigtest))
	(begin (display "+ ") (display (+ b1 b2)) (newline)
	       (display "- ") (display (- b1 b2)) (newline)
	       (display "* ") (display (* b1 b2)) (newline)
	       (display "quot ") (display (bignum-quotient b1 b2)) (newline)
	       (display "rem ") (display (bignum-remainder b1 b2)) (newline)
	       (display "/ ") (display "not yet") (newline)
	       (display "mod ") (display "not yet") (newline)
	       (display "= ") (display (= b1 b2)) (newline)
	       (display "< ") (display (< b1 b2)) (newline)
	       (display "> ") (display (> b1 b2)) (newline)
	       (display "<= ") (display (<= b1 b2)) (newline)
	       (display ">= ") (display (>= b1 b2)) (newline)
	       (display "(the following for the 1st # only") (newline)
	       (display "exact? ") (display (exact? b1)) (newline)
	       (display "inexact? ") (display (inexact? b1)) (newline)
	       (display "zero? ") (display (zero? b1)) (newline)
	       (display "abs ") (display (abs b1)) (newline)
	       ))))

(define (flotest)
  (let ((a 2.0)
	(b -3.5))
    (fpr a)
    (fpr b)
    (fpr (+ a b))
    (fpr (* a a))
    (fpr (/ a a))))

(define (fpr n)
  (break)
  (let ((s "0123456789abcdef"))
    (if (not (flonum? n))
	(error "Not a flonum!"))
    (let loop ((i 4))
      (if (< i 12)
	  (let ((b (bytevector-like-ref n i)))
	    (write-char (string-ref s (quotient b 16)))
	    (write-char (string-ref s (remainder b 16)))
	    (loop (+ i 1)))
	  (newline)))))
