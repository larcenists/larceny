; The application "main" file; in this case for testing.
;
; $Id: main.sch,v 1.3 92/02/23 16:56:07 lth Exp $

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
  (display "1=fixnums, 2=flonums, 3=bignums, 4=strings, 5=ctest, 6=ctak, 0=quit: ")
  (flush-output-port)
  (let ((choice (read)))
    (case choice
      ((1) (fixtest) (numtest))
      ((2) (flotest) (numtest))
      ((3) (bigtest) (numtest))
      ((4) (strtest) (numtest))
      ((5) (ctest) (numtest))
      ((6) (let* ((t1 (getrusage))
		  (x (ctak 18 12 6))
		  (t2 (getrusage)))
	     (display "(ctak 18 12 6) = ")
	     (display x)
	     (display " in ")
	     (display (- t2 t1))
	     (display " milliseconds.")
	     (newline)
	     (numtest)))
      ((0) #t)
      (else (numtest)))))

; tests continuation stuff (I think).

(define (ctest)
  (ctest1)
  (ctest2))

; supposed to print "Now in ye procedure" followed by "hello, world"

(define (ctest1)
  (write 
   (call-with-current-continuation
    (lambda (k)
      (write "Now in ye procedure")
      (k "hello, world")
      "goodbye, world"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         ctak.sch
; Description:  The ctak benchmark
; Author:       Richard Gabriel
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:53:02 (Bob Shaw)
;               24-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The original version of this benchmark used a continuation mechanism that
; is less powerful than call-with-current-continuation and also relied on
; dynamic binding, which is not provided in standard Scheme.  Since the
; intent of the benchmark seemed to be to test non-local exits, the dynamic
; binding has been replaced here by lexical binding.

; For Scheme the comment that follows should read:
;;; CTAK -- A version of the TAK procedure that uses continuations.

;;; CTAK -- A version of the TAK function that uses the CATCH/THROW facility.

;;; call: (ctak 18 12 6)

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k)
     (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))			;xy
      (k z)
      (call-with-current-continuation
       (ctak-aux
	k
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- x 1)
		     y
		     z)))
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- y 1)
		     z
		     x)))
	(call-with-current-continuation
	 (lambda (k)
	   (ctak-aux k
		     (- z 1)
		     x
		     y)))))))

; jumps back in; supposed to print '33'.

(define (ctest2)

  (define (deep-and-hairy n)
    (if (zero? n)
	(call-with-current-continuation
	 (lambda (k)
	   (k k)))
	(deep-and-hairy (- n 1))))

  (let ((k (deep-and-hairy 10)))
    (if (not (procedure? k))
	(write "fum!")
	(write (k 33)))))

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
