; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test cases for fixnum-specific primitives.  

(define (run-fixnum-tests . rest)
  (display "Fixnums") (newline)
  (fixnum-test-functionality)
  (if (null? rest)
      (fixnum-test-error)
      #t)
  )

(define (fixnum-test-functionality)
  (let ((x '((0
	      0
	      (#t #f #f 1 -1 0 0 #t #f #t #f #t #t #f #t #f #t
		  0 0 536870911 -536870912)
	      (0 1 1 0 1 0 1 0 0 1 0 1 0))
	     (-10
	      10
	      (#f #t #f -9 -11 0 -20 #f #t #t #f #f #f #t #t #f #f
		  -10 -100 536870911 -536870912)
	      (1 0 1 1 0 0 1 1 1 0 0 1 1))
	     (10
	      -10
	      (#f #f #t 11 9 0 20 #f #f #f #t #t #f #f #f #t #t
		  10 -100 536870911 -536870912)
	      (1 1 0 1 1 1 0 0 1 1 1 0 0)))))
    (allof-map
     "Fixnum functionality." 
     (lambda (x)
       (let ((k (car x))
	     (l (cadr x))
	     (correct (cddr x)))
	 (let ((res (fixnum-tester k l)))
	   (if (not (equal? correct res))
	       (begin (failure-message-failed (list "fixnum test" k l) 
					      res 
					      correct)
		      #f)
	       #t))))
     x)))

(define (fixnum-tester k l)

  ; For result.

  (define (test1)
    (list (fxzero? k)
	  (fxnegative? k)
	  (fxpositive? k)
	  (fx+ k 1)
	  (fx- k 1)
	  (fx+ k l)
	  (fx- k l)
	  (fx= k 0)
	  (fx< k 0)
	  (fx<= k 0)
	  (fx> k 0)
	  (fx>= k 0)
	  (fx= k l)
	  (fx< k l)
	  (fx<= k l)
	  (fx> k l)
	  (fx>= k l)
	  (fx* k 1)
	  (fx* k l)
	  (most-positive-fixnum)
	  (most-negative-fixnum)))

  ; For control.

  (define (test2)
    (list (if (fxzero? k) 0 1)
	  (if (fxnegative? k) 0 1)
	  (if (fxpositive? k) 0 1)
	  (if (fx= k 0) 0 1)
	  (if (fx< k 0) 0 1)
	  (if (fx<= k 0) 0 1)
	  (if (fx> k 0) 0 1)
	  (if (fx>= k 0) 0 1)
	  (if (fx= k l) 0 1)
	  (if (fx< k l) 0 1)
	  (if (fx<= k l) 0 1)
	  (if (fx> k l) 0 1)
	  (if (fx>= k l) 0 1)))

  (list (test1) (test2)))

(define (fixnum-test-error)

  (define fxadd (lambda (a b) (fx+ a b)))
  (define fxsub (lambda (a b) (fx- a b)))
  (define fxmul (lambda (a b) (fx* a b)))
  (define fxneg (lambda (a) (fx-- a)))
  
  (allof "Fixnum error cases"
   (mustfail "add#1" fxadd 10 1.5)	          ; 1.5 is not a fixnum
   (mustfail "add#2" fxadd 413414123 123456789)   ; barely overflows (positive)
   (mustfail "add#3" fxadd -413414124 -123456789) ; barely overflows (negative)

   (mustfail "sub#1" fxsub 10 1.5)	          ; 1.5 is not a fixnum
   (mustfail "sub#2" fxsub -413414124 123456789)  ; barely overflows (negative)
   (mustfail "sub#3" fxsub 413414123 -123456789)  ; barely overflows (positive)

   (mustfail "neg#1" fxneg -536870912)            ; barely overflows (positive)
   (mustfail "neg#2" fxneg 1.5)                   ; 1.5 is not a fixnum

   (mustfail "mul#1" fxmul 10 1.5)	          ; 1.5 is not a fixnum
   (mustfail "mul#2" fxmul 524288 1024)	          ; barely overflows (positive)
   (mustfail "mul#2" fxmul 524288 -1025)	  ; overflows (negative)
   ))

; eof
