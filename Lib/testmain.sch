; Larceny
; Test cases
;
; lth@cs.uoregon.edu / updated 950526
;
; This file can be loaded dynamically iff the reader is ok, since there
; is all sorts of syntax in here. Perhaps better to dump as part of a heap;
; the procedure main is run as the main loop of that heap. There is a
; special clause in the scheme makefile, make-test-heap, which does more
; or less the right thing.

(define (main)
  (display "       1  generic arithmetic regression test") (newline)
  (display "       2  i/o system test") (newline)
  (display "       3  ctak test") (newline)
  (display "       4  reverse test") (newline)
  (display "       5  memory statistics") (newline)
  (display "       6  printer test") (newline)
  (display "       7  sort test") (newline)
  (display "       8  append test") (newline)
  (display "       9  ephemeral collection") (newline)
  (display "       a  character procedures test") (newline)
  (display "       0  exit") (newline)
  (newline)
  (display "       > ")
  (flush-output-port)
  (let ((choice (read)))
    (cond ((or (eof-object? choice) (eqv? choice 0))
	   #t)
	  ((eqv? choice 1) 
	   (gentest) (main))
	  ((eqv? choice 2)
	   (iotest) (main))
	  ((eqv? choice 3)
	   (display (run-with-stats (lambda () (ctak 18 12 6)))) (newline)
	   (main))
	  ((eqv? choice 4)
	   (display "tail-recursive: ") (newline)
	   (run-with-stats (lambda () (rtest1)))
	   (display "side-effecting: ") (newline)
	   (run-with-stats (lambda () (rtest2)))
	   (main))
	  ((eqv? choice 5)
	   (display-memstats (memstats)) (newline) (main))
	  ((eqv? choice 6)
	   (testprint)
	   (main))
	  ((eqv? choice 7)
	   (sorttest)
	   (main))
	  ((eqv? choice 8)
	   (append-test)
	   (main))
	  ((eqv? choice 9)
	   (gc 0)
	   (main))
	  ((eqv? choice 'a)
	   (char-tests)
	   (main))
	  (else
	   (main)))))

(define module-access (lambda (m n) (cadr (assq n m))))
(define (make-module x) x)

(define (rgen n)
  (let loop ((n n) (l '()))
    (if (zero? n) 
	l
	(loop (- n 1) (cons (random 2048) l)))))

; Requires the sorting code to be loaded.

(define (sorttest)
  (let ((complex-sort  (module-access sort-module 'complex-sort))
	(simple-sort   (module-access sort-module 'simple-sort))
	(sort!         (module-access sort-module 'sort!))
	(l             (rgen 16000)))
    (display "Gen")
    (newline)
    (run-with-stats
     (lambda () (ntimes 20 (lambda () (append l '())))))
    (display "Complex-Sort") 
    (newline)
    (run-with-stats
     (lambda () (ntimes 20 (lambda () (complex-sort (append l '()) <)))))
    (display "Simple-Sort") 
    (newline)
    (run-with-stats
     (lambda () (ntimes 20 (lambda () (simple-sort (append l '()) <)))))
    (display "Sort!")
    (newline)
    (run-with-stats
     (lambda () (ntimes 20 (lambda () (sort! (append l '()) <)))))))

(define (iotest)
  (display "i/o system test not implemented.") (newline))

(define (gen n)
  (if (zero? n) '() (cons n (gen (- n 1)))))

(define (ntimes n thunk)
  (if (zero? n) #t (begin (thunk) (ntimes (- n 1) thunk))))

(define (ntimes-res n proc initial)
  (if (zero? n) #t (ntimes-res (- n 1) proc (proc initial))))

(define (rtest1)
  (ntimes 1000 (let ((l (gen 1000))) (lambda () (reverse l)))))

(define (reverse! l)
  (let loop ((prev '()) (curr l))
    (if (null? curr)
	prev
	(let ((next (cdr curr)))
	  (set-cdr! curr prev)
	  (loop curr next)))))

(define (rtest2)
  (ntimes-res 1000 (lambda (x) (reverse! x)) (gen 1000)))

(define (append1 l1 l2)
  (if (null? l1) 
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append2 l1 l2)
  (if (not (null? l1))
      (let loop ((l1 l1) (prev #f) (head #f))
	(if (null? l1)
	    (begin (set-cdr! prev l2)
		   head)
	    (let ((q (cons (car l1) #f)))
	      (if prev
		  (begin (set-cdr! prev q)
			 (loop (cdr l1) q head))
		  (loop (cdr l1) q q)))))
      l2))

(define (append-test)
  (let ((l1 (gen 16000))
	(l2 '(foo)))
    (display "Recursive")
    (newline)
    (run-with-stats
     (lambda () (ntimes 200 (lambda () (append1 l1 '())))))
    (display "Iterative") 
    (newline)
    (run-with-stats
     (lambda () (ntimes 200 (lambda () (append2 l1 '())))))))


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

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

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


; (define (flotest)
;   (let ((a 2.0)
; 	(b -3.5))
;     (fpr a)
;     (fpr b)
;     (fpr (+ a b))
;     (fpr (* a a))
;     (fpr (/ a a))))
; 
; (define (fpr n)
;   (let ((s "0123456789abcdef"))
;     (if (not (flonum? n))
; 	(error "Not a flonum!"))
;     (let loop ((i 4))
;       (if (< i 12)
; 	  (let ((b (bytevector-like-ref n i)))
; 	    (write-char (string-ref s (quotient b 16)))
; 	    (write-char (string-ref s (remainder b 16)))
; 	    (loop (+ i 1)))
; 	  (newline)))))


; AUTOMATED TESTING STUFF.

; Generic test procedure; compares answer to expected answer.
; It requires that the answer and the expected answer are represented in
; a way which exercises only trusted procedures.

(define (test id ans correct)
  (if (not (equal? ans correct))
      (begin (display id) (display " did not pass test.") (newline)
	     (display "answer=") (display ans) (newline)
	     (display "correct=") (display correct) (newline)
	     #f)
      #t))

(define (allof . x)
  (let loop ((l x))
    (cond ((null? l) #t)
	  ((not (car l)) #f)
	  (else (loop (cdr l))))))


; Tests the generic arithmetic system in an automated fashion.
; Each subtest returns #t if all tests were passed, and #f if not.
;
; While there is a notion here that later tests depend on earlier tests, this
; is only partially so, as some innocent primitives invoke very heavy machinery
; indeed behind the programmer's back, and hence we have difficulty doing
; proper incremental testing.

(define (gentest)
  (and (begin (display "test 0") (newline)
	      (generic-arithmetic-test-0))
       (begin (display "test 1") (newline)
	      (generic-arithmetic-test-1))
       (begin (display "test 2") (newline)
	      (generic-arithmetic-test-2))
       (begin (display "test 3") (newline)
	      (predicate-test-0))
       (begin (display "test 4") (newline)
	      (generic-arithmetic-test-3))
       (begin (display "basic-arithmetic-test") (newline)
	      (basic-arithmetic-test))
       (begin (display "equality predicates") (newline)
	      (basic-generic-equality-test))
       (begin (display "basic-bignum-test") (newline)
	      (basic-bignum-arithmetic-test))
       (begin (display "test 5") (newline)
	      (generic-arithmetic-test-4))
       (begin (display "test 6") (newline)
	      (generic-arithmetic-test-5))
       ))

; We're trying to defeat the optimizations in the front end here. Really.

(define (basic-generic-equality-test)
  (let ((zero 0)
	(one 1) 
	(xone 1)
	(two 2)
	(a 'a)
	(b 'b))
    (allof
     (test "(eq? a 'a)" (eq? a 'a) #t)
     (test "(eq? a b)" (eq? a b) #f)
     (test "(eq? one xone)" (eq? one xone) #t)
     (test "(eq? zero one)" (eq? zero one) #f)
     (test "(eqv? a 'a)" (eqv? a 'a) #t)
     (test "(eqv? a b)" (eqv? a b) #f)
     (test "(eqv? one xone)" (eqv? one xone) #t)
     (test "(eqv? zero one)" (eqv? zero one) #f)
     (test "(equal? a 'a)" (equal? a 'a) #t)
     (test "(equal? a b)" (equal? a b) #f)
     (test "(equal? xone one)" (equal? xone one) #t)
     (test "(equal? zero one)" (equal? zero one) #f)
     )))

; Tests representation predicates; some are in millicode and some are not.

(define (generic-arithmetic-test-0)

  (define (rpred n)
    (list (fixnum? n)
	  (bignum? n)
	  (ratnum? n)
	  (flonum? n)
	  (compnum? n)
	  (rectnum? n)))

  (allof
   (test "(rpred 1)" (rpred 1) '(#t #f #f #f #f #f))
   (test "(rpred -1)" (rpred -1) '(#t #f #f #f #f #f))
   (test "(rpred 536870911)" (rpred 536870911) '(#t #f #f #f #f #f))
   (test "(rpred -536870912)" (rpred -536870912) '(#t #f #f #f #f #f))
   (test "(rpred 536870912)" (rpred 536870912) '(#f #t #f #f #f #f))
   (test "(rpred -536870913)" (rpred -536870913) '(#f #t #f #f #f #f))
   (test "(rpred 1234567890)" (rpred 1234567890) '(#f #t #f #f #f #f))
   (test "(rpred -1234567890)" (rpred -1234567890) '(#f #t #f #f #f #f))
   (test "(rpred 1122334455667788990011223344556677889900)"
	 (rpred 1122334455667788990011223344556677889900)
	 '(#f #t #f #f #f #f))
   (test "(rpred -1122334455667788990011223344556677889900)"
	 (rpred -1122334455667788990011223344556677889900)
	 '(#f #t #f #f #f #f))
   (test "(rpred 2/3)" (rpred 2/3) '(#f #f #t #f #f #f))
   (test "(rpred -2/3)" (rpred -2/3) '(#f #f #t #f #f #f))
   (test "(rpred 1/1234567890)" (rpred 1/1234567890) '(#f #f #t #f #f #f))
   (test "(rpred -1/1234567890)" (rpred -1/1234567890) '(#f #f #t #f #f #f))
   (test "(rpred 1.0)" (rpred 1.0) '(#f #f #f #t #f #f))
   (test "(rpred 1.01)" (rpred 1.01) '(#f #f #f #t #f #f))
   (test "(rpred -23.3333)" (rpred -23.3333) '(#f #f #f #t #f #f))
   (test "(rpred 1.0+1i)" (rpred 1.0+1i) '(#f #f #f #f #t #f))
   (test "(rpred 1+1.0i)" (rpred 1+1.0i) '(#f #f #f #f #t #f))
   (test "(rpred 1+1i)" (rpred 1+1i) '(#f #f #f #f #f #t))
   (test "(rpred 1-1i)" (rpred 1-1i) '(#f #f #f #f #f #t))
   (test "(rpred 1234567890+33i)" (rpred 1234567890+33i) '(#f #f #f #f #f #t))
   ))

; Tests the numeric tower classification predicates millicode.

(define (generic-arithmetic-test-1)

  (define (numberpred n)
    (list (integer? n) (rational? n) (real? n) (complex? n)))

  (allof
   ; fixnums
   (test "(numberpred 1)" (numberpred 1) '(#t #t #t #t))
   (test "(numberpred -1)" (numberpred -1) '(#t #t #t #t))
   (test "(numberpred 0)" (numberpred 0) '(#t #t #t #t))
					; largest fixnums
   (test "(numberpred 536870911)" (numberpred 536870911) '(#t #t #t #t))
   (test "(numberpred -536870912)" (numberpred -536870912) '(#t #t #t #t))
					; bignums
   (test "(numberpred 1234567890)" (numberpred 1234567890) '(#t #t #t #t))
   (test "(numberpred -22334455667788)" (numberpred -22334455667788)
	 '(#t #t #t #t))
   (test "(numberpred 536870912)" (numberpred 536870912) '(#t #t #t #t))
   (test "(numberpred -536870913)" (numberpred -536870913) '(#t #t #t #t))
   ; ratnums
   (test "(numberpred 2/3)" (numberpred 2/3) '(#f #t #t #t))
   (test "(numberpred -2/3)" (numberpred -2/3) '(#f #t #t #t))
   (test "(numberpred 1234567890/13)" (numberpred 1234567890/13) 
	 '(#f #t #t #t))
   (test "(numberpred -1234567890/13)" (numberpred -1234567890/13) 
   	 '(#f #t #t #t))
   ; flonums
   (test "(numberpred 1.0)" (numberpred 1.0) '(#t #t #t #t))
   (test "(numberpred -1.0)" (numberpred -1.0) '(#t #t #t #t))
   (test "(numberpred 1.01)" (numberpred 1.01) '(#f #t #t #t))
   (test "(numberpred -1.01)" (numberpred -1.01) '(#f #t #t #t))
   (test "(numberpred 1e53)" (numberpred 1e53) '(#t #t #t #t))
   (test "(numberpred 23.8765)" (numberpred 23.8765) '(#f #t #t #t))
   (test "(numberpred -23.8765)" (numberpred -23.8765) '(#f #t #t #t))
   ; compnums
   (test "(numberpred 1.0+1.0i)" (numberpred 1.0+1.0i) '(#f #f #f #t))
   ; rectnums
   (test "(numberpred 1+1i)" (numberpred 1+1i) '(#f #f #f #t))
   ; non-numbers
   (test "(numberpred \"foo!\")" (numberpred "foo!") '(#f #f #f #f))
   (test "(numberpred #\4)" (numberpred #\4) '(#f #f #f #f))
   (test "(numbrepred '(a))" (numberpred '(a)) '(#f #f #f #f))
   (test "(numberpred '#(1 2))" (numberpred '#(1 2)) '(#f #f #f #f))
   (test "(numberpred (make-bytevector 4))"
	 (numberpred (make-bytevector 4))
	 '(#f #f #f #f))
   (test "(numberpred '())" (numberpred '()) '(#f #f #f #f))
   (test "(numberpred #f)" (numberpred #f) '(#f #f #f #f))
   (test "(numberpred #t)" (numberpred #t) '(#f #f #f #f))
   (test "(numberpred 'fum)" (numberpred 'fum) '(#f #f #f #f))
   ))

; Test misc. other stuff in the generic arithmetic system.

(define (generic-arithmetic-test-2)

  (define (etest n)
    (list (exact? n) (inexact? n)))

  (allof
   ; constructors

   (test "(complex? (make-rectangular 1 1))"
	 (complex? (make-rectangular 1 1)) 
	 #t)

   ; exactness predicates

   (test "(etest 1)" (etest 1) '(#t #f))
   (test "(etest -1)" (etest -1) '(#t #f))
   (test "(etest 1234567890)" (etest 1234567890) '(#t #f))
   (test "(etest -1234567890)" (etest -1234567890) '(#t #f))
   (test "(etest 2/3)" (etest 2/3) '(#t #f))
   (test "(etest 1234567890/13)" (etest 1234567890/13) '(#t #f))
   (test "(etest 1.0)" (etest 1.0) '(#f #t))
   (test "(etest -1.0)" (etest -1.0) '(#f #t))
   (test "(etest 1.0+3i)" (etest 1.0+3i) '(#f #t))
   (test "(etest 1+1i)" (etest 1+1i) '(#t #f))
   ))

; Tests eq? and eqv?, first time around (basic stuff, no numbers.)

(define (predicate-test-0)

  (define (e a b)
    (list (eq? a b) (eqv? a b)))

  (let ((a "string1")
	(b (lambda (x) x)))
    (allof
     (test "(e '() '())" (e '() '()) '(#t #t))
     (test "(e #t #f)" (e #t #f) '(#f #f))
     (test "(e a a)" (e a a) '(#t #t))
     (test "(e b b)" (e b b) '(#t #t))
     (test "(e 'foo 'foo)" (e 'foo 'foo) '(#t #t))
     )))

; Tests comparison predicates.

(define (generic-arithmetic-test-3)

  (define (p m n)
    (list (= m n) (> m n) (< m n) (>= m n) (<= m n)))

  (define (q m)
    (list (zero? m) (negative? m) (positive? m)))

  (allof
   (test "(p 1 1)" (p 1 1) '(#t #f #f #t #t))
   (test "(p -1 -1)" (p -1 -1) '(#t #f #f #t #t))
   (test "(p 1 -1)" (p 1 -1) '(#f #t #f #t #f))
   (test "(p -1 1)" (p -1 1) '(#f #f #t #f #t))
   (test "(p 1234567890 1234567890)" 
	 (p 1234567890 1234567890) 
	 '(#t #f #f #t #t))
   (test "(p -12345678901234567890 -12345678901234567890)"
	 (p -12345678901234567890 -12345678901234567890)
	 '(#t #f #f #t #t))
   (test "(p 1234567890123 -1234567890987654321)"
	 (p 1234567890123 -1234567890987654321)
	 '(#f #t #f #t #f))
   (test "(p 2233445566 12345678901234567890)"
	 (p 2233445566 12345678901234567890)
	 '(#f #f #t #f #t))
   (test "(= 2/3 2/3)" (= 2/3 2/3) #t)
   (test "(= 1/3 2/3)" (= 1/3 2/3) #f)
   (test "(p 2/3 2/3)" (p 2/3 2/3) '(#t #f #f #t #t))
   (test "(p 1.0 1.0)" (p 1.0 1.0) '(#t #f #f #t #t))
   (test "(p 1.01 1.01)" (p 1.01 1.01) '(#t #f #f #t #t))
   (test "(p -1.5 -1.5)" (p -1.5 -1.5) '(#t #f #f #t #t))
   (test "(p -1.01 1.01)" (p -1.01 1.01) '(#f #f #t #f #t))
   (test "(p 1.01 -1.01)" (p 1.01 -1.01) '(#f #t #f #t #f))
   (test "(= 1.5+3i 1.5+3i)" (= 1.5+3i 1.5+3i) #t)
   (test "(= 1.5+4i 1.5+3i)" (= 1.5+4i 1.5+3i) #f)
   (test "(q 0)" (q 0) '(#t #f #f))
   (test "(q 1)" (q 1) '(#f #f #t))
   (test "(q -1)" (q -1) '(#f #t #f))
   (test "(= 0.0 -0.0)" (= 0.0 -0.0) #t)
   ))

; Difficulter stuff.

(define (generic-arithmetic-test-4)

  (define (q m)
    (list (zero? m) (negative? m) (positive? m)))

  (allof
   ; need harder tests to make sure it rounds to even.
   (test "(= (round 1.4) 1.0)" (= (round 1.4) 1.0) #t)
   (test "(= (round 1.0) 1.0)" (= (round 1.0) 1.0) #t)
   (test "(= (round 1.4) 1.4)" (= (round 1.4) 1.4) #f)
   (test "(= (round -1.5) -1.0)" (= (round -1.5) -1.0) #f)
   (test "(= (round 1.5) 2.0)" (= (round 1.5) 2.0) #t)
   (test "(= (round -1.5) -2.0)" (= (round -1.5) -2.0) #t)

   (test "(= (truncate 1.5) 1.0)" (= (truncate 1.5) 1.0) #t)
   (test "(= (truncate 1.0) 1.0)" (= (truncate 1.0) 1.0) #t)
   (test "(= (truncate -1.5) -1.0)" (= (truncate -1.5) -1.0) #t)

   (test "(= (logior 3 5) 7)" (= (logior 3 5) 7) #t)
   (test "(= (logand #x33 #x55) #x11)" (= (logand #x33 #x55) #x11) #t)
   (test "(= (lsh #x44 2) #x110)" (= (lsh #x44 2) #x110) #t)
   (test "(= (lsh #x44 4) #x440)" (= (lsh #x44 4) #x440) #t)
   (test "(= (rshl #x44 2) #x11)" (= (rshl #x44 2) #x11) #t)
   (test "(= (rshl #x44 7) 0)" (= (rshl #x44 7) 0) #t)
   (test "(= (rsha -1 4) -1)" (= (rsha -1 4) -1) #t)
   (test "(= (rshl #x-20000000 4) #x2000000)"
	 (= (rshl #x-20000000 4) #x2000000)
	 #t)
; Test 'lognot' also.
; Test 'logxor' also.
; Test 'rot' also.
   ))


; Much of this is preempted by the fact that we are able to run this 
; test program, but is here for the sake of providing a fire wall.

(define (basic-arithmetic-test)

  (define (n1 x y)
    (list (+ x y) (- x y) (* x y)))

  (let* ((a 12345)
	 (-a (- a))
	 (b 3145)
	 (-b (- b))
	 (q (lambda (x) (- x)))
	 (two^28 268435456)
	 (two^29 536870912)
	 (two^30 1073741824)
	 (two^31 2147483648)
	 (two^32-1 4294967295))
    (allof
     (test "(- a)" (- a) -a)
     (test "(- -a)" (- -a) a)

     ; these multiply fixnums, producing fixnums

     (test "(* 30 40)" (* 30 40) 1200)
     (test "(* 30 -40)" (* 30 -40) -1200)
     (test "(* -40 -30)" (* -40 -30) 1200)

     ; simple quotients

     (test "(quotient a b)" (quotient a b) 3)
     (test "(quotient b a)" (quotient b a) 0)
     (test "(quotient a a)" (quotient a a) 1)
     (test "(quotient a -a)" (quotient a -a) -1)
     (test "(quotient -a a)" (quotient -a a) -1)
     (test "(quotient -a -a)" (quotient -a -a) 1)
     (test "(quotient 63888 65536)" (quotient 63888 65536) 0)

     ; simple remainders

     (test "(remainder a b)" (remainder a b) 2910)
     (test "(remainder b a)" (remainder b a) b)
     (test "(remainder a a)" (remainder a a) 0)
     (test "(remainder b -a)" (remainder b -a) b)
     (test "(remainder -b a)" (remainder -b a) -b)
     (test "(remainder a -b)" (remainder a -b) 2910)
     (test "(remainder -a b)" (remainder -a b) -2910)
     (test "(remainder 63888 65536)" (remainder 63888 65536) 63888)

     ; these generate 1-word bignums from fixnums

     (test "(* 65536 65535)" (* 65536 65535) 4294901760)
     (test "(* -65536 65535)" (* -65536 65535) -4294901760)

     ; these generate 2-word bignums from fixnums

     (test "(* 268435456 268435455)" (* 268435456 268435455) 
	   72057593769492480)
     (test "(* 268435456 -268435455)" (* 268435456 -268435455) 
	   -72057593769492480)
     (test "(* -268435456 -268435455)" (* -268435456 -268435455) 
	   72057593769492480)
     (test "(* 536870911 536870911)" (* 536870911 536870911) 
	   288230375077969921)     
     (test "(* -536870911 536870911)" (* -536870911 536870911) 
	   -288230375077969921)
     (test "(* -536870911 -536870911)" (* -536870911 -536870911) 
	   288230375077969921)

     ; Belongs in bignum tests, but are necessary to report errors here.

     (test "(- 288230375077969921)" 
	   (q 288230375077969921)
	   -288230375077969921)
     (test "(- -288230375077969921)" 
	   (q -288230375077969921)
	   288230375077969921)

     ; some bignums will be generated from fixnums

     (test "(n1 536870911 1)" (n1 536870911 1)
	   '(536870912 536870910 536870911))
     (test "(n1 536870911 2)" (n1 536870911 2) 
	   '(536870913 536870909 1073741822))
     (test "(n1 2 536870911)" (n1 2 536870911) 
	   '(536870913 -536870909 1073741822))
     (test "(n1 536870911 536870911)" (n1 536870911 536870911)
	   '(1073741822 0 288230375077969921))
     (test "(n1 -536870911 536870911)" (n1 -536870911 536870911)
	   '(0 -1073741822 -288230375077969921))

     ; 32-bit unsigned-bignum-by-unsigned-fixnum division (in millicode)

     (test "(quotient 2^29 10)" (quotient two^29 10) 53687091)
     (test "(quotient 2^30 10)" (quotient two^30 10) 107374182)
     (test "(quotient 2^31 10)" (quotient two^31 10) 214748364)
     (test "(quotient 2^32-1 10)" (quotient two^32-1 10) 429496729)
     )))

(define (basic-bignum-arithmetic-test)
  (let ((a 1234567890)
	(b 3141598765)
	(add (lambda (a b) (+ a b)))
	(sub (lambda (a b) (- a b)))
	(mul (lambda (a b) (* a b)))
	(div (lambda (a b) (quotient a b)))
	(mod (lambda (a b) (remainder a b))))
    (begin

     (test "(string=? (number->string 1234567890 10) \"1234567890\")"
	   (string=? (number->string 1234567890 10) "1234567890")
	   #t)

     (display "add/sub") (newline)
     (test "(bignum-add a b)" (add a b) 4376166655)
     (test "(bignum-add b a)" (add b a) 4376166655)
     (test "(bignum-subtract a b)" (sub a b) -1907030875)
     (test "(bignum-subtract b a)" (sub b a) 1907030875)
     (test "(bignum-subtract a a)" (sub a a) 0)

     ; tests contagion: fixnum * bignum

     (display "fix * big") (newline)
     (test "(* 42 a)" (mul 42 a) 51851851380)
     (test "(* a 42)" (mul a 42) 51851851380)
     (test "(* -42 a)" (mul -42 a) -51851851380)
     (test "(* a -42)" (mul a -42) -51851851380)

     ; heavier stuff (already bignums)

     (display "bignum multiply") (newline)
     (test "(bignum-multiply a a)" (mul a a) 1524157875019052100)
     (test "(bignum-multiply b b)" (mul b b) 9869642800249525225)
     (test "(bignum-multiply a b)" (mul a b) 3878516958532655850)
     (test "(bignum-multiply b a)" (mul b a) 3878516958532655850)

     (display "bignum division") (newline)
     (test "(bignum-quotient a b)" (div a b) 0)
     (test "(bignum-quotient b a)" (div b a) 2)
     (test "(bignum-quotient a a)" (div a a) 1)

     (test "(bignum-remainder a b)" (mod a b) a)
     (test "(bignum-remainder b a)" (mod b a) 672462985)
     (test "(bignum-remainder a a)" (mod a a) 0)

     )))

; Tests inexact->exact, exact->inexact, and some related stuff.
 
(define (generic-arithmetic-test-5)

  (define (q m)
    (list (zero? m) (negative? m) (positive? m)))

  (allof
   (test "(= 0.0 (exact->inexact 0))" (= 0.0 (exact->inexact 0)) #t)
   (test "(= 1.0 (exact->inexact 1))" (= 1.0 (exact->inexact 1)) #t)
   (test "(= 0 (inexact->exact 0.0))" (= 0 (inexact->exact 0.0)) #t)
   (test "(= 1 (inexact->exact 1.0))" (= 1 (inexact->exact 1.0)) #t)

   ; these will use contagion because negative? and positive? are 
   ; expanded to comparisons with zero.

   (test "(q 0.0)" (q 0.0) '(#t #f #f))
   (test "(q -0.0)" (q -0.0) '(#t #f #f))
   (test "(q 10.5)" (q 10.5) '(#f #f #t))
   (test "(q 12345678901234567890)" (q 12345678901234567890) '(#f #f #t))
   (test "(q -12345678901234567890)" (q -12345678901234567890) '(#f #t #f))
   ))


; Character predicate tests.

(define (char-tests)
  (and (begin (display "char test 0") (newline)
	      (char-test-0))
       (begin (display "char test 1") (newline)
	      (char-test-1 #\a))
       (begin (display "char test 2") (newline)
	      (char-test-2))
       (begin (display "char test 3") (newline)
	      (char-test-3 #\a))
       (begin (display "char test 4") (newline)
	      (char-test-4 #\a))))

; Simple character comparisons (both constants)

(define (char-test-0)
  (allof
   (test "(char=? #\a #\a)" (char=? #\a #\a) #t)
   (test "(char=? #\a #\b)" (char=? #\a #\b) #f)
   (test "(char>? #\a #\a)" (char>? #\a #\a) #f)
   (test "(char>? #\a #\b)" (char>? #\a #\b) #f)
   (test "(char>? #\b #\a)" (char>? #\b #\a) #t)
   (test "(char>=? #\a #\a)" (char>=? #\a #\a) #t)
   (test "(char>=? #\a #\b)" (char>=? #\a #\b) #f)
   (test "(char>=? #\b #\a)" (char>=? #\b #\a) #t)
   (test "(char<? #\a #\a)" (char<? #\a #\a) #f)
   (test "(char<? #\a #\b)" (char<? #\a #\b) #t)
   (test "(char<? #\b #\a)" (char<? #\b #\a) #f)
   (test "(char<=? #\a #\a)" (char<=? #\a #\a) #t)
   (test "(char<=? #\a #\b)" (char<=? #\a #\b) #t)
   (test "(char<=? #\b #\a)" (char<=? #\b #\a) #f)))

; Simple character comparisons (one constant, one variable)
; X is known to have value #\a.

(define (char-test-1 x)
  (allof
   (test "(char=? #\a x)" (char=? #\a x) #t)
   (test "(char=? #\a x)" (char=? #\b x) #f)
   (test "(char>? #\a x)" (char>? #\a x) #f)
   (test "(char>? #\b x)" (char>? #\b x) #t)
   (test "(char>=? #\a x)" (char>=? #\a x) #t)
   (test "(char>=? #\b x)" (char>=? #\b x) #t)
   (test "(char<? #\a x)" (char<? #\a x) #f)
   (test "(char<? #\b x)" (char<? #\b x) #f)
   (test "(char<=? #\a x)" (char<=? #\a x) #t)
   (test "(char<=? #\b x)" (char<=? #\b x) #f)))

; Character comparison for control, very basic (the compiler should
; really be able to grok these, so peephole opt. should kick in).
; (If the compiler is too smart and does constant folding, we lose.)

(define (char-test-2)
  (allof
   (test "(if (char=? #\a #\b) 0 1)" (if (char=? #\a #\b) 0 1) 1)
   (test "(if (char=? #\a #\a) 0 1)" (if (char=? #\a #\a) 0 1) 0)
   (test "(if (char>? #\a #\b) 0 1)" (if (char>? #\a #\b) 0 1) 1)
   (test "(if (char>? #\a #\a) 0 1)" (if (char>? #\a #\a) 0 1) 1)
   (test "(if (char>? #\b #\a) 0 1)" (if (char>? #\b #\a) 0 1) 0)
   (test "(if (char>=? #\a #\b) 0 1)" (if (char>=? #\a #\b) 0 1) 1)
   (test "(if (char>=? #\a #\a) 0 1)" (if (char>=? #\a #\a) 0 1) 0)
   (test "(if (char>=? #\b #\a) 0 1)" (if (char>=? #\b #\a) 0 1) 0)
   (test "(if (char<? #\a #\b) 0 1)" (if (char<? #\a #\b) 0 1) 0)
   (test "(if (char<? #\a #\a) 0 1)" (if (char<? #\a #\a) 0 1) 1)
   (test "(if (char<? #\b #\a) 0 1)" (if (char<? #\b #\a) 0 1) 1)
   (test "(if (char<=? #\a #\b) 0 1)" (if (char<=? #\a #\b) 0 1) 0)
   (test "(if (char<=? #\a #\a) 0 1)" (if (char<=? #\a #\a) 0 1) 0)
   (test "(if (char<=? #\b #\a) 0 1)" (if (char<=? #\b #\a) 0 1) 1)))

; More test for control, with one variable.
; x is known to be #\a

(define (char-test-3 x)
  (allof
   (test "(if (char=? x #\b) 0 1)" (if (char=? x #\b) 0 1) 1)
   (test "(if (char=? x #\a) 0 1)" (if (char=? x #\a) 0 1) 0)
   (test "(if (char>? x #\b) 0 1)" (if (char>? x #\b) 0 1) 1)
   (test "(if (char>? x #\a) 0 1)" (if (char>? x #\a) 0 1) 1)
   (test "(if (char>? #\b x) 0 1)" (if (char>? #\b x) 0 1) 0)
   (test "(if (char>=? x #\b) 0 1)" (if (char>=? x #\b) 0 1) 1)
   (test "(if (char>=? x #\a) 0 1)" (if (char>=? x #\a) 0 1) 0)
   (test "(if (char>=? #\b x) 0 1)" (if (char>=? #\b x) 0 1) 0)
   (test "(if (char<? x #\b) 0 1)" (if (char<? x #\b) 0 1) 0)
   (test "(if (char<? x #\a) 0 1)" (if (char<? x #\a) 0 1) 1)
   (test "(if (char<? #\b x) 0 1)" (if (char<? #\b x) 0 1) 1)
   (test "(if (char<=? x #\b) 0 1)" (if (char<=? x #\b) 0 1) 0)
   (test "(if (char<=? x #\a) 0 1)" (if (char<=? x #\a) 0 1) 0)
   (test "(if (char<=? #\b x) 0 1)" (if (char<=? #\b x) 0 1) 1)))

; x is known to be #\a

(define (char-test-4 x)
  (allof
   (test "(= (char->integer x) 97)" (= (char->integer x) 97) #t)
   (test "(char=? (integer->char (char->integer x)) x)"
	 (char=? (integer->char (char->integer x)) x)
	 #t)))


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

; eof
