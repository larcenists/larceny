; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Past error cases are tested here.
;
; FIXME:  If the compiler is sufficiently clever, some of these cases
; may not test anything but the host system.

(define (hide x) x)

(define (run-regression-tests)
  (display "Regression") (newline)
  (allof "Past error cases"
   (test "Error case #1"		; Bug 060
	 (= (+ 1 (make-rectangular (expt 2 100) 1))
	    (make-rectangular (expt 2.0 100) 1.0))
	 #f)
   (test "Error case #2"		; Bug 060
	 (- (+ 1 (make-rectangular (expt 2 100) 1))
	    (make-rectangular (expt 2.0 100) 1.0))
	 1.0)
   (test "Error case #3"		; Bug 073
	 (let ((a (string->number
		   (string-append "#b" (number->string (sqrt 2) 2))))
	       (b (sqrt 2)))
	   (= a b))
	 #t)
   (test "Error case #4"		; Bug 058
	 (zero? (- (expt 2. 100) (+ (expt 2 100) 1)))
	 #f)
   (test "Error case #5"		; Bug 058
	 (- (expt 2.0 100) (+ (expt 2 100) 1))
	 -1.0)
   (test "Error case #6"		; Bug 007
	 (number->string -0.0)
	 "-0.0")
   (test "Error case #7"		; Bug 038
	 (- (expt 2 29))
	 -536870912)
   (test "Error case #8"		; Bug 061
	 (fxlogand -536870912 1)
	 0)
   (test "Error case #9"		; Bug 066
	 (exact->inexact 14285714285714285714285)
	 1.4285714285714286e22)
   (let ((z (make-rectangular +inf.0 +inf.0)))
     (test "Error case #10"		; Bug 059
	   (* 1.0 z)
	   z))
   (let ((z (make-rectangular +inf.0 +inf.0)))
     (test "Error case #11"		; Bug 059
	   (* z 1.0)
	   z))
   (let ((z (make-rectangular +inf.0 +inf.0)))
     (test "Error case #12"		; Bug 059
	   (* 1 z)
	   z))
   (let ((z (make-rectangular +inf.0 +inf.0)))
     (test "Error case #13"		; Bug 059
	   (* z 1)
	   z))
   (test "Error case #14"		; Bug 079
	 (modulo 33333333333333333333 -3) 0)
   (test "Error case #15"		; Bug 079
	 (modulo 2177452800 -86400) 0)
   (test "Error case #16"		; Bug 079
	 (modulo -2177452800 -86400) 0)
   (test "Error case #17"		; Bug 080
	 (modulo 33333333333333333333.0 -3.0) 0.0)
   (test "Error case #18"		; Bug 064
	 (call-with-values 
	  (lambda () 
	    (call-with-current-continuation (lambda (k) (values 1 2 3))))
	  (lambda (a b c) 
	    (list a b c)))
	 '(1 2 3))
   (test "Error case #19"		; Bug 064
	 (call-with-values 
	  (lambda () 
	    (call-with-current-continuation (lambda (k) (k 1 2 3)))) 
	  (lambda (a b c) 
	    (list a b c)))
	 '(1 2 3))
   (test "Error case #20"		; Bug 064
	 (call-with-values 
	  (lambda ()
	    (call-with-current-continuation (lambda (k) (k 1 2 3)))) 
	  (lambda a a))
	 '(1 2 3))
   (test "Error case #21"               ; Bug 082
         (let ()
           (define (fact n)
             (if (< n 2)
                 1
                 (* n (fact (- n 1)))))
           (exact->inexact (fact 171)))
         +inf.0)
   (test "Error case #22"               ; Bug 082
         (let ()
           (define (fact n)
             (if (< n 2)
                 1
                 (* n (fact (- n 1)))))
           (= +inf.0 (exact->inexact (fact 170))))
         #f)
   (test "Error case #23"               ; Bug 082
         (exact->inexact 14285714285714285714285)
         1.4285714285714286e22)
   ; NOTE!  This test can fail because it computes a ratio of execution
   ; times of two identical programs, but the programs are not the sole
   ; determinant of how fast they run...  I have observed a factor-of-two
   ; difference on _identical_ binary code running in the _same_ process,
   ; where the only difference between the two was where in memory the
   ; code was located.
   (test "Error case #24"               ; Bug 105
         (let* ((t0 (memstats))
                (d0 (bug-105-test1))
                (t1 (memstats))
                (d1 (bug-105-test2))
                (t2 (memstats)))
           (let ((alloc 0)              ; words allocated
                 (user 23))             ; user time
             ; Test that allocation is the same and that execution time
             ; ratio is within reason.
             (cons 
              (= (- (vector-ref t1 alloc) (vector-ref t0 alloc))
                 (- (vector-ref t2 alloc) (vector-ref t1 alloc)))
              (let ((time1 (- (vector-ref t1 user) (vector-ref t0 user)))
                    (time2 (- (vector-ref t2 user) (vector-ref t1 user))))
                (if (> time1 time2)
                    (or (>= (/ time2 time1) 0.7) (/ time2 time1))
                    (or (>= (/ time1 time2) 0.7) (/ time1 time2)))))))
         '(#t . #t))
   (test "Error case #25"               ; Bug 107
         (let ((ans #t))
           (let ((x (bug-107-datum)))
             (case (car x)
               ((codevector) (set! ans #f))
               ((constantvector) (set! ans #f))))
           ans)
         #t)
   (test "Error case #26"		; Bug in Larceny through 0.51
	 (remainder (hide #xc6450c30) (hide #x100000000))
	 #xc6450c30)
   (test "Ticket #27"                   ; Bug in Larceny through 0.90
         (+ (hide 1+2i) (hide 1.1+2.1i))
         2.1+4.1i)
   (test "Ticket #28"                   ; Bug in Larceny through 0.90
         (exact->inexact (hide (/ (expt 2 1500) (- (expt 2 1500) 1))))
         1.0)
   (test "(= 1234567890 3.4)"           ; discovered via Ticket #28
         (= (hide 1234567890) (hide 3.4))
         #f)
   (test "Ticket #31 (1)"               ; Bug in Larceny through 0.90
         (exact->inexact (hide (/ (expt 10 1000) 3)))
         +inf.0)
   (test "Ticket #31 (2)"               ; Bug in Larceny through 0.90
         (* (hide 12345678901) (hide +inf.0))
         +inf.0)
   (test "Ticket #31 (3)"               ; Bug in Larceny through 0.90
         (/ (hide 1.0) (hide (expt 2 -2000)))
         +inf.0)
   (test "Ticket #31 (4)"               ; Bug in Larceny through 0.90
         (/ (hide 1.0) (hide (expt 2 1000)))
         9.332636185032189e-302)
   (test "Ticket #31 (5)"               ; Bug in Larceny through 0.90
         (/ (hide 1.0) (hide (expt 2 2000)))
         0.0)
   (test "Ticket #32 (1)"               ; Bug in Larceny through 0.90
         (string->number (hide "1/0"))
         #f)
   (test "Ticket #32 (2)"               ; Bug in Larceny through 0.90
         (string->number (hide "-1/0"))
         #f)
   (test "Ticket #32 (3)"               ; Bug in Larceny through 0.90
         (string->number (hide "0/0"))
         #f)
   (test "Ticket #32 (4)"               ; Bug in Larceny through 0.90
         (let ((x (string->number (hide "#i0/0")))) (= x x))
         #f)
   (test "Ticket #32 (5)"               ; Bug in Larceny through 0.90
         (string->number (hide "#i7/0"))
         +inf.0)
   (test "Ticket #32 (6)"               ; Bug in Larceny through 0.90
         (string->number (hide "#i-1/0"))
         -inf.0)
   (test "Ticket #33 (1)"               ; Bug in Larceny through 0.90
         (/ (hide 3+4.5i) (hide 3+4.5i))
         1.0)
   (test "Ticket #33 (2)"               ; Bug in Larceny through 0.90
         (/ (hide 4.5+3i) (hide 4.5+3i))
         1.0)
   (test "Ticket #33 (3)"               ; Bug in Larceny through 0.90
         (number->string (hide (/ (hide +3.0i) (hide +3.0i))))
         "1.0")
   (test "Ticket #33 (4)"               ; Bug in Larceny through 0.90
         (flonum? (/ (hide 3+4.5i) (hide 3+4.5i)))
         #t)
   (test "Ticket #33 (5)"               ; Bug in Larceny through 0.90
         (flonum? (/ (hide 4.5+3i) (hide 4.5+3i)))
         #t)
   ; Long-time latent bug in Larceny through 0.91,
   ; discovered during reimplementation of multiple values for 0.92.
   (test "dynamic-wind and values"
         (let ((f (lambda () (values 17 42 666))))
           (call-with-values
            (lambda ()
              (dynamic-wind (lambda () (hide 37))
                            (lambda () ((hide f)))
                            (lambda () (values 1 2 3))))
            list))
         '(17 42 666))
   (test "Ticket #151"                  ; Bug in Larceny through 0.92b
         (let* ((x 1.1125369292536007e-308)
                (y (* 2 x))
                (rx (inexact->exact x))
                (ry (inexact->exact y)))
           (/ ry rx))
         2)
   (test "Ticket #87 (1)"               ; Bug in Larceny through 0.92b
         (expt 0.0 -2.0)
         +inf.0)
   (test "Ticket #87 (2)"               ; Bug in Larceny through 0.92b
         (expt 0.0 0.0)
         1.0)
   (test "Ticket #87 (3)"               ; Bug in Larceny through 0.92b
         (expt 0.0 2.0)
         0.0)
   (mustfail "Ticket #147"              ; Bug in Larceny through 0.92b
         (lambda () (floor 0+1.0i)))
   (test "Ticket #390"
         (* #x10000000 (- #x10))
         (- #x100000000))
   (test "Ticket #424"                  ; Bug in IA32 Larceny 0.93
         (bug-424-plus 4294967293)
         (+ 1 4294967293))
   ))

(define (bug-105-test1)
  (do ((i 0 (+ i 1))
       (j 0 (+ j 1)))
      ((= i 100000000) j)))

(define (bug-105-test2)
  (let loop ((i 0) (j 0))
    (if (= i 100000000)
        j
        (loop (+ i 1) (+ j 1)))))

(define (bug-107-datum)
  '(foobar))

(define (bug-424-plus n)
  (+ n 1))

; eof
