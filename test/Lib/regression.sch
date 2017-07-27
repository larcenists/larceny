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
         1.0+0.0i)
   (test "Ticket #33 (2)"               ; Bug in Larceny through 0.90
         (/ (hide 4.5+3i) (hide 4.5+3i))
         1.0+0.0i)
   (test "Ticket #33 (3)"               ; Bug in Larceny through 0.90
         (number->string (hide (/ (hide +3.0i) (hide +3.0i))))
         "1.0+0.0i")
   (test "Ticket #33 (4)"               ; Bug in Larceny through 0.90
         (compnum? (/ (hide 3+4.5i) (hide 3+4.5i)))
         #t)
   (test "Ticket #33 (5)"               ; Bug in Larceny through 0.90
         (compnum? (/ (hide 4.5+3i) (hide 4.5+3i)))
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

   ; As of v0.963, (expt 0.0 -2.0) returns +nan.0 instead of +inf.0

   (test "Ticket #87 (1)"               ; Bug in Larceny through 0.92b
         (begin (expt 0.0 -2.0) 'okay)
         'okay)
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
   (test "Ticket #457"                  ; Bug in Sparc Larceny 0.93
         (* 2.0 (log -1.0))
         0.0+6.283185307179586i)
   (test "Ticket #512"                  ; Bug in Larceny 0.95
         (char->integer (string-ref "\x10FFFD;" 0))
         #x10fffd)
   (test "Ticket #512"                  ; Bug in Larceny 0.95
         (char->integer (string-ref (symbol->string '\x10FFFD;) 0))
         #x10fffd)
   (test "Ticket #511"                  ; Bug in Larceny 0.95
         (let ()                        ; contributed by R Kent Dybvig
           (define x
             (let ((x1 (vector 'h))
                   (x2 (let ((x (list #f))) (set-car! x x) x)))
               (vector x1 (vector 'h) x1 (vector 'h) x1 x2)))
           (define y
             (let ((y1 (vector 'h))
                   (y2 (vector 'h))
                   (y3 (let ((x (list #f))) (set-car! x x) x)))
               (vector (vector 'h) y1 y1 y2 y2 y3)))
           (equal? x y))
         #t)
   (test "Ticket #511"                  ; Bug in Larceny 0.95
         (let ()                        ; contributed by R Kent Dybvig
           (define x
             (let ((x (cons (cons #f 'a) 'a)))
               (set-car! (car x) x)
               x))
           (define y
             (let ((y (cons (cons #f 'a) 'a)))
               (set-car! (car y) (car y))
               y))
           (equal? x y))
         #t)
   (test "Ticket #511"                  ; Bug in Larceny 0.95
         (let ((k 100))                 ; contributed by R Kent Dybvig
           (define x
             (let ((x1 (cons 
                        (let f ((n k))
                          (if (= n 0)
                              (let ((x0 (cons #f #f)))
                                (set-car! x0 x0)
                                (set-cdr! x0 x0)
                                x0)
                              (let ((xi (cons #f (f (- n 1)))))
                                (set-car! xi xi)
                                xi)))
                       #f)))
               (set-cdr! x1 x1)
               x1))
           (define y
             (let* ((y2 (cons #f #f)) (y1 (cons y2 y2)))
               (set-car! y2 y1)
               (set-cdr! y2 y1)
               y1))
           (equal? x y))
         #t)
   (test "Ticket #519"                  ; Bug in Larceny 0.96
         (let ((s "abcdefgh"))          ; contributed by Ray Racine
           (define echochars
             (lambda (ip op)
               (let loop ((ch (peek-char ip)))
                 (cond ((eof-object? ch)
                        (get-output-string op))
                       (else
                        (write-char (read-char ip) op)
                        (loop (peek-char ip)))))))
           (string=?
            s
            (echochars (open-input-string s) (open-output-string))))
         #t)
   (test "Ticket #521"                  ; Bug in Larceny 0.96
                                        ; contributed by Ray Racine
         (let ((bip (open-bytevector-input-port (string->utf8 "abcd"))))
           (get-u8 bip)
           (get-char (transcoded-port bip (native-transcoder))))
         #\b)
   (test "Ticket #523"                  ; Bug in Larceny 0.96
                                        ; contributed by Ray Racine
         (let* ((ip (open-string-input-port "12345678"))
                (s1 (get-string-n ip 4))
                (s2 (get-string-n ip 4)))
           (list s1 s2))
         '("1234" "5678"))
   (test "Ticket #523"                  ; Bug in Larceny 0.96
                                        ; contributed by Ray Racine
         (let* ((ip (open-string-input-port "12345678"))
                (s (make-string 8)))
           (get-string-n! ip s 0 4)
           (get-string-n! ip s 4 4)
           s)
         "12345678")
   (test "Ticket #525"                  ; Bug in Larceny 0.96
         (eof-object?                   ; contributed by Ray Racine
          (get-bytevector-n! (open-bytevector-input-port (make-bytevector 0))
                             (make-bytevector 32)
                             0 16))
         #t)
   (test "Ticket #525"                  ; Bug in Larceny 0.96
         (eof-object?                   ; contributed by Ray Racine
          (get-bytevector-n (open-bytevector-input-port (make-bytevector 0))
                            16))
         #t)
   (test "Ticket #530"                  ; Bug in Larceny 0.96
                                        ; contributed by Ray Racine
         (let ((bv (make-bytevector 4 0)))
           (bytevector-s32-set! bv 0 -1 'little)
           (bytevector-s32-ref bv 0 'little))
         -1)
   (test "Ticket #530"                  ; Bug in Larceny 0.96
                                        ; contributed by Ray Racine
         (let ((bv (make-bytevector 4 0)))
           (bytevector-s32-set! bv 0 -1 'big)
           (bytevector-s32-ref bv 0 'big))
         -1)
   (test "Ticket #526"                  ; Bug in Larceny 0.96
                                        ; contributed by Abdulaziz Ghuloum
                                        ;
                                        ; In Unicode 7.0.0, #\x180e does not
                                        ; have the "White_Space" property.
         (let ((s0 (string
                    #\"
                    #\a #\\ #\tab #\xa0 #\newline #\x1680 ; #\x180e
                                                            #\x2000
                    #\b #\\ #\x2001 #\x2002 #\x2003 #\x2004 #\x2005 #\return
                    #\c #\\ #\x2006 #\x2006 #\x2006 #\return #\linefeed #\x2007
                    #\d #\\ #\x2008 #\x2009 #\x200a #\x85 #\x202f #\x205f
                    #\e #\\ #\x3000 #\space #\return #\x85 #\space #\space
                    #\f #\\ #\tab #\tab #\x2028 #\space #\space
                    #\g
                    #\")))
           (call-with-input-string s0 get-datum))
         "abcdefg")
   (test "Ticket #548"                  ; Bug in Larceny 0.961
         (list (char=? #\a #\a #\a)
               (char=? #\a #\a #\a #\a #\a #\a)
               (char=? #\a #\a #\a #\a #\a #\b)
               (char=? #\b #\a #\a)
               (char=? #\a #\b #\a)
               (char=? #\a #\a #\b)
               (char<? #\a #\b #\c)
               (char<? #\a #\b #\a)
               (char>? #\c #\b #\a)
               (char>? #\c #\a #\b)
               (char<=? #\a #\b #\c)
               (char<=? #\b #\a #\a)
               (char>=? #\c #\b #\b)
               (char>=? #\c #\d #\c))
         '(#t #t #f #f #f #f #t #f #t #f #t #f #t #f))

   ; This was really a bug in Twobit's pass 3.

   (test "Ticket #543"                  ; Bug in Larceny 0.961
         (let* ((f (hide (lambda (x y)
                           (let* ((pos-finis (+ x 1))
                                  (rng-finis (or (+ y 1) 'top)))
                             (let ((p3 (if rng-finis
                                           (min pos-finis rng-finis)
                                           pos-finis))
                                   (p4 (if rng-finis
                                           (max pos-finis rng-finis)
                                           pos-finis)))
                               (list p3 p4)))))))
           (f (hide 33) (hide 44)))
         '(34 45))

   (test "Ticket #552"                  ; Bug in Larceny 0.962
         (assp odd?                     ; contributed by Ray Racine
               '((0 . 5) (1 . 6)))
         '(1 . 6))

   (test "Ticket #568"                  ; Bug in Larceny 0.962
         (fixnum->flonum (hide -17))    ; detected by PLT test suite
         -17.0)

   (test "Ticket #557"                  ; Bug in Larceny 0.962
         (log 1024 2)                   ; detected by PLT test suite
         10.0)

   (test "Ticket #557"                  ; Bug in Larceny 0.962
         (log 2048.0 2.0)               ; detected by PLT test suite
         11.0)

   (test "Ticket #564"                  ; Bug in Larceny 0.962
         (let ((v (vector 5 3 8 4 2)))  ; detected by PLT test suite
           (vector-sort! < v)
           v)
         '#(2 3 4 5 8))

   (test "Ticket #565"                  ; Bug in Larceny 0.962
         (for-all values '(1 2 3 4 5))  ; detected by PLT test suite
         5)

   (test "Ticket #565"                  ; Bug in Larceny 0.962
         (exists list '(1 2 3 4)        ; detected by PLT test suite
                      '(5 6 7 8)
                      '(9 10 11 12))
         '(1 5 9))

   (test "Ticket #565"                  ; Bug in Larceny 0.962
         (for-all list '(1 2 3 4)       ; detected by PLT test suite
                       '(5 6 7 8)
                       '(9 10 11 12))
         '(4 8 12))

   (test "Ticket #566"                  ; Bug in Larceny 0.962
         (fold-left list                ; detected by PLT test suite
                    '()
                    '(1 2 3 4 5)
                    '(6 7 8 9 10))
         '(((((() 1 6) 2 7) 3 8) 4 9) 5 10))

   (test "Ticket #566"                  ; Bug in Larceny 0.962
         (fold-right list                ; detected by PLT test suite
                     '()
                     '(1 2 3 4 5)
                     '(6 7 8 9 10)
                     '(11 12 13 14 15))
         '(1 6 11 (2 7 12 (3 8 13 (4 9 14 (5 10 15 ()))))))

   (test "Ticket #569"                  ; Bug in Larceny 0.962
         (string=? "ab" "ab" "ab")      ; detected by PLT test suite
         #t)

   (test "Ticket #569"                  ; Bug in Larceny 0.962
         (string<? "ab" "ab" "ab")      ; detected by PLT test suite
         #f)

   (test "Ticket #569"                  ; Bug in Larceny 0.962
         (string<=? "ab" "ab" "ac")     ; detected by PLT test suite
         #t)

   (test "Ticket #569"                  ; Bug in Larceny 0.962
         (string>? "ab" "ab" "ac")      ; detected by PLT test suite
         #f)

   (test "Ticket #569"                  ; Bug in Larceny 0.962
         (string>=? "ab" "ab" "ac")     ; detected by PLT test suite
         #f)

   (test "Ticket #571"                  ; Bug in Larceny 0.962
         (quotient (least-fixnum) -1)   ; detected with help of PLT test suite
         (+ 1 (greatest-fixnum)))

   (test "Ticket #572"                  ; Bug in Larceny 0.962
         (rational? +inf.0)             ; detected by PLT test suite
         #f)

   (test "Ticket #572"                  ; Bug in Larceny 0.962
         (rational? +nan.0)             ; detected by PLT test suite
         #f)

   (test "Ticket #572"                  ; Bug in Larceny 0.962
         (integer? +inf.0)              ; detected by PLT test suite
         #f)

   (test "Ticket #572"                  ; This wasn't a bug
         (integer? +nan.0)              ; but let's test it anyway.
         #f)

   (test "Ticket #573"                  ; Bug introduced while fixing #572
         (flnumerator -inf.0)
         -inf.0)

   (test "Ticket #574"                  ; Bug in Larceny 0.962
         (rationalize +inf.0 3)         ; detected by PLT test suite
         +inf.0)

   (test "Ticket #574"                  ; Bug in Larceny 0.962
         (nan?                          ; detected by PLT test suite
          (rationalize +inf.0 +inf.0))
         #t)

   (test "Ticket #554"                  ; Bug in Larceny 0.962
         (bitwise-and (expt 2 100) 17)  ; detected by PLT test suite
         0)

   (test "Ticket #605"                  ; Bug in Larceny 0.97b1
         (exact->inexact (- (expt 2 96) 1))
         7.922816251426434e28)

   (test "Ticket #641"                  ; Bug in Larceny 0.97b1
         ((lambda (f_2_3)               ; compiler bug
            (f_2_3 '1 '2))
          (lambda (ta_5_8 ta_6_8)
            ((lambda (a_11)
               ((lambda (b_14)
                  ((lambda ()
                     ((lambda (c_19)
                        ((lambda (d_21)
                           ((lambda (e_23) '99)
                            (+ a_11
                               b_14
                               c_19
                               d_21)))
                         (+ a_11 b_14 c_19)))
                      a_11))))
                ta_6_8))
             ta_5_8)))
         99)

   (test "Ticket #643"                  ; Bug in Larceny 0.97
         (procedure-name procedure-name)
         'procedure-name)

   (test "Ticket #11"                   ; Bug in Larceny 0.97 and previous
         (let-syntax ((foo (syntax-rules ()
                            ((_ var) (define var 1)))))
           (let ((x 2))
             (begin (define foo +))
             (cond (else (foo x))) 
             x))
         2)

   (test "Ticket #667"                  ; Bug in Larceny 0.962 through 0.97
         (string->number "+i")
         (make-rectangular 0 1))

   (test "Ticket #670"                  ; Bug in Larceny 0.97 and previous
         (begin (enable-interrupts 1000000)
                (call-with-current-continuation
                 (lambda (k)
                   (dynamic-wind
                    (lambda () #t)
                    (lambda () 17)
                    (lambda () (k #t)))))
                (let ((status (disable-interrupts)))
                  (enable-interrupts 1000000)
                  (and status #t)))
         #t)

   (test "Ticket #688"                  ; Bug in Larceny 0.97 and previous
         ((apply bug-688 bug-688-input))
         bug-688-input)

   (test "Ticket #681"                  ; Bug in Larceny 0.97 and previous
         (let ((a 'hi))
           (define-values (a weird) (let ((w (not a))) (values '_ w)))
           weird)
         #f)

   (test "Ticket #698"                  ; Bug in Larceny 0.97 and previous
         (bitwise-arithmetic-shift-right 42 (greatest-fixnum))
         0)

   (test "Ticket #674"                  ; Bug found during ARM port
	 (bug-rest-arguments-test
	  (lambda (x1 x2 x3 x4 . rest)
	    (list x1 x2 x3 x4 rest)))
	 '(1 2 3 4 (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)))

   (test "Ticket #674"                  ; Bug found during ARM port
	 (bug-rest-arguments-test
	  (lambda (x1 x2 x3 x4 x5 . rest)
	    (list x1 x2 x3 x4 x5 rest)))
	 '(1 2 3 4 5 (6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                      21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)))

   (test "Ticket #674"                  ; Bug found during ARM port
	 (bug-rest-arguments-test
	  (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
		   x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
		   x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
		   x31 x32 x33 x34 x35 x36 . rest)
	    (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
		   x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
		   x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
		   x31 x32 x33 x34 x35 x36 rest)))
	 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
           21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 (37 38 39 40)))

   (test "Ticket #744 (min)"            ; Bug in v0.98 and previous
         (min +nan.0 0)
         +nan.0)

   (test "Ticket #744 (max)"            ; Bug in v0.98 and previous
         (max +nan.0 0)
         +nan.0)

   (test "Ticket #749"                  ; Bug in v0.93 through v0.98
         (let* ((filename "temptemp")
                (ignored (delete-file filename))
                (opts (file-options no-fail no-truncate))
                (p (open-file-input/output-port filename opts))
                (x (get-u8 p)))
           (close-port p)
           (delete-file filename)
           (eof-object? x))
         #t)

   (test "Ticket #750"                  ; Bug in v0.98 and previous
         (string-titlecase "x\x130;")
         "Xi\x307;")

   (test "Ticket #766"                  ; Bug in v0.99 and previous
         '(#0=#1=(a) #0#)
         '((a) (a)))

   (test "Ticket #767"                  ; Bug in v0.98 and v0.99
         (let ((param (make-parameter 1 (lambda (x) (* 10 x)))))
           (parameterize ((param 2)) #f)
           (param))
         10)

   (test "Ticket #770"                  ; Bug in v0.99 and previous
         (let* ((v1 '#(quote foo))
                (v2 '#(syntax foo))
                (q (open-output-string))
                (ignored (begin (write v1 q)
                                (write-char #\space q)
                                (write v2 q)))
                (s (get-output-string q))
                (p (open-input-string s)))
           (and (equal? v1 (read p))
                (equal? v2 (read p))))
         #t)

   (test "Ticket #782"                  ; Bug in v0.99 and previous
         (let ()                        ; (compiler bug: constant propagation)
           (define (g f x y z)
             (if (= z 0)
                 (f 1)
                 (g (lambda (g13)
                      (f (* z g13)))
                    #f y (- z 1))))
           (g (lambda (x) x)
              #f #f 3))
         6)

   (test "Ticket #786"                  ; Bug in v0.99 and previous
         (let ((x 2.9802322e-8)
               (bv (make-bytevector 4)))
           (bytevector-ieee-single-native-set! bv 0 x)
           (bytevector-ieee-single-native-ref bv 0))
         2.9802322387695312e-8)

   (test "Ticket #790"                  ; Bug in v0.99 and previous
         (eqv? (fl+ -0.0) -0.0)
         #t)

   (test "Ticket #792"                  ; Bug in v0.99 and previous
         (expt 2.0 -1024)
         5.562684646268003e-309)

   (test "Ticket #793"                  ; Bug in v0.99 and previous
         (inexact (expt 2 -1024))
         5.562684646268003e-309)

   (test "Ticket #794"                  ; Bug in v0.99 and previous
         (fl- 1e308 1e308 1e308)
         -1e308)

   (test "Ticket #794"                  ; Bug in v0.99 and previous
         (fl/ 1e308 1e308 2.0)
         0.5)

   (test "Ticket #795"                  ; Bug in v0.99 and previous
         (magnitude -1e308)
         1e308)

   (test "Ticket #797"                  ; Bug in v0.99 and previous
         (max -34.0 -0.0)
         -0.0)

   (test "Ticket #800"                  ; Bug in v0.99 (ARM only)
         (fl<? +nan.0 1.0)
         #f)

   (test "Ticket #743"                  ; Bug in v0.99 and previous
         (list
          (vector-length     ((if #t make-vector) 4))
          (bytevector-length ((if #t make-bytevector) 4))
          (string-length     ((if #t make-string) 4))
          (ustring-length    ((if #t make-ustring) 4))    ; FIXME
          ((if #t =) 1 2 3 4)
          ((if #t <) 1 2 3 4)
          ((if #t >) 1 2 3 4)
          ((if #t <=) 1 2 3 4)
          ((if #t >=) 1 2 3 4)
          ((if #t +) 1 2 3 4)
          ((if #t -) 1 2 3 4)
          ((if #t *) 1 2 3 4)
          ((if #t /) 1 2 3 4)
          ((if #t char=?) #\a #\b #\c #\d)
          ((if #t char<?) #\a #\b #\c #\d)
          ((if #t char>?) #\a #\b #\c #\d)
          ((if #t char<=?) #\a #\b #\c #\d)
          ((if #t char>=?) #\a #\b #\c #\d))
         '(4 4 4 4 #f #t #f #t #f 10 -8 24 1/24 #f #t #f #t #f))
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

(define (bug-688 a b c d e f g h i j k l m n o p q r s t u v w x y z
                 a27 a28 a29 a30 a31)
  (lambda ()
    (list a b c d e f g h i j k l m n o p q r s t u v w x y z
          a27 a28 a29 a30 a31)))

(define bug-688-input
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
      27 28 29 30 31))

(define (bug-rest-arguments-test f)
  (f  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
     21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40))

; eof
