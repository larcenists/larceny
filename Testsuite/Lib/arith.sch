; Test/arith.sch
; Larceny test suite -- numerical operations
;
; $Id$
;
; The test scaffolding is in test.sch, which should be loaded first.
;
; While there is a notion here that later tests depend on earlier tests, this
; is only partially so, as some innocent primitives invoke very heavy machinery
; indeed behind the programmer's back, and hence we have difficulty doing
; proper incremental testing.
;
; For best results, one would run this code in both compiled and interpreted
; modes.  Some code has a very contorted look in order to defy compiler
; optimizations; it's unclear how effective (and necessary) this is.

(define (test-numerical-operations)
  (and (test-number-representation-predicates)
       (test-number-type-predicates)
       (test-number-ordering-predicates/same-representation)
       (test-eqv?-on-numbers)
       (test-basic-arithmetic)
       (test-round-and-truncate)
       (test-bit-operations)
       (test-bignum-arithmetic)
       (test-exactness-predicates)
       (test-exactness-conversion)
       ; (test-number-ordering-predicates/mixed-representation)
       (test-odd-even)))

(define (test-number-representation-predicates)

  (define (rpred n)
    (list (fixnum? n)
	  (bignum? n)
	  (ratnum? n)
	  (flonum? n)
	  (compnum? n)
	  (rectnum? n)))

  (display "----------------------------------------") (newline)
  (display "Testing fixnum?, bignum?, ratnum?, flonum?, compnum?, rectnum?")
  (newline)
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

(define (test-number-type-predicates)

  (define (numberpred n)
    (list (integer? n) (rational? n) (real? n) (complex? n)))

  (display "----------------------------------------") (newline)
  (display "Testing integer?, rational?, real?, complex?") (newline)
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
   (test "(numberpred 0.0)" (numberpred 0.0) '(#t #t #t #t))
   (test "(numberpred -0.0)" (numberpred -0.0) '(#t #t #t #t))
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

(define (test-number-ordering-predicates/same-representation)

  (define (p m n)
    (list (= m n) (> m n) (< m n) (>= m n) (<= m n)))

  (define (q m)
    (list (zero? m) (negative? m) (positive? m)))

  (display "----------------------------------------") (newline)
  (display "Testing =, >, <, >=, <=, zero?, negative?, positive?") (newline)
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

(define (test-eqv?-on-numbers)
  (display "----------------------------------------") (newline)
  (display "Testing eqv? on numbers") (newline)
  (test-eqv?-on-numbers-helper 0 1 -1 0.0 1.0 -1.0 1/2 0.5 1+1i 1.0+1.0i))

(define (test-eqv?-on-numbers-helper exact-zero exact-one exact-neg-one
				     inexact-zero inexact-one inexact-neg-one
				     exact-one-half inexact-one-half
				     exact-1+1i inexact-1+1i)
  (allof
   (test "(eqv? 0 1)" (eqv? exact-zero exact-one) #f)
   (test "(eqv? 1 1)" (eqv? exact-one exact-one) #t) ; A smart compiler...
   (test "(eqv? 0.0 1.0)" (eqv? inexact-zero inexact-one) #f)
   (test "(eqv? 1.0 1.0)" (eqv? inexact-one inexact-one) #f)
   (test "(eqv? 1 1.0)" (eqv? exact-one inexact-one) #f)
   (test "(eqv? 0 0.0)" (eqv? exact-zero inexact-zero) #f)
   (test "(eqv? 1/2 0.5)" (eqv? exact-one-half inexact-one-half) #f)
   (test "(eqv? 1/2 1/2)" (eqv? exact-one-hald exact-one-hald) #t)
   (test "(eqv? 1+1i 1.0+1.0i)" (eqv? exact-1+1i inexact-1+1i) #f)
   (test "(eqv? 1+1i 1+1i)" (eqv? exact-1+1i exact-1+1i) #t)
   (test "(eqv? 1.0+1.0i 1.0+1.0i)" (eqv? inexact-1+1i inexact-1+1i) #t)
   ))

(define (test-basic-arithmetic)

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
    (display "----------------------------------------") (newline)
    (display "Basic arithmetic") (newline)

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

(define (test-round-and-truncate)
  (display "----------------------------------------") (newline)
  (display "Testing round, truncate") (newline)
  (allof
   ; Use eqv? since it takes exactness into account.
   (test "(= (round 1.4) 1.0)" (eqv? (round 1.4) 1.0) #t)
   (test "(= (round 1.0) 1.0)" (eqv? (round 1.0) 1.0) #t)
   (test "(= (round 1.4) 1.4)" (eqv? (round 1.4) 1.4) #f)
   (test "(= (round -1.5) -1.0)" (eqv? (round -1.5) -1.0) #f)
   (test "(= (round 1.5) 2.0)" (eqv? (round 1.5) 2.0) #t)
   (test "(= (round -1.5) -2.0)" (eqv? (round -1.5) -2.0) #t)
   (test "(= (truncate 1.5) 1.0)" (eqv? (truncate 1.5) 1.0) #t)
   (test "(= (truncate 1.0) 1.0)" (eqv? (truncate 1.0) 1.0) #t)
   (test "(= (truncate -1.5) -1.0)" (eqv? (truncate -1.5) -1.0) #t)
   ))

(define (test-bit-operations)
  (display "----------------------------------------") (newline)
  (display "Testing bit operations") (newline)
  (allof
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
; FIXME
; Test 'lognot' also.
; Test 'logxor' also.
; Test 'rot' also.
   ))

(define (test-bignum-arithmetic)
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

(define (test-exactness-predicates)

  (define (etest n)
    (list (exact? n) (inexact? n)))

  (display "----------------------------------------") (newline)
  (display "Test exact?, inexact?") (newline)
  (allof
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


(define (test-exactness-conversion)
  (display "----------------------------------------") (newline)
  (display "Testing exact->inexact, inexact->exact") (newline)
  (allof
   (test "(eqv? 0.0 (exact->inexact 0))" (eqv? 0.0 (exact->inexact 0)) #t)
   (test "(eqv? 1.0 (exact->inexact 1))" (eqv? 1.0 (exact->inexact 1)) #t)
   (test "(eqv? 0 (inexact->exact 0.0))" (eqv? 0 (inexact->exact 0.0)) #t)
   (test "(eqv? 1 (inexact->exact 1.0))" (eqv? 1 (inexact->exact 1.0)) #t)
   (test "(eqv? 1.0+1.0i (exact->inexact 1+1i))" 
	 (eqv? 1.0+1.0i (exact->inexact 1+1i)) #t)
   (test "(eqv? 1+1i (inexact->exact 1.0+1.0i))"
	 (eqv? 1+1i (inexact->exact 1.0+1.0i)) #t)
   (test "(eqv? 0.5 (exact->inexact 1/2))" (eqv? 0.5 (exact->inexact 1/2)) #t)
   (test "(eqv? 1/2 (inexact->exact 0.5))" (eqv? 1/2 (inexact->exact 0.5)) #t)
   ))


; FIXME
(define (test-number-constructors)
  (test "(complex? (make-rectangular 1 1))"
	(complex? (make-rectangular 1 1)) 
	#t))


; FIXME
(define (test-number-ordering-predicates/mixed-representation)
  (allof
   (test "(q 0.0)" (q 0.0) '(#t #f #f))
   (test "(q -0.0)" (q -0.0) '(#t #f #f))
   (test "(q 10.5)" (q 10.5) '(#f #f #t))
   (test "(q 12345678901234567890)" (q 12345678901234567890) '(#f #f #t))
   (test "(q -12345678901234567890)" (q -12345678901234567890) '(#f #t #f))
   ))


(define (test-odd-even)
  (display "----------------------------------------") (newline)
  (display "Testing odd?, even?") (newline)
  (allof
   ; easy cases
   (test "(even? 0)" (even? 0) #t)
   (test "(even? 1)" (even? 1) #f)
   (test "(even? 2)" (even? 2) #t)
   (test "(even? -1)" (even? -1) #f)
   (test "(even? -2)" (even? -2) #t)
   ; fixnum limits
   (test "(even? 536870911)" (even? 536870911) #f)
   (test "(even? -536870912)" (even? -536870912) #t)
   ; bignums
   (test "(even? 536870912)" (even? 536870912) #t)
   (test "(even? 536870913)" (even? 536870913) #f)
   ; flonums
   (test "(even? 0.0)" (even? 0.0) #t)
   (test "(even? 1.0)" (even? 1.0) #f)
   (test "(even? 2.0)" (even? 2.0) #t)
   (test "(even? -1.0)" (even? -1.0) #f)
   (test "(even? -2.0)" (even? -2.0) #t)
   ; other -- invalid arguments
   (shouldfail "(even? 3/4)" (lambda () (even? 3/4)))
   (shouldfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (shouldfail "(even? 1.1)" (lambda () (even? 1.1)))
   (shouldfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (shouldfail "(even? 1+3i)" (lambda () (even? 1+3i)))
   (shouldfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (shouldfail "(even? 'foo)" (lambda () (even? 'foo)))
   (shouldfail "(odd? 'foo)" (lambda () (odd? 'foo)))
   ; easy cases
   (test "(odd? 0)" (odd? 0) #f)
   (test "(odd? 1)" (odd? 1) #t)
   (test "(odd? 2)" (odd? 2) #f)
   (test "(odd? -1)" (odd? -1) #t)
   (test "(odd? -2)" (odd? -2) #f)
   ; fixnum limits
   (test "(odd? 536870911)" (odd? 536870911) #t)
   (test "(odd? -536870912)" (odd? -536870912) #f)
   ; bignums
   (test "(odd? 536870912)" (odd? 536870912) #f)
   (test "(odd? 536870913)" (odd? 536870913) #t)
   ; flonums
   (test "(odd? 0.0)" (odd? 0.0) #f)
   (test "(odd? 1.0)" (odd? 1.0) #t)
   (test "(odd? 2.0)" (odd? 2.0) #f)
   (test "(odd? -1.0)" (odd? -1.0) #t)
   (test "(odd? -2.0)" (odd? -2.0) #f)
   ; other -- invalid arguments
   (shouldfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (shouldfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (shouldfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (shouldfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (shouldfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (shouldfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (shouldfail "(odd? 'foo)" (lambda () (odd? 'foo)))
   (shouldfail "(odd? 'foo)" (lambda () (odd? 'foo)))
   ))

; eof
