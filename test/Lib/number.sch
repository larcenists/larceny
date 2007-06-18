; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test suite -- numerical operations.
;
; The test scaffolding is in test.sch, which should be loaded first.
;
; While there is a notion here that later tests depend on earlier tests, 
; this is only partially so, as some innocent primitives invoke very 
; heavy machinery indeed behind the programmer's back, and hence we have 
; difficulty doing proper incremental testing.
;
; For best results, one would run this code in both compiled and interpreted
; modes.  Some code has a very contorted look in order to defy compiler
; optimizations; it's unclear how effective (and necessary) this is.
;
; IMPLEMENTATION STATUS
;   * All sections marked FIXME should be fixed (usually: implemented)
;   * More type checking needs to be tested -- the procedures should fail
;     when given the wrong types; see test-odd-even for an example of
;     how to do this
;   * There are too few hard cases, esp. with regard to flonum arithmetic.

; Order matters.

(define (run-number-tests)
  (display "Number") (newline)
  (test-number-representation-predicates)
  (test-number-type-predicates)
  (test-number-ordering-predicates/same-representation)
  (test-eqv?-on-numbers)
  (test-basic-arithmetic)
  (test-round-truncate-floor-ceiling)
  (test-bit-operations)
  (test-bignum-arithmetic)
  (test-exactness-predicates)
  (test-exactness-conversion)
  (test-number-constructors-and-accessors)
  (test-number-ordering-predicates/mixed-representation)
  (test-odd-even)
  (test-sundry-arithmetic)
  (test-in-out-conversion)
  (test-trancendental-functions))


; NOTE
; Answers should contain booleans only -- correctness of eqv?/equal? has
; not yet been established.

(define (test-number-representation-predicates)

  (define (rpred n)
    (list (fixnum? n)
	  (bignum? n)
	  (ratnum? n)
	  (flonum? n)
	  (compnum? n)
	  (rectnum? n)))

  (allof "number fixnum?, bignum?, ratnum?, flonum?, compnum?, rectnum?"
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

; NOTE
; Answers should contain booleans only -- correctness of eqv?/equal? has
; not yet been established.

(define (test-number-type-predicates)

  (define (numberpred n)
    (list (integer? n) (rational? n) (real? n) (complex? n)))

  (allof "integer?, rational?, real?, complex?"
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

; NOTE
; Answers should contain booleans only -- correctness of eqv?/equal? has
; not yet been established.

(define (test-number-ordering-predicates/same-representation)

  (define (p m n)
    (list (= m n) (> m n) (< m n) (>= m n) (<= m n)))

  (define (q m)
    (list (zero? m) (negative? m) (positive? m)))

  (allof "zero?, negative?, positive?, =, >, <, >=, <="
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
   (test "(= (/ 1.0 0.0) (/ 1.0 -0.0))" (= (/ 1.0 0.0) (/ 1.0 -0.0)) #f)
   ))

; Establishes correctness of eqv?/equal?.

(define (test-eqv?-on-numbers)
  (test-eqv?-on-numbers-helper 0 1 -1 0.0 1.0 -1.0 1/2 0.5 1+1i 1.0+1.0i))

; We must test equal? too, since equal? is used to determine the
; correctness of answers.

(define (test-eqv?-on-numbers-helper exact-zero exact-one exact-neg-one
				     inexact-zero inexact-one inexact-neg-one
				     exact-one-half inexact-one-half
				     exact-1+1i inexact-1+1i)
  (define (p x y)
    (list (eqv? x y) (equal? x y)))

  (allof "eqv? on numbers"
   (test "(p 0 1)" (p exact-zero exact-one) '(#f #f))
   (test "(p 1 1)" (p exact-one exact-one) '(#t #t))
   (test "(p 0.0 1.0)" (p inexact-zero inexact-one) '(#f #f))
   (test "(p 1.0 1.0)" (p inexact-one inexact-one) '(#t #t))
   (test "(p 1 1.0)" (p exact-one inexact-one) '(#f #f))
   (test "(p 0 0.0)" (p exact-zero inexact-zero) '(#f #f))
   (test "(p 1/2 0.5)" (p exact-one-half inexact-one-half) '(#f #f))
   (test "(p 1/2 1/2)" (p exact-one-half exact-one-half) '(#t #t))
   (test "(p 1+1i 1.0+1.0i)" (p exact-1+1i inexact-1+1i) '(#f #f))
   (test "(p 1+1i 1+1i)" (p exact-1+1i exact-1+1i) '(#t #t))
   (test "(p 1.0+1.0i 1.0+1.0i)" (p inexact-1+1i inexact-1+1i) '(#t #t))
   ))

(define (test-basic-arithmetic)

  (define (n1 x y)
    (list (+ x y) (- x y) (* x y)))

  (let* ((a 12345)
	 ($-a (- a))
	 (b 3145)
	 ($-b (- b))
	 (q (lambda (x) (- x)))
	 (two^28 268435456)
	 (two^29 536870912)
	 (two^30 1073741824)
	 (two^31 2147483648)
	 (two^32-1 4294967295))

    (allof "basic arithmetic"
     (test "(- a)" (- a) $-a)
     (test "(- $-a)" (- $-a) a)

     ; these multiply fixnums, producing fixnums

     (test "(* 30 40)" (* 30 40) 1200)
     (test "(* 30 -40)" (* 30 -40) -1200)
     (test "(* -40 -30)" (* -40 -30) 1200)

     ; simple quotients

     (test "(quotient a b)" (quotient a b) 3)
     (test "(quotient b a)" (quotient b a) 0)
     (test "(quotient a a)" (quotient a a) 1)
     (test "(quotient a $-a)" (quotient a $-a) -1)
     (test "(quotient $-a a)" (quotient $-a a) -1)
     (test "(quotient $-a $-a)" (quotient $-a $-a) 1)
     (test "(quotient 63888 65536)" (quotient 63888 65536) 0)

     ; simple remainders

     (test "(remainder a b)" (remainder a b) 2910)
     (test "(remainder b a)" (remainder b a) b)
     (test "(remainder a a)" (remainder a a) 0)
     (test "(remainder b $-a)" (remainder b $-a) b)
     (test "(remainder $-b a)" (remainder $-b a) $-b)
     (test "(remainder a $-b)" (remainder a $-b) 2910)
     (test "(remainder $-a b)" (remainder $-a b) -2910)
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

(define (test-round-truncate-floor-ceiling)

  (define big 4294967296)              ; Big enough to be a bignum!

  (allof "round, truncate, floor, ceiling"
   ; Basic rounding.  
   ; FIXME: need some biggish flonums.
   (test "(round 1)" (round 1) 1)
   (test "(round big)" (round big) big)
   (test "(round 1.0)" (round 1.0) 1.0)
   (test "(round 1.4)" (round 1.4) 1.0)
   (test "(round 1.6)" (round 1.6) 2.0)
   (test "(round 1.4)"  (round 1.4) 1.0)
   (test "(round -1.4)" (round -1.4) -1.0)
   (test "(round -1.6)" (round -1.6) -2.0)
   (test "(round 1/3)" (round 1/3) 0)
   (test "(round -1/3)" (round -1/3) 0)
   (test "(round 4/5)" (round 4/5) 1)
   (test "(round -4/5)" (round -4/5) -1)
   (test "(round big/big+1)" (round (/ big (+ big 1))) 1)
   (test "(round -big/big+1)" (round (/ (- big) (+ big 1))) -1)
   (test "(round big+1/big)" (round (/ (+ big 1) big)) 1)
   (test "(round -(big+1)/big)" (round (/ (- (+ big 1)) big)) -1)

   ; Tests round-to-even
   ; FIXME: should use some bigger numbers
   (test "(round 1.5)" (round 1.5) 2.0)
   (test "(round -1.5)" (round -1.5) -2.0)
   (test "(round 0.5)" (round 0.5) 0.0)
   (test "(round -0.5)" (round -0.5) -0.0)
   (test "(round 2.5)" (round 2.5) 2.0)
   (test "(round 3.5)" (round 3.5) 4.0)
   (test "(round 4.5)" (round 4.5) 4.0)
   (test "(round -2.5)" (round -2.5) -2.0)
   (test "(round -3.5)" (round -3.5) -4.0)
   (test "(round -4.5)" (round -4.5) -4.0)
   (test "(round 1/2)" (round 1/2) 0)
   (test "(round 3/2)" (round 3/2) 2)
   (test "(round -1/2)" (round -1/2) 0)
   (test "(round -3/2)" (round -3/2) -2)

   (test "(truncate 1)" (truncate 1) 1)
   (test "(truncate -1)" (truncate -1) -1)
   (test "(truncate 1.0)" (truncate 1.0) 1.0)
   (test "(truncate -1.0)" (truncate -1.0) -1.0)
   (test "(truncate 1.5)" (truncate 1.5) 1.0)
   (test "(truncate -1.5)" (truncate -1.5) -1.0)
   (test "(truncate 1/2)" (truncate 1/2) 0)
   (test "(truncate -1/2)" (truncate -1/2) 0)
   (test "(truncate 3/2)" (truncate 3/2) 1)
   (test "(truncate -3/2)" (truncate -3/2) -1)

   ; FIXME: want some bigger numbers, too
   (test "(floor 1)" (floor 1) 1)
   (test "(floor -1)" (floor -1) -1)
   (test "(floor 1.5)" (floor 1.5) 1.0)
   (test "(floor -1.5)" (floor -1.5) -2.0)
   (test "(floor 1/2)" (floor 1/2) 0)
   (test "(floor -1/2)" (floor -1/2) -1)
   (test "(floor 3/2)" (floor 3/2) 1)
   (test "(floor -3/2)" (floor -3/2) -2)

   ; FIXME: want some bigger numbers, too.
   (test "(ceiling 1)" (ceiling 1) 1)
   (test "(ceiling -1)" (ceiling -1) -1)
   (test "(ceiling 1.5)" (ceiling 1.5) 2.0)
   (test "(ceiling -1.5)" (ceiling -1.5) -1.0)
   (test "(ceiling 1/2)" (ceiling 1/2) 1)
   (test "(ceiling -1/2)" (ceiling -1/2) 0)
   (test "(ceiling 3/2)" (ceiling 3/2) 2)
   (test "(ceiling -3/2)" (ceiling -3/2) -1)

   ))


; Bitwise operations on fixnums, only.

(define (test-bit-operations)
  (test-bit-operations-help fxlogand fxlogior fxlogxor fxlognot fxlsh fxrshl fxrsha))

(define (test-bit-operations-help fxlogandp fxlogiorp fxlogxorp fxlognotp fxlshp fxrshlp fxrshap)

  (define (test-logand)
    (allof "fxlogand"
      (test "(fxlogand #x33 #x55)" (fxlogand #x33 #x55) #x11)
      (test "(fxlogand 536870911 1)" (fxlogand 536870911 1) 1)
      (test "(fxlogand -536870912 1)" (fxlogand -536870912 1) 0)

      (test "(fxlogandp #x33 #x55)" (fxlogandp #x33 #x55) #x11)
      (test "(fxlogandp 536870911 1)" (fxlogandp 536870911 1) 1)
      (test "(fxlogandp -536870912 1)" (fxlogandp -536870912 1) 0)))

  (define (test-logior)
    (allof "fxlogior"
      (test "(fxlogior 3 5)" (fxlogior 3 5) 7)
      (test "(fxlogior 536870911 1)" (fxlogior 536870911 1) 536870911)
      (test "(fxlogior -536870912 1)" (fxlogior -536870912 1) -536870911)

      (test "(fxlogiorp 3 5) 7)" (fxlogiorp 3 5) 7)
      (test "(fxlogiorp 536870911 1)" (fxlogiorp 536870911 1) 536870911)
      (test "(fxlogiorp -536870912 1)" (fxlogiorp -536870912 1) -536870911)))

  (define (test-logxor)
    (allof "fxlogxor"
      (test "(fxlogxor #x33 #x55)" (fxlogxor #x33 #x55) #x66)
      (test "(fxlogxor 536870911 1)" (fxlogxor 536870911 1) 536870910)
      (test "(fxlogxor -536870912 1)" (fxlogxor -536870912 1) -536870911)

      (test "(fxlogxorp #x33 #x55)" (fxlogxorp #x33 #x55) #x66)
      (test "(fxlogxorp 536870911 1)" (fxlogxorp 536870911 1) 536870910)
      (test "(fxlogxorp -536870912 1)" (fxlogxorp -536870912 1) -536870911)))

  (define (test-lognot)
    (allof "fxlognot" 
      (test "(fxlognot 0)" (fxlognot 0) -1)
      (test "(fxlognot -1)" (fxlognot -1) 0)
      (test "(fxlognot 1)" (fxlognot 1) -2)
      (test "(fxlognot 536870911)" (fxlognot 536870911) -536870912)
      (test "(fxlognot -536870912)" (fxlognot -536870912) 536870911)

      (test "(fxlognotp 0)" (fxlognotp 0) -1)
      (test "(fxlognotp -1)" (fxlognotp -1) 0)
      (test "(fxlognotp 1)" (fxlognotp 1) -2)
      (test "(fxlognotp 536870911)" (fxlognotp 536870911) -536870912)
      (test "(fxlognotp -536870912)" (fxlognotp -536870912) 536870911)))

  ; FIXME: need to implement tests on lshp, etc.

  (define (test-shift)
    (allof "shift"
     (test "(= (fxlsh #x44 2) #x110)" (= (fxlsh #x44 2) #x110) #t)
     (test "(= (fxlsh #x44 4) #x440)" (= (fxlsh #x44 4) #x440) #t)
     (test "(= (fxrshl #x44 2) #x11)" (= (fxrshl #x44 2) #x11) #t)
     (test "(= (fxrshl #x44 7) 0)" (= (fxrshl #x44 7) 0) #t)
     (test "(= (fxrsha -1 4) -1)" (= (fxrsha -1 4) -1) #t)
     (test "(= (fxrshl #x-20000000 4) #x2000000)"
	   (= (fxrshl #x-20000000 4) #x2000000)
	   #t)))

  (test-logand)
  (test-logior)
  (test-logxor)
  (test-lognot)
  (test-shift))


(define (test-bignum-arithmetic)
  (let ((a 1234567890)
	(b 3141598765)
	(add (lambda (a b) (+ a b)))
	(sub (lambda (a b) (- a b)))
	(mul (lambda (a b) (* a b)))
	(div (lambda (a b) (quotient a b)))
	(mod (lambda (a b) (remainder a b))))

    (allof "bignum arithmetic: conversion to string"
     (test "(string=? (number->string 1234567890 10) \"1234567890\")"
	   (string=? (number->string 1234567890 10) "1234567890")
	   #t)
     )
    (allof "bignum arithmetic: add/subtract"
     (test "(bignum-add a b)" (add a b) 4376166655)
     (test "(bignum-add b a)" (add b a) 4376166655)
     (test "(bignum-subtract a b)" (sub a b) -1907030875)
     (test "(bignum-subtract b a)" (sub b a) 1907030875)
     (test "(bignum-subtract a a)" (sub a a) 0)
     )
    
     ; tests contagion: fixnum * bignum

    (allof "bignum arithmetic: bignum/fixnum contagion"
     (test "(* 42 a)" (mul 42 a) 51851851380)
     (test "(* a 42)" (mul a 42) 51851851380)
     (test "(* -42 a)" (mul -42 a) -51851851380)
     (test "(* a -42)" (mul a -42) -51851851380)
     )

     ; heavier stuff (already bignums)

     (allof "bignum arithmetic: multiply"
      (test "(bignum-multiply a a)" (mul a a) 1524157875019052100)
      (test "(bignum-multiply b b)" (mul b b) 9869642800249525225)
      (test "(bignum-multiply a b)" (mul a b) 3878516958532655850)
      (test "(bignum-multiply b a)" (mul b a) 3878516958532655850)
      )

     (allof "bignum arithmetic: division"
      (test "(bignum-quotient a b)" (div a b) 0)
      (test "(bignum-quotient b a)" (div b a) 2)
      (test "(bignum-quotient a a)" (div a a) 1)
      (test "(bignum-remainder a b)" (mod a b) a)
      (test "(bignum-remainder b a)" (mod b a) 672462985)
      (test "(bignum-remainder a a)" (mod a a) 0)
      )

     ))

(define (test-exactness-predicates)

  (define (etest n)
    (list (exact? n) (inexact? n)))

  (allof "exactness predicates"
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
  (allof "exactness conversion"
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
   (test "(exact->inexact 14285714285714285714285)"
	 (exact->inexact 14285714285714285714285)
	 1.4285714285714286e22)
   ))

(define (test-number-constructors-and-accessors)

  (define (p x)
    (list (integer? x) (rational? x) (real? x) (complex? x)))

  (define (mkrat a b)
    (/ a b))

  (allof "rational operations"
   (test "(numerator 37)" (numerator 37) 37)
   (test "(denominator 37)" (denominator 37) 1)
   (test "(numerator 1/2)" (numerator 1/2) 1)
   (test "(denominator 1/2)" (denominator 1/2) 2)
   (test "(numerator 0.5)" (numerator 0.5) 1.0)
   (test "(denominator 0.5)" (denominator 0.5) 2.0)
   (test "(mkrat 1 2)" (mkrat 1 2) 1/2)
   (test "(mkrat 1.0 2.0)" (mkrat 1.0 2.0) 0.5)
   (test "(mkrat 6 8)" (mkrat 6 8) 3/4)
   )

  ; Complexes
  ; FIXME: should be more exhaustive here
  ; FIXME: should try some larger numbers
  (allof "complex numbers"
   (test "(p (make-rectangular 1 1))" (p (make-rectangular 1 1))
	 '(#f #f #f #t))
   (test "(p (make-rectangular 1.0 1.0))" (p (make-rectangular 1.0 1.0))
	 '(#f #f #f #t))
   (test "(p (make-polar 1.0 1))" (p (make-polar 1.0 1)) '(#f #f #f #t))
   (test "(make-rectangular 1.0 2.0)" (make-rectangular 1.0 2.0) 1.0+2.0i)
   (test "(make-rectangular 1 3/2)" (make-rectangular 1 3/2) 1+3/2i)
   (test "(real-part 1+3i)" (real-part 1+3i) 1)
   (test "(imag-part 1+3i)" (imag-part 1+3i) 3)
   (test "(real-part 5.0e3+7.5i)" (real-part 5.0e3+7.5i) 5000.0)
   (test "(imag-part 5.0e3+7.5i)" (imag-part 5.0e3+7.5i) 7.5)
   (test "(magnitude 1@3)"
         (<= 0.9999999999999999 (magnitude 1@3) 1.0000000000000001)
         #t)
   (test "(angle 1@3)"
         (<= 2.9999999999999997 (angle 1@3)     3.0000000000000003)
         #t))
  )



(define (test-number-ordering-predicates/mixed-representation)

  (define (p a b)
    (list (= a b) (> a b) (>= a b) (< a b) (<= a b)))

  (allof "mixed-representation =,>,>=,<,<="

   (test "(p 1 1234567890)" (p 1 1234567890) '(#f #f #f #t #t))
   (test "(p 1 -987654321098765)" (p 1 -987654321098765) '(#f #t #t #f #f))
   (test "(p -11 -987654321098765)" (p 1 -987654321098765) '(#f #t #t #f #f))
   (test "(p 45 91/2)" (p 45 91/2) '(#f #f #f #t #t))
   (test "(p 1 (/ (expt 2 1000) (+ (expt 2 1000) 1)))"
         (p 1 (/ (expt 2 1000) (+ (expt 2 1000) 1)))
         '(#f #t #t #f #f))
   (test "(p 1 (/ (expt 2 1000) (- (expt 2 1000) 1)))"
         (p 1 (/ (expt 2 1000) (- (expt 2 1000) 1)))
         '(#f #f #f #t #t))
   (test "(p 14 14.0)" (p 14 14.0) '(#t #f #t #f #t))
   (test "(p 12 3.1)" (p 12 3.1) '(#f #t #t #f #f))
   (test "(p 5 1e200)" (p 5 1e200) '(#f #f #f #t #t))
   (test "(p 8 918273645+0i)" (p 8 918273645+0i) '(#f #f #f #t #t))
   (test "(p 24 3+0.0i)" (p 24 3+0.0i) '(#f #t #t #f #f))

   (test "(p 1234567890 0)" (p 1234567890 0) '(#f #t #t #f #f))
   (test "(p -987654321098765 5)" (p -987654321098765 5) '(#f #f #f #t #t))
   (test "(p -987654321098765 -7)" (p -987654321098765 -7) '(#f #f #f #t #t))
   (test "(p 1234567890 12345678901/10)"
         (p 1234567890 12345678901/10) '(#f #f #f #t #t))
   (test "(p (expt 2 50) (/ (expt 2 1000) (+ (expt 2 950) 1)))"
         (p (expt 2 50) (/ (expt 2 1000) (+ (expt 2 950) 1)))
         '(#f #t #t #f #f))
   (test "(p (expt 2 50) (/ (expt 2 1000) (- (expt 2 950) 1)))"
         (p (expt 2 50) (/ (expt 2 1000) (- (expt 2 950) 1)))
         '(#f #f #f #t #t))
   (test "(p 9876543210 9876543210.0)" (p 9876543210 9876543210.0)
         '(#t #f #t #f #t))
   (test "(p 9876543210 9876543210.0001)" (p 9876543210 9876543210.0001)
         '(#f #f #f #t #t))
   (test "(p -9876543210 -9876543210.0001)" (p -9876543210 -9876543210.0001)
         '(#f #t #t #f #f))
   ; assumes double precision
   (test "(p (expt 10 100) 1e100)" (p (expt 10 100) 1e100) '(#f #f #f #t #t))
   (test "(p 9876543210 9876543210+0i)" (p 9876543210 9876543210+0i)
         '(#t #f #t #f #t))
   (test "(p 9876543210 9876543210.0+0.0i)" (p 9876543210 9876543210.0+0.0i)
         '(#t #f #t #f #t))

   (test "(p 3/4 0)" (p 3/4 0) '(#f #t #t #f #f))
   (test "(p 12345678901/10 1234567890)" (p 12345678901/10 1234567890)
         '(#f #t #t #f #f))
   (test "(p 12345678901/10 1234567890.2)" (p 12345678901/10 1234567890.2)
         '(#f #f #f #t #t))
   (test "(p 12345678901/10 1234567889+0i)" (p 12345678901/10 1234567889+0i)
         '(#f #t #t #f #f))
   (test "(p 12345678901/10 1234567890.2+0.0i)"
         (p 12345678901/10 1234567890.2+0.0i)
         '(#f #f #f #t #t))

   (test "(p 1.0000001 1)" (p 1.0000001 1) '(#f #t #t #f #f))
   (test "(p 9.0 9000001/1000000)" (p 9.0 9000001/1000000) '(#f #f #f #t #t))
   (test "(p 1e23 (expt 10 23))" (p 1e23 (expt 10 23)) '(#f #f #f #t #t))
   (test "(p 4.56 (/ (- #e1e1000 1) #e1e1000))"
         (p 4.56 (/ (- #e1e1000 1) #e1e1000))
         '(#f #t #t #f #f))
   #t
   ))

(define (test-odd-even)
;  (allof "odd? and even?"
   ; easy cases
  (begin
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
   (mustfail "(even? 3/4)" (lambda () (even? 3/4)))
   (mustfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (mustfail "(even? 1.1)" (lambda () (even? 1.1)))
   (mustfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (mustfail "(even? 1+3i)" (lambda () (even? 1+3i)))
   (mustfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (mustfail "(even? 'foo)" (lambda () (even? 'foo)))
   (mustfail "(odd? 'foo)" (lambda () (odd? 'foo)))
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
   (mustfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (mustfail "(odd? 3/4)" (lambda () (odd? 3/4)))
   (mustfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (mustfail "(odd? 1.1)" (lambda () (odd? 1.1)))
   (mustfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (mustfail "(odd? 1+3i)" (lambda () (odd? 1+3i)))
   (mustfail "(odd? 'foo)" (lambda () (odd? 'foo)))
   (mustfail "(odd? 'foo)" (lambda () (odd? 'foo)))
   ))

(define (test-sundry-arithmetic)

  (define big 4294967296)
  (define two^32 4294967296)
  (define inexact-two^16 65536.0)

  (define (square x)
    (* x x))

  (define (within-tolerance x ans t)
    (<= (abs (- x ans)) t))

  (allof "max, min, modulo, abs, gcd, lcm, rationalize, sqrt, expt"
   (test "(max 1 2 3)" (max 1 2 3) 3)
   (test "(max 3 2 1)" (max 3 2 1) 3)
   (test "(max 1.0 3.0 2.0)" (max 1.0 3.0 2.0) 3.0)
   (test "(max 1.0 2 3)" (max 1.0 2 3) 3.0)
   (test "(max 1/2 2/3 3/4 4/5)" (max 1/2 2/3 3/4 4/5) 4/5)
   (test "(max 1/2 2/3 0.75 4/5)" (max 1/2 2/3 0.75 4/5) 0.8)

   (test "(min 1 2 3)" (min 1 2 3) 1)
   (test "(min 3 2 1)" (min 3 2 1) 1)
   (test "(min 1.0 3.0 2.0)" (min 1.0 3.0 2.0) 1.0)
   (test "(min 1 2 3.0)" (min 1 2 3.0) 1.0)
   (test "(min 1/2 2/3 3/4 4/5)" (min 1/2 2/3 3/4 4/5) 1/2)
   (test "(min 1/2 2/3 0.75 4/5)" (min 1/2 2/3 0.75 4/5) 0.5)
   (test "(min -1/2 2/3)" (min -1/2 2/3) -1/2)

   (test "(abs 1)" (abs 1) 1)
   (test "(abs -1)" (abs -1) 1)
   (test "(abs big)" (abs big) big)
   (test "(abs (- big))" (abs (- big)) big)
   (test "(abs 1.0)" (abs 1.0) 1.0)
   (test "(abs -1.0)" (abs -1.0) 1.0)
   (test "(abs 79/80)" (abs 79/80) 79/80)
   (test "(abs -79/80)" (abs -79/80) 79/80)

   ; FIXME: square root of complexes -- hard to test, because we
   ; lose some precision along the way.  Should test whether the answer
   ; is within some tolerance, but how do we compute the tolerance?

   (test "(sqrt 4)" (sqrt 4) 2.0)	       ; Always inexact
   (test "(sqrt 16.0)" (sqrt 16.0) 4.0)
   (test "(sqrt -4)" (sqrt -4) 0.0+2.0i)
   (test "(sqrt -16.0)" (sqrt -16.0) 0.0+4.0i)
   (test "(sqrt two^32)" (sqrt two^32) inexact-two^16)
   (test "(sqrt 0.9...)" (sqrt 0.9999999999999999) 0.9999999999999999)
   (test "(gcd 33495 1085)" (gcd 33495 1085) 35)
   (test "(gcd 1085 33495)" (gcd 1085 33495) 35)
   (test "(gcd 33495 96577)" (gcd 33495 96577) 1)
   (test "(gcd 96577 33495)" (gcd 96577 33495) 1)

   (test "(lcm 90 13717421)" (lcm 90 13717421) 1234567890)
   (test "(lcm 90 90)" (lcm 90 90) 90)

   ; FIXME: rationalize
   ; FIXME: expt
   ; FIXME: modulo
   ; FIXME: all complex arithmetic!

   ))

; FIXME: implement more

(define (test-in-out-conversion)
  (allof  "number->string, string->number"
	  #t))


; Based on glibc's tests
(define (test-trancendental-functions)
  (define (flonum-nan? x) ;; should I be checking the real and the imaginary parts?
    (not (= x x))) ;; (this is a trick pnkfelix copied out of src/Lib/Common/flonums.sch)

  (define (eql? x y)
    (or (and (not (zero? x)) (not (zero? y)) (= x y))
        ;; Explicitly check sign of 0.0
        (and (zero? x) (zero? y) (= (/ 1.0 x) (/ 1.0 y)))
        (and (flonum-nan? x)
             (flonum-nan? y))))
  (define (fuzzy-eql?? epsilon)
    (lambda (x y)
      (cond 
       ((and (flonum-nan? x) (flonum-nan? y))
        #t)
       ((or  (flonum-nan? x) (flonum-nan? y))
        #f)
       ((= x y) ;; this catches the infinities (can't take the difference of two inf's)
        #t)
       (else
        (< (/ (magnitude (- (inexact->exact x) 
                            (inexact->exact y)))
              (inexact->exact y)) epsilon)))))

  (parameterize ((test-equivalence (fuzzy-eql?? 1e-15)))
    ;; (allof "exp, log, sin, cos, tan, asin, acos, atan, atan2" #t)
    ;; FIXME: did Lars really mean to include atan2 above?  We don't export that...

    (parameterize ((test-equivalence eql?))
      (allof "exp 1"
       (test "(exp  +0.0)"   (exp  +0.0)    1.0)
       (test "(exp  -0.0)"   (exp  -0.0)    1.0)
       (test "(exp  +inf.0)" (exp  +inf.0) +inf.0)
       (test "(exp  -inf.0)" (exp  -inf.0)  0.0)
       (test "(exp  +nan.0)" (exp  +nan.0) +nan.0)))
    (allof "exp 2"
     (test "(exp   1.0)"   (exp   1.0)   2.718281828459045)
     (test "(exp   2.0)"   (exp   2.0)   7.38905609893065)
     (test "(exp   3.0)"   (exp   3.0)  20.085536923187668)
     (test "(exp   0.75)"  (exp   0.75)  2.117000016612675)
     (test "(exp  50.0)"   (exp 50.0)    5.184705528587072e+21))

    (allof "log"
     ;(test "(log  +0.0)"   (log  +0.0)   -inf.0) ;; Larceny throws Domain
     ;(test "(log  -0.0)"   (log  -0.0)   -inf.0) ;; Error exceptions on these
     (test "(log   1.0)"   (log   1.0)    0.0)
     (test "(log  -1.0)"   (log  -1.0)    0.0+3.141592653589793i)
     (test "(log  +inf.0)" (log  +inf.0) +inf.0)
     (test "(log   e)"     (log   2.718281828459045)    1.0)
     (test "(log   1/e)"   (log   0.36787944117144233) -1.0)
     (test "(log   2.0)"   (log   2.0)                  0.6931471805599453)
     (test "(log  10.0)"   (log  10.0)                  2.302585092994046)
     (test "(log   0.75)"  (log   0.75)                -0.2876820724517809)
     )

    (parameterize ((test-equivalence eql?))
      (allof "sin 1"
       (test "(sin  +0.0)"   (sin  +0.0)   +0.0)
       (test "(sin  -0.0)"   (sin  -0.0)   -0.0)
       (test "(sin  +inf.0)" (sin  +inf.0) +nan.0)
       (test "(sin  -inf.0)" (sin  -inf.0) +nan.0)
       (test "(sin  +nan.0)" (sin  +nan.0) +nan.0)))

    (allof "sin 2"
     (test "(sin   pi/6)"  (sin  0.5235987755982988)  0.49999999999999994)
     (test "(sin  -pi/6)"  (sin -0.5235987755982988) -0.49999999999999994)
     (test "(sin   pi/2)"  (sin  1.5707963267948966)  1.0)
     (test "(sin   pi/2)"  (sin -1.5707963267948966) -1.0)
     (test "(sin   0.75)"  (sin  0.75)                0.6816387600233341))

    (parameterize ((test-equivalence eql?))
      (allof "cos 1"
       (test "(cos   0.0)"   (cos   0.0)    1.0)
       (test "(cos  -0.0)"   (cos  -0.0)    1.0)
       (test "(cos  +inf.0)" (cos  +inf.0) +nan.0)
       (test "(cos  -inf.0)" (cos  -inf.0) +nan.0)))

    (allof "cos 2"
     (test "(cos  pi/3)"   (cos 1.0471975511965976)  0.5000000000000001)
     (test "(cos  2pi/3)"  (cos 2.0943951023931953) -0.4999999999999998)
     (test "(cos  2pi)"    (cos 6.283185307179586)   1.0)
     (test "(cos  0.75)"   (cos 0.75)                0.7316888688738209))

    (parameterize ((test-equivalence eql?))
      (allof "tan 1"
       (test "(tan   0.0)"   (tan   0.0)    0.0)
       (test "(tan  -0.0)"   (tan  -0.0)   -0.0)
       (test "(tan  +inf.0)" (tan  +inf.0) +nan.0)
       (test "(tan  -inf.0)" (tan  -inf.0) +nan.0)
       (test "(tan  +nan.0)" (tan  +nan.0) +nan.0)))

    (allof "tan 2"
     (test "(tan   pi/4)"  (tan   0.7853981633974483) 0.9999999999999999)
     (test "(tan   0.75)"  (tan   0.75)   0.9315964599440725))

    (parameterize ((test-equivalence eql?))
      (allof "asin 1"
       (test "(asin +inf.0)" (asin +inf.0) +nan.0)
       (test "(asin -inf.0)" (asin -inf.0) +nan.0)
       (test "(asin +nan.0)" (asin +nan.0) +nan.0)
       ;;(asin +9/8) ;; this may be too fuzzy to test well?
       ;;(asin -9/8) ;; this may be too fuzzy to test well?
       (test "(asin +0.0)"   (asin +0.0)   +0.0)
       (test "(asin -0.0)"   (asin -0.0)   -0.0)))

    (allof "asin 2"
     (test "(asin  0.5)"   (asin  0.5)    0.5235987755982989)
     (test "(asin -0.5)"   (asin -0.5)   -0.5235987755982989)
     (test "(asin  1.0)"   (asin  1.0)    1.5707963267948966)
     (test "(asin  0.75)"  (asin  0.75)   0.848062078981481))
    
    (parameterize ((test-equivalence eql?))
      (allof "acos 1"
       (test "(acos +inf.0)" (acos +inf.0) +nan.0)
       (test "(acos -inf.0)" (acos -inf.0) +nan.0)
       (test "(acos +nan.0)" (acos +nan.0) +nan.0)))

    (allof "acos 2"     
     ;;(acos +9/8) ;; this may be too fuzzy to test well?
     ;;(acos -9/8) ;; this may be too fuzzy to test well?
     (test "(acos +0.0)"   (acos +0.0)   1.5707963267948966)
     (test "(acos -0.0)"   (acos -0.0)   1.5707963267948966)
     (test "(acos  1)"     (acos  1)     0.0)
     (test "(acos -1)"     (acos -1)     3.141592653589793)
     (test "(acos  0.5)"   (acos  0.5)   1.0471975511965979)
     (test "(acos -0.5)"   (acos -0.5)   2.0943951023931957)
     (test "(acos  0.75)"  (acos  0.75)  0.7227342478134157)
     (test "(acos  2e-17)" (acos  2e-17) 1.5707963267948966)
     (test "(acos  0.0625)"(acos  0.0625)1.5082555649984053) ; NB. glibc wants 1.50825556499840522843072005474337068L
     )
    
    ;; pnkfelix doesn't trust glibc's tests here...
    '(allof "acos complex"
     (test "(acos +0.0+0.0i)" (acos +0.0+0.0i) 1.5707963267948966-0.0i)
     (test "(acos -0.0+0.0i)" (acos -0.0+0.0i) 1.5707963267948966-0.0i)
     (test "(acos -0.0-0.0i)" (acos -0.0-0.0i) 1.5707963267948966+0.0i)
     (test "(acos +0.0-0.0i)" (acos +0.0-0.0i) 1.5707963267948966+0.0i)
     ...)
    
    (parameterize ((test-equivalence eql?))
      (allof "atan 1"
       (test "(atan  0)"     (atan  0)     0.0)
       (test "(atan +0.0)" (atan  0.0)    +0.0)
       (test "(atan -0.0)" (atan -0.0)    -0.0)))
    (allof "atan 2"
     (test "(atan +inf.0)" (atan +inf.0) 1.5707963267948966)
     (test "(atan -inf.0)" (atan -inf.0)-1.5707963267948966)
     (test "(atan +nan.0)" (atan +nan.0) +nan.0)
     (test "(atan  1.0)"   (atan  1.0)   0.7853981633974483)
     (test "(atan -1.0)"   (atan -1.0)  -0.7853981633974483)
     (test "(atan  0.75)"  (atan  0.75)  0.6435011087932844))

   ))

; eof
