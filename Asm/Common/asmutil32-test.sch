; Asm/Common/asmutil32-test.sch
; Larceny assembler -- test code for 32-bit asm utilities.
;
; $Id$
;
; This code is endianness-independent.
;
; Bugs
; * IMPORTANT: This code should automatically check the results rather 
;   than rely on vgrep.
; * Should probably add some checks that depend on endianness.
; * Need test cases for
;      asm:bv->int
;      asm:add
;      asm:signed
;   at least.

(define (asm:test-util32)

  (define (prnx bv)
    (asm:print-bv bv))

  (define (prn bv)
    (if (eq? asm:endianness 'little)
	(do ((i 3 (- i 1)))
	    ((< i 0))
	  (display (bytevector-ref bv i))
	  (display " "))
	(do ((i 0 (+ i 1)))
	    ((= i 4))
	  (display (bytevector-ref bv i))
	  (display " "))))

  (define (printbv msg bv)
    (display "(")
    (display msg)
    (display ") = ")
    (prn bv)
    (newline))
  
  (define (printbvx msg bv)
    (display "(")
    (display msg)
    (display ") = ")
    (prnx bv)
    (newline))
  
  (define (printst caption answer result)
    (display caption)
    (display " should be ")
    (write answer)
    (display " and is ")
    (write result)
    (newline)
    (if (not (equal? answer result))
	(begin (display "WARNING!! Failed.")
	       (newline))))

  (define (testbv msg result answer)
    (if (not (equal? result answer))
	(begin (display "FAILED: ")
	       (display " ")
	       (display msg)
	       (display "; result=")
	       (prnx result)
	       (display ", answer=")
	       (prn answer)
	       (newline)
	       #f)
	#t))

  (define (testbv3 msg input result answer)
    (if (not (equal? result answer))
	(begin (display "FAILED: ")
	       (display " ")
	       (display msg)
	       (display "; input=")
	       (prnx input)
	       (display "; result=")
	       (prnx result)
	       (display ", answer=")
	       (prnx answer)
	       (newline)
	       #f)
	#t))

  (define (test-construction)
    (display "CONSTRUCT") (newline)
    (printbv "bv 1 2 3 4" (asm:bv 1 2 3 4))
    (printbvx "bv 255 255 255 255" (asm:bv 255 255 255 255))
    (and 
     (testbv "int->bv 0" (asm:int->bv 0) (asm:bv 0 0 0 0))
     (testbv "int->bv 1" (asm:int->bv 1) (asm:bv 0 0 0 1))
     (testbv "int->bv 1024" (asm:int->bv 1024) (asm:bv 0 0 4 0))
     (testbv "int->bv -1"
	     (asm:int->bv -1)
	     (asm:bv 255 255 255 255))
     (testbv "int->bv -1024"
	     (asm:int->bv -1024)
	     (asm:bv #xFF #xFF #xFC 0))
     (testbv "int->bv 2^31-1"
	     (asm:int->bv (- (expt 2 31) 1))
	     (asm:bv 127 255 255 255))
     (testbv "int->bv -2^31"
	     (asm:int->bv (expt -2 31))
	     (asm:bv 128 0 0 0))
     ))

  (define (test-logior)
    (and 
     (testbv "logior 0" (asm:logior 0) (asm:bv 0 0 0 0))
     (testbv "logior 256" (asm:logior 256) (asm:bv 0 0 1 0))
     (testbv "logior 256 255"
	     (asm:logior 256 255)
	     (asm:bv 0 0 1 255))
     (testbv "logior (bv 1 2 3 4) (bv 4 3 2 1)"
	     (asm:logior (asm:bv 1 2 3 4) (asm:bv 4 3 2 1))
	     (asm:bv 5 3 3 5))
     (testbv "logior 2 4 8 16 32"
	     (asm:logior 2 4 8 16 32)
	     (asm:bv 0 0 0 (+ 2 4 8 16 32)))
     ))

  (define (test-logand)
    (and 
     (testbv "logand 0 2^31-1"
	     (asm:logand 0 (- (expt 2 31) 1))
	     (asm:bv 0 0 0 0))
     (testbv "logand #xffff #xf0f0"
	     (asm:logand #xffff #xf0f0)
	     (asm:bv 0 0 #xF0 #xF0))
     (testbv "logand (bv 1 2 3 4) (bv 4 3 2 1)"
	     (asm:logand (asm:bv 1 2 3 4) (asm:bv 4 3 2 1))
	     (asm:bv 0 2 2 0))
     ))

  (define (test-lsh)
    (let ((bv (asm:bv 1 2 3 4)))
      (and
       (testbv3 "shift left 0 bits" bv (asm:lsh bv 0) bv)
       (testbv3 "shift left 1 bit" bv (asm:lsh bv 1) (asm:bv 2 4 6 8))
       (testbv3 "shift left 8 bits" bv (asm:lsh bv 8) (asm:bv 2 3 4 0))
       (testbv3 "shift left 9 bits" bv (asm:lsh bv 9) (asm:bv 4 6 8 0))
       (testbv3 "shift left 17 bits" bv (asm:lsh bv 17) (asm:bv 6 8 0 0))
       (testbv3 "shift left 25 bits" bv (asm:lsh bv 25) (asm:bv 8 0 0 0))
       (testbv3 "shift left 32 bits" bv (asm:lsh bv 32) (asm:bv 0 0 0 0))
       )))

  (define (test-rshl)
    (let ((bv (asm:bv 128 2 3 4)))
      (and 
       (testbv3 "lshift right 0 bits" bv (asm:rshl bv 0) bv)
       (testbv3 "lshift right 1 bit"  bv (asm:rshl bv 1) (asm:bv 64 1 1 130))
       (testbv3 "lshift right 8 bits" bv (asm:rshl bv 8) (asm:bv 0 128 2 3))
       (testbv3 "lshift right 9 bits" bv (asm:rshl bv 9) (asm:bv 0 64 1 1))
       (testbv3 "lshift right 17 bits" bv (asm:rshl bv 17) (asm:bv 0 0 64 1))
       (testbv3 "lshift right 26 bits" bv (asm:rshl bv 26) (asm:bv 0 0 0 32))
       (testbv3 "lshift right 32 bits" bv (asm:rshl bv 32) (asm:bv 0 0 0 0))
       (let ((bv (asm:bv 255 255 255 255)))
	 (testbv3 "lshift right 32 bits" bv (asm:rshl bv 32) (asm:bv 0 0 0 0)))
       )))

  (define (test-rsha)
    (and (test-rsha1)
	 (test-rsha2)))

  (define (test-rsha1)
    (let ((bv (asm:bv 128 2 3 4)))
      (and 
       (testbv3 "ashift right 0 bits" bv (asm:rsha bv 0) bv)
       (testbv3 "ashift right 1 bit" bv (asm:rsha bv 1) (asm:bv #xc0 1 1 130))
       (testbv3 "ashift right 8 bits" bv (asm:rsha bv 8) (asm:bv #xff 128 2 3))
       (testbv3 "ashift right 9 bits" bv
		(asm:rsha bv 9) (asm:bv #xff #xc0 1 1))
       (testbv3 "ashift right 17 bits" bv
		(asm:rsha bv 17) (asm:bv #xff #xff #xc0 1))
       (testbv3 "ashift right 25 bits" bv
		(asm:rsha bv 25) (asm:bv #xff #xff #xff #xc0))
       (testbv3 "ashift right 32 bits" bv
		(asm:rsha bv 32) (asm:bv #xff #xff #xff #xff))
       )))

  (define (test-rsha2)
    (let ((bv (asm:bv 1 2 3 4)))
      (and 
       (testbv3 "ashift right 0 bits" bv (asm:rsha bv 0) bv)
       (testbv3 "ashift right 1 bit" bv (asm:rsha bv 1) (asm:bv 0 129 1 130))
       (testbv3 "ashift right 8 bits" bv (asm:rsha bv 8) (asm:bv 0 1 2 3))
       (testbv3 "ashift right 9 bits" bv (asm:rsha bv 9) (asm:bv 0 0 129 1))
       (testbv3 "ashift right 17 bits" bv (asm:rsha bv 17) (asm:bv 0 0 0 129))
       (testbv3 "ashift right 25 bits" bv (asm:rsha bv 25) (asm:bv 0 0 0 0))
       (testbv3 "ashift right 32 bits" bv (asm:rsha bv 32) (asm:bv 0 0 0 0))
       )))

  (define (test-lobits) 
    (let ((bv (asm:bv #xf1 #xe6 #x4a #x85)))
      (printbvx "LOBITS" bv)
      (printbvx "lobits 0" (asm:lobits bv 0))
      (printbvx "lobits 5" (asm:lobits bv 5))
      (printbvx "lobits 10" (asm:lobits bv 10))
      (printbvx "lobits 16" (asm:lobits bv 16))
      (printbvx "lobits 32" (asm:lobits bv 32))
      #t))

  (define (test-hibits)
    (let ((bv (asm:bv #xf1 #xe6 #x4a #x85)))
      (printbvx "HIBITS" bv)
      (printbvx "hibits 0" (asm:hibits bv 0))
      (printbvx "hibits 5" (asm:hibits bv 5))
      (printbvx "hibits 10" (asm:hibits bv 10))
      (printbvx "hibits 16" (asm:hibits bv 16))
      (printbvx "hibits 32" (asm:hibits bv 32))
      #t))

  (define (test-fits)
    (display "FITS") (newline)
    (printst "2^12-1 in 13" #t (asm:fits? (- (expt 2 12) 1) 13))
    (printst "2^12 in 13" #f (asm:fits? (expt 2 12) 13))
    (printst "-2^12 in 13" #t (asm:fits? (- (expt 2 12)) 13))
    (printst "-2^12-1 in 13" #f (asm:fits? (- (- (expt 2 12)) 1) 13))
    (printst "2^22-1 in 22" #t (asm:fits-unsigned? (- (expt 2 22) 1) 22))
    (printst "2^22 in 22" #f (asm:fits-unsigned? (expt 2 22) 22))
    (printst "0 in 22" #t (asm:fits-unsigned? 0 22))
    (printst "-1 in 22" #f (asm:fits-unsigned? -1 22))
    #t
    )
    
  (and 
   (test-construction)
   (test-logior)
   (test-logand)
   (test-lsh)
   (test-rshl)
   (test-rsha)
   (test-lobits)
   (test-hibits)
   (test-fits)))

; eof
