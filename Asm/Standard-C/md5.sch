;;; -*- mode: scheme; mode: font-lock -*-
;;; md5.scm  --  Jens Axel Søgaard, 16 oct 2002  

;;; History
; 14-10-2002  
;   - Bored. Initial attempt. Done. Well, except for faulty output.
; 15-10-2002  
;   - It works at last
; 16-10-2002  
;   - Added R5RS support
; 16-02-2003 / lth
;   - Removed let-values implementation because Larceny has it already
;   - Implemented Larceny versions of many bit primitives (note, 0.52 
;     or later required due to bignum bug)
;   - Removed most 'personal idiosyncrasies' to give the compiler a fair 
;     chance to inline primitives and improve performance some.
;     Performance in the interpreter is still really quite awful.
;   - Wrapped entire procedure in a big LET to protect the namespace
;   - Some cleanup of repeated computations
;   - Moved test code to separate file
; 17-02-2003 / lth
;   - Removed some of the indirection, for a 30% speedup in Larceny's
;     interpreter.  Running in the interpreter on my Dell Inspiron 4000
;     I get a fingerprint of "Lib/Common/bignums-be.sch" in about 63ms,
;     which is slow but adequate.

;;; Summary
; This is an implementation of the md5 message-digest algorithm
; in R5RS Scheme. The algorithm takes an arbitrary string and 
; returns a 128-bit "fingerprint". 
; The algorithm was invented by Ron Rivest, RSA Security, INC.
; Reference:  RFC 1321, <http://www.faqs.org/rfcs/rfc1321.html>

;;; Contact
; Email jensaxel@soegaard.net if you have problems,
; suggestions, code for 32 bit arithmetic for your
; favorite implementation.
; Check <http://www.scheme.dk/md5/> for new versions.

;;; Technicalities
; The algorithm is designed to be efficiently implemented
; using 32 bit arithmetic. If your implementation supports
; 32 bit arithmetic directly, you should substitute the
; portable 32 operations with primitives of your implementation.
; See the PLT version below for an example. 

;;; Word aritmetic (32 bit)
; Terminology
;    word:  32 bit unsigned integer
;    byte:   8 bit unsigned integer

(define md5)

(let ()
  (let-syntax ((mod32
		(syntax-rules ()
		  ((mod32 n)
		   (remainder n 4294967296)))))

    ;; word+ : word word -> word
    (define (word+ w1 w2)
      (mod32 (+ w1 w2)))

    (define (word.4+ w1 w2 w3 w4)
      (mod32 (+ w1 w2 w3 w4)))

    (define bitpos 
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

    (define powers 
      '#(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
	   131072 262144 524288 1048576 2097152 4194304 8388608 16777216 33554432
	   67108864 134217728 268435456 536870912 1073741824 2147483648 4294967296))

    ;; word->bits : word -> (list (union 0 1))
    (define (word->bits w)
      (define (bit i)  
	(remainder (quotient w (vector-ref powers i)) 2))
      (map bit bitpos))

    ;; bits->integer : (list (union 0 1)) -> integer
    (define (bits->integer bs)
      (apply + (map * bs (map (lambda (i) (vector-ref powers i)) bitpos))))
  
    ;; map-bitwise (bit -> bit) word word -> word
    (define (map-bitwise f w1 w2)
      (bits->integer (map f (word->bits w1) (word->bits w2))))

    ;; These primitives can be made more efficient by operating directly
    ;; on the bignum representations.

    (define (word-or w1 w2)
      (if (and (fixnum? w1) (fixnum? w2))
	  (logior w1 w2)
	  (+ (* 65536 (logior (quotient w1 65536) (quotient w2 65536)))
	     (logior (remainder w1 65536) (remainder w2 65536)))))

    (define (word-not w)
      (if (fixnum? w)
	  (+ (* 65536 (logand (lognot (rsha w 16)) 65535))
	     (logand (lognot (logand w 65535)) 65535))
	  (- 4294967295 w)))

    (define (word-xor w1 w2)
      (if (and (fixnum? w1) (fixnum? w2))
	  (logxor w1 w2)
	  (+ (* 65536 (logxor (quotient w1 65536) (quotient w2 65536)))
	     (logxor (remainder w1 65536) (remainder w2 65536)))))

    (define (word-and w1 w2)
      (if (and (fixnum? w1) (fixnum? w2))
	  (logand w1 w2)
	  (+ (* 65536 (logand (quotient w1 65536) (quotient w2 65536)))
	     (logand (remainder w1 65536) (remainder w2 65536)))))

    (define (word<<< n s)
      (+ (mod32 (* n (vector-ref powers s)))
	 (quotient n (vector-ref powers (- 32 s)))))

    ;; Bytes and words
    ;; The least significant byte of a word is the first

    ;; bytes->word : (list byte*) -> word
    (define (bytes->word bs)
      (define (bs->w akk mul bs)
	(cond ((null? bs) akk)
	      (else       (bs->w (+ akk (* (car bs) mul)) (* 256 mul) (cdr bs)))))
      (bs->w 0 1 bs))

    ;; word->bytes : word -> "(list byte byte byte byte)"
    (define (word->bytes word)
      (define (extract w i)
	(remainder (quotient w (expt 256 i)) 256))
      (list (extract word 0) (extract word 1) (extract word 2) (extract word 3)))

    ;; bytes->words : (list byte) -> (list word)
    (define (bytes->words bytes)
      (define (loop bs l)
	(cond ((null? l)          (list (bytes->word (reverse bs))))
	      ((< (length bs) 4)  (loop (cons (car l) bs)  (cdr l)))
	      (else               (cons (bytes->word (reverse bs))  (loop '() l)))))
      (if (null? bytes)
	  '()
	  (loop '() bytes)))

    ;; string->bytes : string -> (list byte)
    (define (string->bytes s)
      (map char->integer (string->list s)))

    ;; List Helper
    ;; block/list : list -> (values vector list)
    ;; return a vector of the first 16 elements of the list,
    ;;         and the rest of the list
    (define (block/list l)
      (let* (( v0 (car  l))  ( l0 (cdr l))
	     ( v1 (car l0))  ( l1 (cdr l0))
	     ( v2 (car l1))  ( l2 (cdr l1))
	     ( v3 (car l2))  ( l3 (cdr l2))
	     ( v4 (car l3))  ( l4 (cdr l3))
	     ( v5 (car l4))  ( l5 (cdr l4))
	     ( v6 (car l5))  ( l6 (cdr l5))
	     ( v7 (car l6))  ( l7 (cdr l6))
	     ( v8 (car l7))  ( l8 (cdr l7))
	     ( v9 (car l8))  ( l9 (cdr l8))
	     (v10 (car l9))  (l10 (cdr l9))
	     (v11 (car l10)) (l11 (cdr l10))
	     (v12 (car l11)) (l12 (cdr l11))
	     (v13 (car l12)) (l13 (cdr l12))
	     (v14 (car l13)) (l14 (cdr l13))
	     (v15 (car l14)) (l15 (cdr l14)))
	(values (vector v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
		l15)))

    ;; MD5
    ;; The algorithm consists of five steps.
    ;; All we need to do, is to call them in order.
    ;; md5 : string -> string

    (define (md5-computation str)
      (step5 (step4 (step2 (* 8 (string-length str)) 
			   (step1 (string->bytes str))))))

    ;; Step 1  -  Append Padding Bits
    ;; The message is padded so the length (in bits) becomes 448 modulo 512.
    ;; We allways append a 1 bit and then append the proper numbber of 0's.
    ;; NB: 448 bits is 14 words and 512 bits is 16 words
    ;; step1 : (list byte) -> (list byte)

    (define (step1 message)
      (let ((zero-bits-to-append (modulo (- 448 (* 8 (length message))) 512)))
	(append message 
		(cons #x80		; The byte containing the 1 bit => one less 0 byte to append 
		      (vector->list (make-vector (quotient (- zero-bits-to-append 1) 8) 0))))))

    ;; Step 2  -  Append Length
    ;; A 64 bit representation of the bit length b of the message before
    ;; the padding of step 1is appended. Lower word first.
    ;; step2 : number (list byte) -> (list word)
    ;;  org-len is the length of the original message in number of bits

    (define (step2 org-len padded-message)
      (let* ((b  org-len)
	     (lo (mod32 b))
	     (hi (mod32 (quotient b (vector-ref powers 32)))))
	(bytes->words 
	 (append padded-message 
		 (append (word->bytes lo)
			 (word->bytes hi))))))

    ;; Step 3  -  Initialize MD Buffer
    ;; These magic constants are used to initialize the loop
    ;; in step 4.
    ;;
    ;;          word A: 01 23 45 67
    ;;          word B: 89 ab cd ef
    ;;          word C: fe dc ba 98
    ;;          word D: 76 54 32 10

    ;; Step 4  -  Process Message in 16-Word Blocks
    ;; For each 16 word block, go through a round one to four.
    ;; step4 : (list word) -> "(list word word word word)"

    (define (step4 message)
      (define (loop A B C D message)
	(if (null? message)
	    (list A B C D)
	    (let-values (((X rest) (block/list message)))
	      (let* ((AA A) (BB B) (CC C) (DD D)

		     (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 0) 3614090360) 7)))
		     (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 1) 3905402710) 12)))
		     (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 2) 606105819) 17)))
		     (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 3) 3250441966) 22)))
		     (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 4) 4118548399) 7)))
		     (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 5) 1200080426) 12)))
		     (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 6) 2821735955) 17)))
		     (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 7) 4249261313) 22)))
		     (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 8) 1770035416) 7)))
		     (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 9) 2336552879) 12)))
		     (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 10) 4294925233) 17)))
		     (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 11) 2304563134) 22)))
		     (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 12) 1804603682) 7)))
		     (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 13) 4254626195) 12)))
		     (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 14) 2792965006) 17)))
		     (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 15) 1236535329) 22)))

		     (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 1) 4129170786) 5)))
		     (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 6) 3225465664) 9)))
		     (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 11) 643717713) 14)))
		     (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 0) 3921069994) 20)))
		     (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 5) 3593408605) 5)))
		     (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 10) 38016083) 9)))
		     (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 15) 3634488961) 14)))
		     (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 4) 3889429448) 20)))
		     (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 9) 568446438) 5)))
		     (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 14) 3275163606) 9)))
		     (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 3) 4107603335) 14)))
		     (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 8) 1163531501) 20)))
		     (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 13) 2850285829) 5)))
		     (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 2) 4243563512) 9)))
		     (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 7) 1735328473) 14)))
		     (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 12) 2368359562) 20)))

		     (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 5) 4294588738) 4)))
		     (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 8) 2272392833) 11)))
		     (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 11) 1839030562) 16)))
		     (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 14) 4259657740) 23)))
		     (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 1) 2763975236) 4)))
		     (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 4) 1272893353) 11)))
		     (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 7) 4139469664) 16)))
		     (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 10) 3200236656) 23)))
		     (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 13) 681279174) 4)))
		     (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 0) 3936430074) 11)))
		     (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 3) 3572445317) 16)))
		     (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 6) 76029189) 23)))
		     (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 9) 3654602809) 4)))
		     (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 12) 3873151461) 11)))
		     (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 15) 530742520) 16)))
		     (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 2) 3299628645) 23)))

		     (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 0) 4096336452) 6)))
		     (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 7) 1126891415) 10)))
		     (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 14) 2878612391) 15)))
		     (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 5) 4237533241) 21)))
		     (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 12) 1700485571) 6)))
		     (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 3) 2399980690) 10)))
		     (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 10) 4293915773) 15)))
		     (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 1) 2240044497) 21)))
		     (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 8) 1873313359) 6)))
		     (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 15) 4264355552) 10)))
		     (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 6) 2734768916) 15)))
		     (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 13) 1309151649) 21)))
		     (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 4) 4149444226) 6)))
		     (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 11) 3174756917) 10)))
		     (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 2) 718787259) 15)))
		     (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 9) 3951481745) 21)))

		     (A (word+ A AA)) 
		     (B (word+ B BB))
		     (C (word+ C CC)) 
		     (D (word+ D DD)))
		  (loop A B C D rest)))))
  
      ;; Step 3 :-) (magic constants)
      (loop #x67452301 #xefcdab89 #x98badcfe #x10325476 message))

    ;; Each round consists of the application of the following
    ;; basic functions. They functions on a word bitwise, as follows.
    ;;          F(X,Y,Z) = XY v not(X) Z  (NB: or can be replaced with + in F)
    ;;          G(X,Y,Z) = XZ v Y not(Z)
    ;;          H(X,Y,Z) = X xor Y xor Z
    ;;          I(X,Y,Z) = Y xor (X v not(Z))

    (define (F x y z)
      (word-or (word-and x y) (word-and (word-not x) z)))

    (define (G x y z)
      (word-or (word-and x z) (word-and y (word-not z))))

    (define (H x y z)
      (word-xor x (word-xor y z)))

    (define (II x y z)
      (word-xor y (word-or x (word-not z))))

    ;; Step 5  -  Output
    ;; To finish up, we convert the word to hexadecimal string
    ;; - and make sure they end up in order.
    ;; step5 : "(list word word word word)" -> string

    (define (step5 l)
  
      (define (number->hex n)
	(let ((str (number->string n 16)))
	  (case (string-length str)
	    ((1)  (string-append "0" str))
	    (else str))))
  
      (apply string-append
	     (map number->hex
		  (apply append (map word->bytes l)))))

    (set! md5 md5-computation)))
