; Copyright 1998 Lars T Hansen.
;
; $Id$

(define (run-char-tests)
  (display "Char") (newline)
  (char-predicate-test)
  (char-test-eqv?)
  (char-simple-comparisons)
  (char-more-simple-comparisons #\a)
  (char-tests-for-control)
  (char-more-tests-for-control #\a)
  (char-yet-more-tests-for-control #\a #\b)
  (char-conversion-tests #\a)
  (char-classification-tests)

  ; There is little point to testing Unicode characters
  ; if they aren't supported in the system we're testing.

  (let* ((char-rep (cdr (assq 'char-representation (system-features))))
         (unicode? (eq? char-rep 'unicode)))
    (if unicode?
        (basic-unicode-char-tests))))

(define (char-predicate-test)
  (allof "char?"
   (test "(char? #\\a)" (char? #\a) #t)
   (test "(char? 37)" (char? 37) #f)
   (test "(char? #x26)" (char? #x26) #f)
   (test "(char? 0.0)" (char? 0.0) #f)
   (test "(char? #'())" (char? '#()) #f)
   (test "(char? '(a . b))" (char? '(a . b)) #f)
   (test "(char? \"\")" (char? "") #f)
   (test "(char? \"a\")" (char? "a") #f)
   (test "(char? #t)" (char? #t) #f)
   (test "(char? #f)" (char? #f) #f)
   (test "(char? '())" (char? '()) #f)
   (test "(char? (unspecified))" (char? (unspecified)) #f)
   (test "(char? (undefined))" (char? (undefined)) #f)
   ))
  
(define (char-test-eqv?)
  (define (p a b)
    (eqv? a b))

  (allof "eqv? on characters"
   (test "(p #\\a #\\a)" (p #\a #\a) #t)
   (test "(p #\\a #\\b)" (p #\a #\b) #f)))

(define (char-simple-comparisons)
  (allof "char=?, char<?, char<=?, char>?, char>=?"
   (test "(char=? #\\a #\\a)" (char=? #\a #\a) #t)
   (test "(char=? #\\a #\\b)" (char=? #\a #\b) #f)
   (test "(char>? #\\a #\\a)" (char>? #\a #\a) #f)
   (test "(char>? #\\a #\\b)" (char>? #\a #\b) #f)
   (test "(char>? #\\b #\\a)" (char>? #\b #\a) #t)
   (test "(char>=? #\\a #\\a)" (char>=? #\a #\a) #t)
   (test "(char>=? #\\a #\\b)" (char>=? #\a #\b) #f)
   (test "(char>=? #\\b #\\a)" (char>=? #\b #\a) #t)
   (test "(char<? #\\a #\\a)" (char<? #\a #\a) #f)
   (test "(char<? #\\a #\\b)" (char<? #\a #\b) #t)
   (test "(char<? #\\b #\\a)" (char<? #\b #\a) #f)
   (test "(char<=? #\\a #\\a)" (char<=? #\a #\a) #t)
   (test "(char<=? #\\a #\\b)" (char<=? #\a #\b) #t)
   (test "(char<=? #\\b #\\a)" (char<=? #\b #\a) #f)))

; X is known to have value #\a.

(define (char-more-simple-comparisons x)
  (allof "char=?, char<?, char<=?, char>?, char>=? (again!)"
   (test "(char=? #\\a x)" (char=? #\a x) #t)
   (test "(char=? #\\a x)" (char=? #\b x) #f)
   (test "(char>? #\\a x)" (char>? #\a x) #f)
   (test "(char>? #\\b x)" (char>? #\b x) #t)
   (test "(char>=? #\\a x)" (char>=? #\a x) #t)
   (test "(char>=? #\\b x)" (char>=? #\b x) #t)
   (test "(char<? #\\a x)" (char<? #\a x) #f)
   (test "(char<? #\\b x)" (char<? #\b x) #f)
   (test "(char<=? #\\a x)" (char<=? #\a x) #t)
   (test "(char<=? #\\b x)" (char<=? #\b x) #f)))

(define (char-tests-for-control)
  (allof "char=?, char<?, char<=?, char>?, char>=? for control"
   (test "(if (char=? #\\a #\\b) 0 1)" (if (char=? #\a #\b) 0 1) 1)
   (test "(if (char=? #\\a #\\a) 0 1)" (if (char=? #\a #\a) 0 1) 0)
   (test "(if (char>? #\\a #\\b) 0 1)" (if (char>? #\a #\b) 0 1) 1)
   (test "(if (char>? #\\a #\\a) 0 1)" (if (char>? #\a #\a) 0 1) 1)
   (test "(if (char>? #\\b #\\a) 0 1)" (if (char>? #\b #\a) 0 1) 0)
   (test "(if (char>=? #\\a #\\b) 0 1)" (if (char>=? #\a #\b) 0 1) 1)
   (test "(if (char>=? #\\a #\\a) 0 1)" (if (char>=? #\a #\a) 0 1) 0)
   (test "(if (char>=? #\\b #\\a) 0 1)" (if (char>=? #\b #\a) 0 1) 0)
   (test "(if (char<? #\\a #\\b) 0 1)" (if (char<? #\a #\b) 0 1) 0)
   (test "(if (char<? #\\a #\\a) 0 1)" (if (char<? #\a #\a) 0 1) 1)
   (test "(if (char<? #\\b #\\a) 0 1)" (if (char<? #\b #\a) 0 1) 1)
   (test "(if (char<=? #\\a #\\b) 0 1)" (if (char<=? #\a #\b) 0 1) 0)
   (test "(if (char<=? #\\a #\\a) 0 1)" (if (char<=? #\a #\a) 0 1) 0)
   (test "(if (char<=? #\\b #\\a) 0 1)" (if (char<=? #\b #\a) 0 1) 1)))

; x is known to be #\a

(define (char-more-tests-for-control x)
  (allof "char=?, char<?, char<=?, char>?, char>=? for control (again!)"
   (test "(if (char=? x #\\b) 0 1)" (if (char=? x #\b) 0 1) 1)
   (test "(if (char=? x #\\a) 0 1)" (if (char=? x #\a) 0 1) 0)
   (test "(if (char>? x #\\b) 0 1)" (if (char>? x #\b) 0 1) 1)
   (test "(if (char>? x #\\a) 0 1)" (if (char>? x #\a) 0 1) 1)
   (test "(if (char>? #\\b x) 0 1)" (if (char>? #\b x) 0 1) 0)
   (test "(if (char>=? x #\\b) 0 1)" (if (char>=? x #\b) 0 1) 1)
   (test "(if (char>=? x #\\a) 0 1)" (if (char>=? x #\a) 0 1) 0)
   (test "(if (char>=? #\\b x) 0 1)" (if (char>=? #\b x) 0 1) 0)
   (test "(if (char<? x #\\b) 0 1)" (if (char<? x #\b) 0 1) 0)
   (test "(if (char<? x #\\a) 0 1)" (if (char<? x #\a) 0 1) 1)
   (test "(if (char<? #\\b x) 0 1)" (if (char<? #\b x) 0 1) 1)
   (test "(if (char<=? x #\\b) 0 1)" (if (char<=? x #\b) 0 1) 0)
   (test "(if (char<=? x #\\a) 0 1)" (if (char<=? x #\a) 0 1) 0)
   (test "(if (char<=? #\\b x) 0 1)" (if (char<=? #\b x) 0 1) 1)))

; x is #\a and y is #\b

(define (char-yet-more-tests-for-control x y)
  (allof "char=?, char<?, char<=?, char>?, char>=? for control (again!!)"
   (test "(if (char=? x y) 0 1)" (if (char=? x y) 0 1) 1)
   (test "(if (char=? x x) 0 1)" (if (char=? x x) 0 1) 0)
   (test "(if (char>? x y) 0 1)" (if (char>? x y) 0 1) 1)
   (test "(if (char>? x x) 0 1)" (if (char>? x x) 0 1) 1)
   (test "(if (char>? y x) 0 1)" (if (char>? y x) 0 1) 0)
   (test "(if (char>=? x y) 0 1)" (if (char>=? x y) 0 1) 1)
   (test "(if (char>=? x x) 0 1)" (if (char>=? x x) 0 1) 0)
   (test "(if (char>=? y x) 0 1)" (if (char>=? y x) 0 1) 0)
   (test "(if (char<? x y) 0 1)" (if (char<? x y) 0 1) 0)
   (test "(if (char<? x x) 0 1)" (if (char<? x x) 0 1) 1)
   (test "(if (char<? y x) 0 1)" (if (char<? y x) 0 1) 1)
   (test "(if (char<=? x y) 0 1)" (if (char<=? x y) 0 1) 0)
   (test "(if (char<=? x x) 0 1)" (if (char<=? x x) 0 1) 0)
   (test "(if (char<=? y x) 0 1)" (if (char<=? y x) 0 1) 1)))

(define (char-case-insensitive-tests)
  ; FIXME: char-ci=?, ...
  #t)

; x is known to be #\a
; FIXME: char-upcase, char-downcase

(define (char-conversion-tests x)
  (allof "integer->char, char->integer"
   (test "(char? (integer->char 44))" (char? (integer->char 44)) #t)
   (let loop ((i 0))
     (cond ((= i 256) #t)
	   ((not (test "(char? (integer->char i))" 
		       (char? (integer->char i))
		       #t))
	    #f)
	   (else
	    (loop (+ i 1)))))
   (test "(eqv? (char->integer x) 97)" (eqv? (char->integer x) 97) #t)
   (test "(char=? (integer->char (char->integer x)) x)"
	 (char=? (integer->char (char->integer x)) x)
	 #t)
   (do ((i 0 (+ i 1)))
       ((or (eqv? i 256) 
	    (not (test "(eqv? (char->integer (integer->char i)) i)"
		       (eqv? (char->integer (integer->char i)) i)
		       #t)))
	(eqv? i 256)))))

(define (char-classification-tests)
  (define (p x)
    (list (char-alphabetic? x)
	  (char-numeric? x)
	  (char-whitespace? x)
	  (char-upper-case? x)
	  (char-lower-case? x)))
  (allof "character classification"
   (let loop ((i (char->integer #\a)))
     (cond ((> i (char->integer #\z)) #t)
	   ((not (test `("(p " ,(integer->char i) ")")
		       (p (integer->char i))
		       '(#t #f #f #f #t)))
	    #f)
	   (else
	    (loop (+ i 1)))))
   (let loop ((i (char->integer #\A)))
     (cond ((> i (char->integer #\Z)) #t)
	   ((not (test `("(p " ,(integer->char i) ")")
		       (p (integer->char i))
		       '(#t #f #f #t #f)))
	    #f)
	   (else
	    (loop (+ i 1)))))
   (let loop ((i (char->integer #\0)))
     (cond ((> i (char->integer #\9)) #t)
	   ((not (test `("(p " ,(integer->char i) ")")
		       (p (integer->char i))
		       '(#f #t #f #f #f)))
	    #f)
	   (else
	    (loop (+ i 1)))))
   (every? (lambda (c)
	     (test `("(p " ,c ")")
		   (p c)
		   '(#f #f #t #f #f)))
	   '(#\space #\newline #\tab #\return #\page))
   ))
	     
(define (basic-unicode-char-tests)

  (define es-zed (integer->char #x00df))
  (define final-sigma (integer->char #x03c2))
  (define lower-sigma (integer->char #x03c3))
  (define upper-sigma (integer->char #x03a3))
  (define upper-chi (integer->char #x03a7))
  (define upper-alpha (integer->char #x0391))
  (define upper-omicron (integer->char #x039f))
  (define lower-chi (integer->char #x03c7))
  (define lower-alpha (integer->char #x03b1))
  (define lower-omicron (integer->char #x03bf))

  (let ()
	
  (define strasse (string #\S #\t #\r #\a es-zed #\e))
  (define upper-chaos (string upper-chi upper-alpha upper-omicron upper-sigma))
  (define final-chaos (string lower-chi lower-alpha lower-omicron final-sigma))
  (define lower-chaos (string lower-chi lower-alpha lower-omicron lower-sigma))

  (call-with-current-continuation
   (lambda (exit)

     ; Given a unary predicate on characters, returns a sorted
     ; list of all characters that satisfy the predicate.

     (define (filter-all-chars p?)
       (do ((i 0 (+ i 1))
            (chars '()
                   (if (and (not (<= #xd800 i #xdfff))
                            (p? (integer->char i)))
                       (cons (integer->char i) chars)
                       chars)))
           ((= i #x110000)
            (reverse chars))))

     ; Given a list of characters and the expected length of that list,
     ; returns the difference between the actual and expected lengths.

     (define (report chars n)
       (- (length chars) n))

     (test "type1" (integer->char 32) #\space)
     (test "type2" (char->integer (integer->char 5000)) 5000)
    ;(test "type3" (integer->char #xd800) error)

     (test "comp1" (char<? #\z es-zed) #t)
     (test "comp2" (char<? #\z #\Z) #f)
     (test "comp3" (char-ci<? #\z #\Z) #f)
     (test "comp4" (char-ci=? #\z #\Z) #t)
     (test "comp5" (char-ci=? final-sigma lower-sigma) #t)

     (test "case1" (char-upcase #\i) #\I)
     (test "case2" (char-downcase #\i) #\i)
     (test "case3" (char-titlecase #\i) #\I)
     (test "case4" (char-foldcase #\i) #\i)

     (test "case5" (char-upcase es-zed) es-zed)
     (test "case6" (char-downcase es-zed) es-zed)
     (test "case7" (char-titlecase es-zed) es-zed)
     (test "case8" (char-foldcase es-zed) es-zed)

     (test "case9" (char-upcase upper-sigma) upper-sigma)
     (test "case10" (char-downcase upper-sigma) lower-sigma)
     (test "case11" (char-titlecase upper-sigma) upper-sigma)
     (test "case12" (char-foldcase upper-sigma) lower-sigma)

     (test "case13" (char-upcase final-sigma) upper-sigma)
     (test "case14" (char-downcase final-sigma) final-sigma)
     (test "case15" (char-titlecase final-sigma) upper-sigma)
     (test "case16" (char-foldcase final-sigma) lower-sigma)

     (test "cat1" (char-general-category #\a) 
           (string->symbol "Ll"))
     (test "cat2" (char-general-category #\space) 
           (string->symbol "Zs"))
     (test "cat3" (char-general-category (integer->char #x10FFFF)) 
           (string->symbol "Cn"))

     (test "alpha1" (char-alphabetic? #\a) #t)
     (test "numer1" (char-numeric? #\1) #t)
     (test "white1" (char-whitespace? #\space) #t)
     (test "white2" (char-whitespace? (integer->char #x00A0)) #t)
     (test "upper1" (char-upper-case? upper-sigma) #t)
     (test "lower1" (char-lower-case? lower-sigma) #t)
     (test "lower2" (char-lower-case? (integer->char #x00AA)) #t)
     (test "title1" (char-title-case? #\I) #f)
     (test "title2" (char-title-case? (integer->char #x01C5)) #t)

     (test "excluded"
           (do ((i 128 (+ i 1))
                (excluded '()
                 (if (and (not (<= #xd800 i #xdfff))
                          (memq (char-general-category (integer->char i))
                                (map string->symbol 
                                     '("Ps" "Pe" "Pi" "Pf" "Zs" 
                                       "Zp" "Zl" "Cc" "Cf"))))
                     (cons i excluded)
                     excluded)))
               ((= i #x110000)
                (reverse excluded)))
           excluded-code-points-above-127)

     (test "upcase"
           (filter-all-chars (lambda (c) (char-upcase c) #f))
           '())

     (test "downcase"
           (filter-all-chars (lambda (c) (char-downcase c) #f))
           '())

     (test "titlecase"
           (filter-all-chars (lambda (c) (char-titlecase c) #f))
           '())

     (test "foldcase"
           (filter-all-chars (lambda (c) (char-foldcase c) #f))
           '())

     (test "general-category"
           (report (filter-all-chars (lambda (c) (char-general-category c)))
                   1112064)
           0)

     (test "alphabetic?"
           (report (filter-all-chars char-alphabetic?) 104077)
           0)

     (test "numeric?"
           (report (filter-all-chars char-numeric?) 530)
           0)

     (test "whitespace?"
           (report (filter-all-chars char-whitespace?) 25)
           0)

     (test "upper-case?"
           (report (filter-all-chars char-upper-case?) 1610)
           0)

     (test "lower-case?"
           (report (filter-all-chars char-lower-case?) 2030)
           0)

     (test "title-case?"
           (report (filter-all-chars char-title-case?) 31)
           0)

))))
    
; SRFI 77 listed all code points above 127 in Unicode 4.1 whose
; Unicode general category is Ps, Pe, Pi, Pf, Zs, Zp, Zl, Cc, or Cf.
;
; Two code points (#\x23b4 and #\x23b5) were dropped from that list
; in Unicode 5.0, and there have been quite a few more changes since
; then.  The following list was generated by the reference
; implementation from which Larceny's code was derived, so it isn't
; an independent test.

(define excluded-code-points-above-127
  '(
    #x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87
    #x88 #x89 #x8a #x8b #x8c #x8d #x8e #x8f
    #x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97
    #x98 #x99 #x9a #x9b #x9c #x9d #x9e #x9f
    #xa0 #xab #xad #xbb #x600 #x601 #x602 #x603
    #x604 #x605 #x61c #x6dd #x70f #xf3a #xf3b #xf3c
    #xf3d #x1680 #x169b #x169c #x180e #x2000 #x2001 #x2002
    #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200a
    #x200b #x200c #x200d #x200e #x200f #x2018 #x2019 #x201a
    #x201b #x201c #x201d #x201e #x201f #x2028 #x2029 #x202a
    #x202b #x202c #x202d #x202e #x202f #x2039 #x203a #x2045
    #x2046 #x205f #x2060 #x2061 #x2062 #x2063 #x2064 #x2066
    #x2067 #x2068 #x2069 #x206a #x206b #x206c #x206d #x206e
    #x206f #x207d #x207e #x208d #x208e #x2308 #x2309 #x230a
    #x230b #x2329 #x232a #x2768 #x2769 #x276a #x276b #x276c
    #x276d #x276e #x276f #x2770 #x2771 #x2772 #x2773 #x2774
    #x2775 #x27c5 #x27c6 #x27e6 #x27e7 #x27e8 #x27e9 #x27ea
    #x27eb #x27ec #x27ed #x27ee #x27ef #x2983 #x2984 #x2985
    #x2986 #x2987 #x2988 #x2989 #x298a #x298b #x298c #x298d
    #x298e #x298f #x2990 #x2991 #x2992 #x2993 #x2994 #x2995
    #x2996 #x2997 #x2998 #x29d8 #x29d9 #x29da #x29db #x29fc
    #x29fd #x2e02 #x2e03 #x2e04 #x2e05 #x2e09 #x2e0a #x2e0c
    #x2e0d #x2e1c #x2e1d #x2e20 #x2e21 #x2e22 #x2e23 #x2e24
    #x2e25 #x2e26 #x2e27 #x2e28 #x2e29 #x2e42 #x3000 #x3008
    #x3009 #x300a #x300b #x300c #x300d #x300e #x300f #x3010
    #x3011 #x3014 #x3015 #x3016 #x3017 #x3018 #x3019 #x301a
    #x301b #x301d #x301e #x301f #xfd3e #xfd3f #xfe17 #xfe18
    #xfe35 #xfe36 #xfe37 #xfe38 #xfe39 #xfe3a #xfe3b #xfe3c
    #xfe3d #xfe3e #xfe3f #xfe40 #xfe41 #xfe42 #xfe43 #xfe44
    #xfe47 #xfe48 #xfe59 #xfe5a #xfe5b #xfe5c #xfe5d #xfe5e
    #xfeff #xff08 #xff09 #xff3b #xff3d #xff5b #xff5d #xff5f
    #xff60 #xff62 #xff63 #xfff9 #xfffa #xfffb #x110bd #x1bca0
    #x1bca1 #x1bca2 #x1bca3 #x1d173 #x1d174 #x1d175 #x1d176 #x1d177
    #x1d178 #x1d179 #x1d17a #xe0001 #xe0020 #xe0021 #xe0022 #xe0023
    #xe0024 #xe0025 #xe0026 #xe0027 #xe0028 #xe0029 #xe002a #xe002b
    #xe002c #xe002d #xe002e #xe002f #xe0030 #xe0031 #xe0032 #xe0033
    #xe0034 #xe0035 #xe0036 #xe0037 #xe0038 #xe0039 #xe003a #xe003b
    #xe003c #xe003d #xe003e #xe003f #xe0040 #xe0041 #xe0042 #xe0043
    #xe0044 #xe0045 #xe0046 #xe0047 #xe0048 #xe0049 #xe004a #xe004b
    #xe004c #xe004d #xe004e #xe004f #xe0050 #xe0051 #xe0052 #xe0053
    #xe0054 #xe0055 #xe0056 #xe0057 #xe0058 #xe0059 #xe005a #xe005b
    #xe005c #xe005d #xe005e #xe005f #xe0060 #xe0061 #xe0062 #xe0063
    #xe0064 #xe0065 #xe0066 #xe0067 #xe0068 #xe0069 #xe006a #xe006b
    #xe006c #xe006d #xe006e #xe006f #xe0070 #xe0071 #xe0072 #xe0073
    #xe0074 #xe0075 #xe0076 #xe0077 #xe0078 #xe0079 #xe007a #xe007b
    #xe007c #xe007d #xe007e #xe007f))

; eof
