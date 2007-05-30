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
  (basic-unicode-char-tests))

(define (char-predicate-test)
  (allof "char?"
   (test "(char? #\a)" (char? #\a) #t)
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
   (test "(p #\a #\a)" (p #\a #\a) #t)
   (test "(p #\a #\b)" (p #\a #\b) #f)))

(define (char-simple-comparisons)
  (allof "char=?, char<?, char<=?, char>?, char>=?"
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

; X is known to have value #\a.

(define (char-more-simple-comparisons x)
  (allof "char=?, char<?, char<=?, char>?, char>=? (again!)"
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

(define (char-tests-for-control)
  (allof "char=?, char<?, char<=?, char>?, char>=? for control"
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

; x is known to be #\a

(define (char-more-tests-for-control x)
  (allof "char=?, char<?, char<=?, char>?, char>=? for control (again!)"
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

     (test "cat1" (char-general-category #\a) 'Ll)
     (test "cat2" (char-general-category #\space) 'Zs)
     (test "cat3" (char-general-category (integer->char #x10FFFF)) 'Cn)

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
                                '(Ps Pe Pi Pf Zs Zp Zl Cc Cf)))
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
           (report (filter-all-chars char-alphabetic?) 93217)
           0)

     (test "numeric?"
           (report (filter-all-chars char-numeric?) 282)
           0)

     (test "whitespace?"
           (report (filter-all-chars char-whitespace?) 26)
           0)

     (test "upper-case?"
           (report (filter-all-chars char-upper-case?) 1362)
           0)

     (test "lower-case?"
           (report (filter-all-chars char-lower-case?) 1791)
           0)

     (test "title-case?"
           (report (filter-all-chars char-title-case?) 31)
           0)

))))
    
(define excluded-code-points-above-127
  '(

 #x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89
 #x8A #x8B #x8C #x8D #x8E #x8F #x90 #x91 #x92 #x93
 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D
 #x9E #x9F #xA0 #xAB #xAD #xBB #x600 #x601 #x602 #x603
 #x6DD #x70F #xF3A #xF3B #xF3C #xF3D #x1680 #x169B
 #x169C #x17B4 #x17B5 #x180E #x2000 #x2001 #x2002 #x2003
 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x200B
 #x200C #x200D #x200E #x200F #x2018 #x2019 #x201A #x201B
 #x201C #x201D #x201E #x201F #x2028 #x2029 #x202A #x202B
 #x202C #x202D #x202E #x202F #x2039 #x203A #x2045 #x2046
 #x205F #x2060 #x2061 #x2062 #x2063 #x206A #x206B #x206C
 #x206D #x206E #x206F #x207D #x207E #x208D #x208E #x2329
 #x232A 
;       #x23B4 #x23B5 ; see note above for Unicode 5.0
                      #x2768 #x2769 #x276A #x276B #x276C
 #x276D #x276E #x276F #x2770 #x2771 #x2772 #x2773 #x2774
 #x2775 #x27C5 #x27C6 #x27E6 #x27E7 #x27E8 #x27E9 #x27EA
 #x27EB #x2983 #x2984 #x2985 #x2986 #x2987 #x2988 #x2989
 #x298A #x298B #x298C #x298D #x298E #x298F #x2990 #x2991
 #x2992 #x2993 #x2994 #x2995 #x2996 #x2997 #x2998 #x29D8
 #x29D9 #x29DA #x29DB #x29FC #x29FD #x2E02 #x2E03 #x2E04
 #x2E05 #x2E09 #x2E0A #x2E0C #x2E0D #x2E1C #x2E1D #x3000
 #x3008 #x3009 #x300A #x300B #x300C #x300D #x300E #x300F
 #x3010 #x3011 #x3014 #x3015 #x3016 #x3017 #x3018 #x3019
 #x301A #x301B #x301D #x301E #x301F #xFD3E #xFD3F #xFE17
 #xFE18 #xFE35 #xFE36 #xFE37 #xFE38 #xFE39 #xFE3A #xFE3B
 #xFE3C #xFE3D #xFE3E #xFE3F #xFE40 #xFE41 #xFE42 #xFE43
 #xFE44 #xFE47 #xFE48 #xFE59 #xFE5A #xFE5B #xFE5C #xFE5D
 #xFE5E #xFEFF #xFF08 #xFF09 #xFF3B #xFF3D #xFF5B #xFF5D
 #xFF5F #xFF60 #xFF62 #xFF63 #xFFF9 #xFFFA #xFFFB #x1D173
 #x1D174 #x1D175 #x1D176 #x1D177 #x1D178 #x1D179 #x1D17A
 #xE0001 #xE0020 #xE0021 #xE0022 #xE0023 #xE0024 #xE0025
 #xE0026 #xE0027 #xE0028 #xE0029 #xE002A #xE002B #xE002C
 #xE002D #xE002E #xE002F #xE0030 #xE0031 #xE0032 #xE0033
 #xE0034 #xE0035 #xE0036 #xE0037 #xE0038 #xE0039 #xE003A
 #xE003B #xE003C #xE003D #xE003E #xE003F #xE0040 #xE0041
 #xE0042 #xE0043 #xE0044 #xE0045 #xE0046 #xE0047 #xE0048
 #xE0049 #xE004A #xE004B #xE004C #xE004D #xE004E #xE004F
 #xE0050 #xE0051 #xE0052 #xE0053 #xE0054 #xE0055 #xE0056
 #xE0057 #xE0058 #xE0059 #xE005A #xE005B #xE005C #xE005D
 #xE005E #xE005F #xE0060 #xE0061 #xE0062 #xE0063 #xE0064
 #xE0065 #xE0066 #xE0067 #xE0068 #xE0069 #xE006A #xE006B
 #xE006C #xE006D #xE006E #xE006F #xE0070 #xE0071 #xE0072
 #xE0073 #xE0074 #xE0075 #xE0076 #xE0077 #xE0078 #xE0079
 #xE007A #xE007B #xE007C #xE007D #xE007E #xE007F
 
))

; eof
