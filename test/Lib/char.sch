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
  (char-classification-tests))

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
  (define strasse (string #\S #\t #\r #\a es-zed #\e))
  (define upper-chaos (string upper-chi upper-alpha upper-omicron upper-sigma))
  (define final-chaos (string lower-chi lower-alpha lower-omicron final-sigma))
  (define lower-chaos (string lower-chi lower-alpha lower-omicron lower-sigma))

  (call-with-current-continuation
   (lambda (exit)

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
    ;(test "case3" (char-titlecase #\i) #\I)
    ;(test "case4" (char-foldcase #\i) #\i)

     (test "case5" (char-upcase es-zed) es-zed)
     (test "case6" (char-downcase es-zed) es-zed)
    ;(test "case7" (char-titlecase es-zed) es-zed)
    ;(test "case8" (char-foldcase es-zed) es-zed)

     (test "case9" (char-upcase upper-sigma) upper-sigma)
     (test "case10" (char-downcase upper-sigma) lower-sigma)
    ;(test "case11" (char-titlecase upper-sigma) upper-sigma)
    ;(test "case12" (char-foldcase upper-sigma) lower-sigma)

     (test "case13" (char-upcase final-sigma) upper-sigma)
     (test "case14" (char-downcase final-sigma) final-sigma)
    ;(test "case15" (char-titlecase final-sigma) upper-sigma)
    ;(test "case16" (char-foldcase final-sigma) lower-sigma)

    ;(test "cat1" (char-general-category #\a) 'Ll)
    ;(test "cat2" (char-general-category #\space) 'Zs)
    ;(test "cat3" (char-general-category (integer->char #x10FFFF)) 'Cn)

     (test "alpha1" (char-alphabetic? #\a) #t)
     (test "numer1" (char-numeric? #\1) #t)
     (test "white1" (char-whitespace? #\space) #t)
     (test "white2" (char-whitespace? (integer->char #x00A0)) #t)
     (test "upper1" (char-upper-case? upper-sigma) #t)
     (test "lower1" (char-lower-case? lower-sigma) #t)
     (test "lower2" (char-lower-case? (integer->char #x00AA)) #t)
    ;(test "title1" (char-title-case? #\I) #f)
    ;(test "title2" (char-title-case? (integer->char #x01C5)) #t)

    '(test "excluded"
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

)))
    
; eof
