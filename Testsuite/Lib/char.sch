; Testsuite/Lib/char.sch
; Larceny testsuite -- character operations
;
; $Id$

(define (run-char-tests)
  (newline)
  (display "****************************************") (newline)
  (display "Characters") (newline)
  (newline)
  (and (char-predicate-test)
       (char-test-eqv?)
       (char-simple-comparisons)
       (char-more-simple-comparisons #\a)
       (char-tests-for-control)
       (char-more-tests-for-control #\a)
       (char-yet-more-tests-for-control #\a #\b)
       (char-conversion-tests #\a)
       (char-classification-tests)))

(define (char-predicate-test)
  (display "----------------------------------------") (newline)
  (display "Testing char?") (newline)
  (allof
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

  (display "----------------------------------------") (newline)
  (display "Testing eqv? on characters") (newline)
  (allof
   (test "(p #\a #\a)" (p #\a #\a) #t)
   (test "(p #\a #\b)" (p #\a #\b) #f)))

(define (char-simple-comparisons)
  (display "----------------------------------------") (newline)
  (display "Simple char comparisons") (newline)
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

; X is known to have value #\a.

(define (char-more-simple-comparisons x)
  (display "----------------------------------------") (newline)
  (display "More simple char comparisons.") (newline)
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

(define (char-tests-for-control)
  (display "----------------------------------------") (newline)
  (display "Character predicates for control") (newline)
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

; x is known to be #\a

(define (char-more-tests-for-control x)
  (display "----------------------------------------") (newline)
  (display "More character predicates for control") (newline)
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

; x is #\a and y is #\b

(define (char-yet-more-tests-for-control x y)
  (display "----------------------------------------") (newline)
  (display "Yet more character predicates for control.") (newline)
  (allof
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
  (display "----------------------------------------") (newline)
  (display "Character conversions.") (newline)
  (allof
   (test "(char? (integer->char 44))" (char? (integer->char 44)) #t)
   (let loop ((i 0 (+ i 1)))
     (cond ((= i 256) #t)
	   ((not (test "(char? (integer->char i))" 
		       (char? (integer->char i))
		       #t))
	    #f)
	   (else (loop (+ i 1)))))
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
  (display "----------------------------------------") (newline)
  (display "Characer classification.") (newline)
  (allof
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
	     
    
; eof
