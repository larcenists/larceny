; Copyright 1998 Lars T Hansen.
;
; $Id$

(define (run-boolean-tests)
  (display "Boolean") (newline)
  (test-not not)
  (test-boolean? boolean?))

; FIXME: should test on _all_ data types.

(define (test-not notp)
  (allof "not"
	 (test "(notp #t)" (notp #t) #f)
	 (test "(notp #f)" (notp #f) #t)
	 (test "(notp 1)" (notp 1) #f)
	 (test "(notp 'abracadabra)" (notp 'abracadabra) #f)
	 (test "(notp '())" (notp '()) #f)
	 (test "(notp car)" (notp car) #f)

	 (test "(not #t)" (not #t) #f)
	 (test "(not #f)" (not #f) #t)
	 (test "(not 1)" (not 1) #f)
	 (test "(not 'abracadabra)" (not 'abracadabra) #f)
	 (test "(not '())" (not '()) #f)
	 (test "(not car)" (not car) #f)))

; FIXME: should test on _all_ data types.

(define (test-boolean? booleanp)
  (allof "boolean?"
	 (test "(booleanp #t)" (booleanp #t) #t)
	 (test "(booleanp #f)" (booleanp #f) #t)
	 (test "(booleanp '()" (booleanp '()) #f)
	 (test "(booleanp \"foo\")" (booleanp "foo") #f)

	 (test "(boolean? #t)" (boolean? #t) #t)
	 (test "(boolean? #f)" (boolean? #f) #t)
	 (test "(boolean? '()" (boolean? '()) #f)
	 (test "(boolean? \"foo\")" (boolean? "foo") #f)

         (test "(boolean=? #f #f)" (boolean=? #f #f) #t)
         (test "(boolean=? #f #t)" (boolean=? #f #t) #f)
         (test "(boolean=? #t #f)" (boolean=? #t #f) #f)
         (test "(boolean=? #t #t)" (boolean=? #t #t) #t)
         (test "(boolean=? #t #t #f)" (boolean=? #t #t #f) #f)
         (test "(boolean=? #t #t #t)" (boolean=? #t #t #t) #t)))

; eof
