; Copyright 2007 William D Clinger
;
; $Id$
;
; These tests are incomplete, but are a lot better
; than nothing.

(define (run-string-tests)
  (display "String") (newline)
  (string-predicate-test)
  ;(string-simple-comparisons)
  ;(string-more-simple-comparisons #\a)
  ;(string-tests-for-control)
  ;(string-more-tests-for-control #\a)
  ;(string-yet-more-tests-for-control #\a #\b)
  ;(string-conversion-tests #\a)
  ;(string-classification-tests)
  (basic-unicode-string-tests))

(define (string-predicate-test)
  (allof "string?"
   (test "(string? (make-string 0))" (string? (make-string 0)) #t)
   (test "(string? (make-bytevector 0))" (string? (make-bytevector 0)) #f)
   (test "(string? (make-vector 0))" (string? (make-vector 0)) #f)
   (test "(string? (current-output-port))" (string? (current-output-port)) #f)
   (test "(string? open-input-file)" (string? open-input-file) #f)
   (test "(string? #\a)" (string? #\a) #f)
   (test "(string? 37)" (string? 37) #f)
   (test "(string? #x26)" (string? #x26) #f)
   (test "(string? 0.0)" (string? 0.0) #f)
   (test "(string? #'())" (string? '#()) #f)
   (test "(string? '(a . b))" (string? '(a . b)) #f)
   (test "(string? \"\")" (string? "") #t)
   (test "(string? \"a\")" (string? "a") #t)
   (test "(string? #t)" (string? #t) #f)
   (test "(string? #f)" (string? #f) #f)
   (test "(string? '())" (string? '()) #f)
   (test "(string? (unspecified))" (string? (unspecified)) #f)
   (test "(string? (undefined))" (string? (undefined)) #f)
   ))
  
(define (basic-unicode-string-tests)

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

  (define null (integer->char 0))
  (define biggy (integer->char #x10ffff))

  (let ()
	
  (define strasse (string #\S #\t #\r #\a es-zed #\e))
  (define upper-chaos (string upper-chi upper-alpha upper-omicron upper-sigma))
  (define final-chaos (string lower-chi lower-alpha lower-omicron final-sigma))
  (define lower-chaos (string lower-chi lower-alpha lower-omicron lower-sigma))
  (define mutable-lower-chaos
    (string lower-chi lower-alpha lower-omicron lower-sigma))

  (test "(string-length (make-string 0))"
        (string-length (make-string 0)) 0)
  (test "(string-length (make-string 34))"
        (string-length (make-string 34)) 34)
  (test "(string-length strasse)" (string-length strasse) 6)

  (test "(string-ref (make-string 5 #\w) 0)"
        (string-ref (make-string 5 #\w) 0) #\w)
  (test "(string-ref (make-string 5 #\w) 4)"
        (string-ref (make-string 5 #\w) 4) #\w)
  (test "(string-ref strasse 0)" (string-ref strasse 0) #\S)
  (test "(string-ref strasse 1)" (string-ref strasse 1) #\t)
  (test "(string-ref strasse 2)" (string-ref strasse 2) #\r)
  (test "(string-ref strasse 3)" (string-ref strasse 3) #\a)
  (test "(string-ref strasse 4)" (string-ref strasse 4) #\x00df)
  (test "(string-ref strasse 5)" (string-ref strasse 5) #\e)
  (test "(string-ref upper-chaos 3)" (string-ref upper-chaos 3) upper-sigma)
  (test "(string-ref final-chaos 3)" (string-ref final-chaos 3) final-sigma)
  (test "(string-ref lower-chaos 3)" (string-ref lower-chaos 3) lower-sigma)

  (test "(string-set! mutable-lower-chaos 0 #\nul)"
        (begin (string-set! mutable-lower-chaos 0 null)
               (string-ref mutable-lower-chaos 0))
        null)
  (test "(string-set! mutable-lower-chaos 3 biggy)"
        (begin (string-set! mutable-lower-chaos 3 biggy)
               (string-ref mutable-lower-chaos 3))
        biggy)
  (test "(string->list mutable-lower-chaos)"
        (list (string-ref mutable-lower-chaos 0)
              (string-ref mutable-lower-chaos 1)
              (string-ref mutable-lower-chaos 2)
              (string-ref mutable-lower-chaos 3))
        (list #\x0 lower-alpha lower-omicron biggy))

  (test "scomp1" (string<? "z" (string es-zed)) #t)
  (test "scomp2" (string<? "z" "zz") #t)
  (test "scomp3" (string<? "z" "Z") #f)
  (test "scomp4" (string=? strasse "Strasse") #f)

  (test "sup1" (string-upcase "Hi") "HI")
  (test "sdown1" (string-downcase "Hi") "hi")
  (test "sfold1" (string-foldcase "Hi") "hi")

  (test "sup2"  (string-upcase strasse) "STRASSE")
  (test "sdown2" (string-downcase strasse)
                 (string-append "s" (substring strasse 1 6)))
  (test "sfold2" (string-foldcase strasse) "strasse")
  (test "sdown3" (string-downcase "STRASSE")  "strasse")

  (test "chaos1" (string-upcase upper-chaos) upper-chaos)
  (test "chaos2" (string-downcase (string upper-sigma))
                 (string lower-sigma))
  (test "chaos3" (string-downcase upper-chaos) final-chaos)
  (test "chaos4" (string-downcase (string-append upper-chaos
                                                 (string upper-sigma)))
                 (string-append (substring lower-chaos 0 3)
                                (string lower-sigma final-sigma)))
  (test "chaos5" (string-downcase (string-append upper-chaos
                                                 (string #\space
                                                         upper-sigma)))
                 (string-append final-chaos
                                (string #\space lower-sigma)))
  (test "chaos6" (string-foldcase (string-append upper-chaos
                                                 (string upper-sigma)))
                 (string-append lower-chaos
                                (string final-sigma)))
  (test "chaos7" (string-upcase final-chaos) upper-chaos)
  (test "chaos8" (string-upcase lower-chaos) upper-chaos)

  (test "stitle1" (string-titlecase "kNock KNoCK") "Knock Knock")
  (test "stitle2" (string-titlecase "who's there?") "Who's There?")
  (test "stitle3" (string-titlecase "r6rs") "R6Rs")
  (test "stitle4" (string-titlecase "R6RS") "R6Rs")

  (test "norm1" (string-normalize-nfd (string #\xE9))
                (string #\x65 #\x301))
  (test "norm2" (string-normalize-nfc (string #\xE9))
                (string #\xE9))
  (test "norm3" (string-normalize-nfd (string #\x65 #\x301))
                (string #\x65 #\x301))
  (test "norm4" (string-normalize-nfc (string #\x65 #\x301))
                (string #\xE9))

  (test "sci1" (string-ci<? "z" "Z") #f)
  (test "sci2" (string-ci=? "z" "Z") #t)
  (test "sci3" (string-ci=? strasse "Strasse") #t)
  (test "sci4" (string-ci=? strasse "STRASSE") #t)
  (test "sci5" (string-ci=? upper-chaos lower-chaos) #t)
))
    
; eof
