;;; Included only if the tested implementation claims to support
;;; the unicode feature.

(define (run-char-tests-for-unicode)
    
  (test (char-upcase #\xDF) #\xDF)
  (test (char-downcase #\xDF) #\xDF)
  (test (char-titlecase #\xDF) #\xDF)
  (test (char-foldcase #\xDF) #\xDF)
    
  (test (char-upcase #\x3A3) #\x3A3)
  (test (char-downcase #\x3A3) #\x3C3)
  (test (char-titlecase #\x3A3) #\x3A3)
  (test (char-foldcase #\x3A3) #\x3C3)

  (test (char-upcase #\x3C2) #\x3A3)
  (test (char-downcase #\x3C2) #\x3C2)
  (test (char-titlecase #\x3C2) #\x3A3)
  (test (char-foldcase #\x3C2) #\x3C3)

  (test (char-ci=? #\x3C2 #\x3C3) #t)

  (test (char-whitespace? #\x00A0) #t)
  (test (char-upper-case? #\x3A3) #t)
  (test (char-lower-case? #\x3C3) #t)
  (test (char-lower-case? #\x00AA) #t)
  (test (char-title-case? #\x01C5) #t)

  (test (string-upcase "Stra\xDF;e") "STRASSE")
  (test (string-downcase "Stra\xDF;e") "stra\xDF;e")
  (test (string-foldcase "Stra\xDF;e") "strasse")
  (test (string-downcase "\x3A3;") "\x3C3;")

  (test (string-upcase "\x39E;\x391;\x39F;\x3A3;")
        "\x39E;\x391;\x39F;\x3A3;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C3;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3; \x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C2; \x3C3;")
  (test (string-foldcase "\x39E;\x391;\x39F;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C3;")
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C3;")
        "\x39E;\x391;\x39F;\x3A3;") 
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C2;")
        "\x39E;\x391;\x39F;\x3A3;") 

  (test (string-downcase "A\x3A3;'x") "a\x3C3;'x") ; ' is a MidLetter

  (test (string-ci=? "Stra\xDF;e" "Strasse") #t)
  (test (string-ci=? "Stra\xDF;e" "STRASSE") #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C2;")
        #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C3;")
        #t)
  )

