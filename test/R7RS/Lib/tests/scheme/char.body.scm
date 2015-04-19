;;; Included only if the tested implementation claims to support
;;; the unicode feature.

;;; Given a unary predicate on characters, returns a sorted
;;; list of all characters that satisfy the predicate.

(define (filter-all-chars p?)
  (do ((i 0 (+ i 1))
       (chars '()
              (if (and (not (<= #xd800 i #xdfff))
                       (p? (integer->char i)))
                  (cons (integer->char i) chars)
                  chars)))
      ((= i #x110000)
       (reverse chars))))

;;; Given a unary predicate and a list, returns a list of
;;; all elements of the given list that satisfy the predicate.

(define (filter p? xs)
  (do ((xs (reverse xs) (cdr xs))
       (ys '() (if (p? (car xs))
                   (cons (car xs) ys)
                   ys)))
      ((null? xs)
       ys)))             

(define (run-char-tests-for-unicode)
    
  (test (char-upcase #\xDF) #\xDF)
  (test (char-downcase #\xDF) #\xDF)
  (test (char-foldcase #\xDF) #\xDF)
    
  (test (char-upcase #\x3A3) #\x3A3)
  (test (char-downcase #\x3A3) #\x3C3)
  (test (char-foldcase #\x3A3) #\x3C3)

  (test (char-upcase #\x3C2) #\x3A3)
  (test (char-downcase #\x3C2) #\x3C2)
  (test (char-foldcase #\x3C2) #\x3C3)

  (test (char-ci=? #\x3C2 #\x3C3) #t)

  (test (char-whitespace? #\x00A0) #t)
  (test (char-upper-case? #\x3A3) #t)
  (test (char-lower-case? #\x3C3) #t)
  (test (char-lower-case? #\x00AA) #t)

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

  ;; Systematic testing on every Unicode character.
  ;; The counts are believed to be correct for Unicode 5.0,
  ;; except for char-whitespace? (which has dropped to 25 in Unicode 7.0).
  ;; The counts are likely to increase monotonically (if at all) in later
  ;; versions, but that's not a given.

  (test (length (filter-all-chars (lambda (c)
                                    (and (char? c)
                                         (char? (char-upcase c))
                                         (char? (char-downcase c))
                                         (char? (char-foldcase c))
                                         (char=? c
                                                 (integer->char
                                                  (char->integer c)))))))
        1112064)

  (test (<= 93217 (length (filter-all-chars char-alphabetic?)))
        #t)

  (test (<= 282 (length (filter-all-chars char-numeric?)))
        #t)

  (test (<= 25 (length (filter-all-chars char-whitespace?)))
        #t)

  (test (<= 1362 (length (filter-all-chars char-upper-case?)))
        #t)

  (test (<= 1791 (length (filter-all-chars char-lower-case?)))
        #t)

  (test (let* ((chars (filter-all-chars char-numeric?))
               (vals (map digit-value chars))
               (mask (map (lambda (n)
                            (and (exact-integer? n)
                                 (<= 0 n 9)))
                          vals))
               (mask (map not mask))
               (bad  (filter values
                             (map (lambda (char is-bad?)
                                    (and is-bad? char))
                                  chars mask))))
          bad)
        '())

  (test (let* ((chars (filter-all-chars (lambda (c) (not (char-numeric? c)))))
               (vals (map digit-value chars))
               (bad  (filter values
                             (map (lambda (char is-bad?)
                                    (and is-bad? char))
                                  chars vals))))
          bad)
        '())

  )

