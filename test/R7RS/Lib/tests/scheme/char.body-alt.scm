;;; Included only if the tested implementation claims to support
;;; the unicode feature and also claims to be very slow.

;;; Given a vector of unary predicates on characters,
;;; returns a vector of sorted lists of all characters
;;; that satisfy the corresponding predicate.

(define (filter-all-chars-by-predicates pvec)

  (define (loop i charlists)
    (cond ((= i #x110000)
           charlists)
          ((<= #xd800 i #xdfff)
           (loop #xe000 charlists))
          (else
           (let ((c (integer->char i)))
             (update! c charlists)
             (loop (+ i 1)
                   charlists)))))

  (define (update! c charlists)
    (do ((n (vector-length pvec))
         (j 0 (+ j 1)))
        ((= j n))
      (if ((vector-ref pvec j) c)
          (vector-set! charlists j (cons c (vector-ref charlists j))))))

  (loop 0 (make-vector (vector-length pvec) '())))

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

  (let* ((is-a-char? (lambda (c)
                       (and (char? c)
                            (char? (char-upcase c))
                            (char? (char-downcase c))
                            (char? (char-foldcase c))
                            (char=? c
                                    (integer->char
                                     (char->integer c))))))

         (is-bad-numeric? (lambda (c)
                            (and (char-numeric? c)
                                 (let ((n (digit-value c)))
                                   (not (and (exact-integer? n)
                                             (<= 0 n 9)))))))

         (is-bad-non-numeric? (lambda (c)
                                (and (not (char-numeric? c))
                                     (digit-value c))))

         (pvec (vector is-a-char?
                       char-alphabetic?
                       char-numeric?
                       char-whitespace?
                       char-upper-case?
                       char-lower-case?
                       is-bad-numeric?
                       is-bad-non-numeric?))

         (nvec (vector 1112064 ; is-a-char?
                       93217   ; char-alphabetic?
                       282     ; char-numeric?
                       25      ; char-whitespace?
                       1362    ; char-upper-case?
                       1791    ; char-lower-case?
                       0       ; is-bad-numeric?
                       0))     ; is-bad-non-numeric?

         (cvec (filter-all-chars-by-predicates pvec)))

    (test (= (length (vector-ref cvec 0))  ; is-a-char?
             (vector-ref nvec 0))
          #t)

    (test (>= (length (vector-ref cvec 1)) ; char-alphabetic?
              (vector-ref nvec 1))
          #t)

    (test (>= (length (vector-ref cvec 2)) ; char-numeric?
              (vector-ref nvec 2))
          #t)

    (test (>= (length (vector-ref cvec 3)) ; char-whitespace?
              (vector-ref nvec 3))
          #t)

    (test (>= (length (vector-ref cvec 4)) ; char-upper-case?
              (vector-ref nvec 4))
          #t)

    (test (>= (length (vector-ref cvec 5)) ; char-lower-case?
              (vector-ref nvec 5))
          #t)

    (test (= (length (vector-ref cvec 6))  ; is-bad-numeric?
             (vector-ref nvec 6))
          #t)

    (test (= (length (vector-ref cvec 7))  ; is-bad-non-numeric?
             (vector-ref nvec 7))
          #t)

    ))

