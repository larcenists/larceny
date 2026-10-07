; Copyright 2007 William D Clinger
;
; $Id$
;
; These tests are incomplete, but are a lot better
; than nothing.

; Copies of all sizes (long ones use memmove), overlapping in both
; directions, against copies made through lists.

; Fills of all sizes and ranges (long ones double the filled part with
; memmove), against fills one character at a time.

(define (string-fill-tests)

  (define (reference-fill n c start end)
    (let ((s (make-string n #\-)))
      (do ((i start (+ i 1)))
          ((= i end) s)
        (string-set! s i c))))

  (define (fill n c . range)
    (let ((s (make-string n #\-)))
      (apply string-fill! s c range)
      s))

  (allof "string fills"
   (allof-map "string-fill!"
    (lambda (n)
      (test (string-append "string-fill! " (number->string n))
            (fill n #\x3bb)
            (reference-fill n #\x3bb 0 n))
      (test (string-append "string-fill! from 3 " (number->string n))
            (fill (+ n 5) #\x 3)
            (reference-fill (+ n 5) #\x 3 (+ n 5)))
      (test (string-append "string-fill! 3 to n+3 " (number->string n))
            (fill (+ n 5) #\x 3 (+ n 3))
            (reference-fill (+ n 5) #\x 3 (+ n 3))))
    '(0 1 2 5 6 7 11 12 13 24 25 100 1000))
   (mustfail "string-fill! end too large" fill 10 #\x 2 11)
   (mustfail "string-fill! start > end" fill 10 #\x 5 4)
   (mustfail "string-fill! not a char" fill 10 65 2 4)))

(define (string-copy-tests)

  (define s
    (let ((s (make-string 500)))
      (do ((i 0 (+ i 1)))
          ((= i 500) s)
        (string-set! s i (integer->char (+ 945 (remainder i 50)))))))

  (define (reference-substring s i j)
    (list->string (list-head (list-tail (string->list s) i) (- j i))))

  (allof "string copies"
   (allof-map "substring and string-append"
    (lambda (n)
      (test (string-append "substring " (number->string n))
            (substring s 7 (+ 7 n))
            (reference-substring s 7 (+ 7 n)))
      (test (string-append "string-append " (number->string n))
            (string-append "ab" (substring s 0 n) "cd")
            (list->string (append (string->list "ab")
                                  (list-head (string->list s) n)
                                  (string->list "cd")))))
    '(0 1 5 6 7 16 100 480))
   (allof-map "overlapping string-copy!"
    (lambda (n)
      (let ((t1 (string-copy s))
            (t2 (string-copy s)))
        (string-copy! t1 13 t1 3 (+ 3 n))
        (string-copy! t2 3 t2 13 (+ 13 n))
        (test (string-append "copy! up " (number->string n))
              (substring t1 13 (+ 13 n))
              (reference-substring s 3 (+ 3 n)))
        (test (string-append "copy! down " (number->string n))
              (substring t2 3 (+ 3 n))
              (reference-substring s 13 (+ 13 n)))))
    '(0 1 5 6 7 100 480))))

(define (run-string-tests)
  (display "String") (newline)
  (string-predicate-test)
  (string-copy-tests)
  (string-fill-tests)
  (string-for-each-test)
  (vector-for-each-test)            ; FIXME: belongs in some other file
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
  
(define (string-for-each-test)

  (let ((x '()))

    (define (run . args)
      (set! x '())
      (apply string-for-each collect args)
      (reverse x))

    (define (collect . chars) (set! x (cons chars x)))

    (allof "string-for-each"
     (test "(string-for-each f \"\")" (run "") '())
     (test "(string-for-each f \"\" \"\")" (run "" "") '())
     (test "(string-for-each f \"\" \"\" \"\")" (run "" "" "") '())
     (test "(string-for-each f \"abc\")" (run "abc") '((#\a) (#\b) (#\c)))
     (test "(string-for-each f \"abc\" \"def\")"
           (run "abc" "def")
           '((#\a #\d) (#\b #\e) (#\c #\f)))
     (test "(string-for-each f \"abc\" \"def\" \"ghi\")"
           (run "abc" "def" "ghi")
           '((#\a #\d #\g) (#\b #\e #\h) (#\c #\f #\i))))))

; FIXME:  This is the wrong file for this.

(define (vector-for-each-test)

  (let ((x '()))

    (define (run . args)
      (set! x '())
      (apply vector-for-each collect args)
      (reverse x))

    (define (collect . chars) (set! x (cons chars x)))

    (allof "vector-for-each"
     (test "(vector-for-each f '#())" (run '#()) '())
     (test "(vector-for-each f '#() '#())" (run '#() '#()) '())
     (test "(vector-for-each f '#() '#() '#())" (run '#() '#() '#()) '())
     (test "(vector-for-each f '#(a b c))" (run '#(a b c)) '((a) (b) (c)))
     (test "(vector-for-each f '#(a b c) '#(d e f))"
           (run '#(a b c) '#(d e f))
           '((a d) (b e) (c f)))
     (test "(vector-for-each f '#(a b c) '#(d e f) '#(g h i))"
           (run '#(a b c) '#(d e f) '#(g h i))
           '((a d g) (b e h) (c f i))))

    (allof "vector-map"
     (test "(vector-map list '#())" (vector-map list '#()) '#())
     (test "(vector-map list '#() '#())" (vector-map list '#() '#()) '#())
     (test "(vector-map list '#() '#() '#())"
           (vector-map list '#() '#() '#()) '#())
     (test "(vector-map list '#(a b c))"
           (vector-map list '#(a b c)) '#((a) (b) (c)))
     (test "(vector-map list '#(a b c) '#(d e f))"
           (vector-map list '#(a b c) '#(d e f))
           '#((a d) (b e) (c f)))
     (test "(vector-map list '#(a b c) '#(d e f) '#(g h i))"
           (vector-map list '#(a b c) '#(d e f) '#(g h i))
           '#((a d g) (b e h) (c f i))))))

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
                                (string lower-sigma)))
  (test "chaos7" (string-upcase final-chaos) upper-chaos)
  (test "chaos8" (string-upcase lower-chaos) upper-chaos)

  (test "stitle1" (string-titlecase "kNock KNoCK") "Knock Knock")
  (test "stitle2" (string-titlecase "who's there?") "Who's There?")
  (test "stitle3" (string-titlecase "r6rs") "R6rs")
  (test "stitle4" (string-titlecase "R6RS") "R6rs")

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
