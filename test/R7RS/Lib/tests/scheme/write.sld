;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme write) procedures:
;;;
;;;     display
;;;     write
;;;     write-shared
;;;     write-simple

(define-library (tests scheme write)
  (export run-write-tests)
  (import (scheme base)
          (scheme write)
          (tests scheme test))

   (cond-expand
    ((library (scheme read))
     (import (scheme read)))
    (else))

  (begin

   (cond-expand
    ((library (scheme read))
     (begin (define (run-write-tests-using-read)
              (run-write-tests-using-read-really))))
    ((not (library (scheme read)))
     (begin (define (read . args) #t)
            (define (run-write-tests-using-read) #t))))

   (define (write-data writer data)
     (map (lambda (datum) (write-datum writer datum))
          data))

   (define (write-datum writer x)
     (let ((q (open-output-string)))
       (writer x q)
       (get-output-string q)))

   ;; Can't assume the (scheme char) library is available.
   ;; We'll use this only for ASCII strings.

   (define (string-downcase s)
     (string-map (lambda (c)
                   (if (char<=? #\A c #\Z)
                       (integer->char
                        (+ (char->integer c)
                           (- (char->integer #\a)
                              (char->integer #\A))))
                       c))
                 s))

   ;; The write procedures often have a choice of external representations.
   ;; The read procedure should treat all legal representations the same.

   (define (run-write-tests-using-read-really)

     (define (write-then-read-help writer datum)
       (let* ((s (write-datum writer datum))
              (p (open-input-string s))
              (x (read p)))
         (and (eof-object? (read p))
              x)))

     (define (write-then-read datum)
       (let ((x1 (write-then-read-help write datum))
             (x2 (write-then-read-help write-shared datum))
             (x3 (write-then-read-help write-simple datum)))
         (if (and (equal? x1 x2) (equal? x2 x3))
             x1
             (list x1 x2 x3))))

     (define (read-from-string s)
       (let ((p (open-input-string s)))
         (read p)))

     ;; Strings

     (test (write-then-read "\r\n\t\b\a\|\"\\")
           (list->string
            (map integer->char
                 '(13 10 9 8 7 124 34 92))))

     (test (write-then-read
            "\x7f;\x4c;\x61;\x72;\x63;\x65;\x6e;\x79;\x0;#x21;")
           (list->string
            (map integer->char
                 '(127 76 97 114 99 101 110 121 0 35 120 50 49 59))))

     ;; Characters

     (test (write-then-read (map integer->char '(32 9 10 13)))
           '(#\space #\tab #\newline #\return))

     (test (write-then-read '(#\alarm #\backspace #\delete #\escape #\newline))
           (map integer->char '(7 8 127 27 10)))

     (test (write-then-read '(#\null #\return #\space #\tab))
           (map integer->char '(0 13 32 9)))

     (test (write-then-read '(#\x0 #\x00 #\x1 #\x20 #\x5f #\x7c #\x7f))
           (map integer->char '(0 0 1 32 95 124 127)))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; <identifier>
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;; <identifier> ::= <initial> <subsequent>*

     (test (write-then-read (map string->symbol
                                 '("a" "!" "$" "%" "&" "*" "/" ":"
                                   "<" "=" ">" "?" "^" "_" "~" "@")))
           '(a ! $ % & * / : < = > ? ^ _ ~ @))

     ;; <subsequent> ::= <initial> | <digit> | <special subsequent>

     (test (write-then-read
            '(Z: !z $0 %/ &? *^ /~ :@ <9 =+ >- ?@ ^+- _-+ ~@- @@@@@))
           (map string->symbol
                '("Z:" "!z" "$0" "%/" "&?" "*^" "/~" ":@"
                  "<9" "=+" ">-" "?@" "^+-" "_-+" "~@-" "@@@@@")))

     ;; <identifier> ::= <vertical line> <symbol element>* <vertical line>

     (test (write-then-read '|;alskjdf;aqwjepojq-1945apgf ;bna]as|)
           (string->symbol ";alskjdf;aqwjepojq-1945apgf ;bna]as"))

     ;; |\a\b\t\n\r\|\"\\|

     (test (write-then-read '|\a\b\t\n\r\|\"\\|)
           (string->symbol
            (string #\alarm #\backspace #\tab #\newline #\return
                    #\| #\" #\\)))

     (test (write-then-read '(|\\\|\" a| |\"\|\\ b|))
           (map string->symbol
                (list (string #\\ #\| #\" #\space #\a)
                      (string #\" #\| #\\ #\space #\b))))

     (test (write-then-read '|\x000;\x01;\x2;\t\r\x41;\n\t\x7e;\x7f;|)
           (string->symbol
            (string #\null #\x1 #\x2 #\tab #\return #\A
                    #\newline #\tab #\~ #\delete)))

     ;; <peculiar identifier>

     ;; <explicit sign>

     (test (write-then-read '(+ -)) '(+ -))

     ;; <explicit sign> <sign subsequent> <subsequent>*

     (test (write-then-read '(+: -@ +- -- +@ -@ +$$ -@3 +-4 --5 +@_ -@.))
           (map string->symbol
                '("+:" "-@" "+-" "--" "+@" "-@" "+$$"
                  "-@3" "+-4" "--5" "+@_" "-@.")))

     ;; <explicit sign> . <dot subsequent> <subsequent>*

     (test (write-then-read '(+.! -.+ +.. -.. +.@ -.@))
           (map string->symbol
                '("+.!" "-.+" "+.." "-.." "+.@" "-.@")))

     (test (write-then-read '(+.<.+ -.++. +..:? -..- +.@&. -.@24))
           (map string->symbol
                '("+.<.+" "-.++." "+..:?" "-..-" "+.@&." "-.@24")))

     ;; . <dot subsequent> <subsequent>*

     (test (write-then-read '(._ .+ .- .@ .. ._. .+. .-. .@. ...))
           (map string->symbol
                '("._" ".+" ".-" ".@" ".."
                  "._." ".+." ".-." ".@." "...")))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; Miscellaneous.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (read-from-string
            (write-datum display '(( ) #( ) #u8( ) 'x `x ,x ,@x (a . b))))
           '(() #() #u8() 'x `x ,x ,@x (a . b)))

     (test (write-then-read '(( ) #( ) #u8( ) 'x `x ,x ,@x (a . b)))
           '(() #() #u8() 'x `x ,x ,@x (a . b)))

     ;; Shared data.

     (test (write-then-read (let* ((x (list 1 2))
                                   (y (list x x)))
                              (list y y)))
           '(((1 2) (1 2)) ((1 2) (1 2))))

     (test (let ((z (read-from-string
                     (write-datum write-shared (let* ((x (list 1 2))
                                                      (y (list x x)))
                                                 (list y y))))))
             (list (eq? (car z) (cadr z))
                   (eq? (car (car z)) (cadr (car z)))))
           '(#t #t))

     ;; Circular data.

     (test (read-from-string
            (write-datum write (let* ((x (list 1 2 3 4 5))
                                      (v (vector x x x)))
                                 (set-car! (cddr x) v)
                                 (set-cdr! (cdr (cdr (cdr x))) x)
                                 (vector-set! v 1 (list v))
                                 v)))
           (let* ((x (list 1 2 3 4 5))
                  (v (vector x x x)))
             (set-car! (cddr x) v)
             (set-cdr! (cdr (cdr (cdr x))) x)
             (vector-set! v 1 (list v))
             v))

     (test (read-from-string
            (write-datum write-shared (let* ((x (list 1 2 3 4 5))
                                             (v (vector x x x)))
                                             (set-car! (cddr x) v)
                                             (set-cdr! (cdr (cdr (cdr x))) x)
                                             (vector-set! v 1 (list v))
                                             v)))
           (let* ((x (list 1 2 3 4 5))
                  (v (vector x x x)))
             (set-car! (cddr x) v)
             (set-cdr! (cdr (cdr (cdr x))) x)
             (vector-set! v 1 (list v))
             v))

     #t)

   (define (run-write-tests-full-unicode) #t)

   (define (run-write-tests-exact-closed) #t)

   (define (run-write-tests-exact-complex) #t)

   (define (run-write-tests-ieee-float) #t)

   (define (run-write-tests-ratios) #t)

   (define (run-write-tests-inexact) #t)

   (define (run-write-tests-inexact-complex) #t)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;; Some of these tests enforce a conventional representation
   ;;; when unconventional representations might be legal ouputs.
   ;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define (run-write-tests)

     ;; <boolean>

     (test (and (member (string-downcase (write-datum display #t))
                        '("#t" "#true"))
                #t)
           #t)

     (test (and (member (string-downcase (write-datum write #t))
                        '("#t" "#true"))
                #t)
           #t)

     (test (and (member (string-downcase (write-datum write-shared #t))
                        '("#t" "#true"))
                #t)
           #t)

     (test (and (member (string-downcase (write-datum write-simple #t))
                        '("#t" "#true"))
                #t)
           #t)

     (test (and (member (string-downcase (write-datum display #f))
                        '("#f" "#false"))
                #t)
           #t)

     ;; <string>

     (test (and (member (string-downcase (write-datum write #f))
                        '("#f" "#false"))
                #t)
           #t)

     ;; <string>

     (test (and (member (string-downcase (write-datum write-shared #f))
                        '("#f" "#false"))
                #t)
           #t)

     ;; <string>

     (test (and (member (string-downcase (write-datum write-simple #f))
                        '("#f" "#false"))
                #t)
           #t)

     ;; <string>

     (test (write-datum display
                        "The quick red fox jumped over the lazy dog.")
           "The quick red fox jumped over the lazy dog.")

     ;; <character>

     (test (write-datum write
                        "The quick red fox jumped over the lazy dog.")
           "\"The quick red fox jumped over the lazy dog.\"")

     ;; <character>

     (test (write-datum write-shared
                        "The quick red fox jumped over the lazy dog.")
           "\"The quick red fox jumped over the lazy dog.\"")

     ;; <character>

     (test (write-datum write-simple
                        "The quick red fox jumped over the lazy dog.")
           "\"The quick red fox jumped over the lazy dog.\"")

     ;; <character>

     (test (write-data display
                       '(#\a  #\Z  #\0  #\9  #\`  #\'   #\"  #\~  #\!  #\=))
           '("a" "Z" "0" "9" "`" "'" "\"" "~" "!" "="))

     (test (write-data write
                       '(#\a  #\Z  #\0  #\9  #\`  #\'   #\"  #\~  #\!  #\=))
           '("#\\a" "#\\Z" "#\\0" "#\\9" "#\\`" "#\\'"
             "#\\\"" "#\\~" "#\\!" "#\\="))

     (test (write-data write-shared
                       '(#\a  #\Z  #\0  #\9  #\`  #\'   #\"  #\~  #\!  #\=))
           '("#\\a" "#\\Z" "#\\0" "#\\9" "#\\`" "#\\'"
             "#\\\"" "#\\~" "#\\!" "#\\="))

     (test (write-data write-simple
                       '(#\a  #\Z  #\0  #\9  #\`  #\'   #\"  #\~  #\!  #\=))
           '("#\\a" "#\\Z" "#\\0" "#\\9" "#\\`" "#\\'"
             "#\\\"" "#\\~" "#\\!" "#\\="))

     (run-write-tests-full-unicode)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; <identifier>
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;; <identifier> ::= <initial> <subsequent>*

     (test (write-data display (map string->symbol
                                  '("a" "!" "$" "%" "&" "*" "/" ":"
                                    "<" "=" ">" "?" "^" "_" "~" "@")))
           '("a" "!" "$" "%" "&" "*" "/" ":" "<" "=" ">" "?" "^" "_" "~" "@"))

     (test (write-data write (map string->symbol
                                  '("a" "!" "$" "%" "&" "*" "/" ":"
                                    "<" "=" ">" "?" "^" "_" "~" "@")))
           '("a" "!" "$" "%" "&" "*" "/" ":" "<" "=" ">" "?" "^" "_" "~" "@"))

     (test (write-data write-shared (map string->symbol
                                  '("a" "!" "$" "%" "&" "*" "/" ":"
                                    "<" "=" ">" "?" "^" "_" "~" "@")))
           '("a" "!" "$" "%" "&" "*" "/" ":" "<" "=" ">" "?" "^" "_" "~" "@"))

     (test (write-data write-simple (map string->symbol
                                  '("a" "!" "$" "%" "&" "*" "/" ":"
                                    "<" "=" ">" "?" "^" "_" "~" "@")))
           '("a" "!" "$" "%" "&" "*" "/" ":" "<" "=" ">" "?" "^" "_" "~" "@"))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; <number>
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (run-write-tests-exact-closed)

     (run-write-tests-exact-complex)

     (run-write-tests-ieee-float)

     (run-write-tests-ratios)

     (run-write-tests-inexact)

     (run-write-tests-inexact-complex)

     ;; small exact integers

     (test (write-data display
                       '(0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write
                       '(0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write-shared
                       '(0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write-simple
                       '(0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data display
                       '(#e0 #e+0 #e-0 #e00 #e+0000 #e-00000
                         #e001 #e2 #e-003 #e007 #e97 #e1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write
                       '(#e0 #e+0 #e-0 #e00 #e+0000 #e-00000
                         #e001 #e2 #e-003 #e007 #e97 #e1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write-shared
                       '(#e0 #e+0 #e-0 #e00 #e+0000 #e-00000
                         #e001 #e2 #e-003 #e007 #e97 #e1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     (test (write-data write-simple
                       '(#e0 #e+0 #e-0 #e00 #e+0000 #e-00000
                         #e001 #e2 #e-003 #e007 #e97 #e1001))
           '("0" "0" "0" "0" "0" "0" "1" "2" "-3" "7" "97" "1001"))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; External representations of data.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (write-data display '(#u8() #u8(0 1 2 3 255)))
           '("#u8()" "#u8(0 1 2 3 255)"))

     (test (write-data write '(#u8() #u8(0 1 2 3 255)))
           '("#u8()" "#u8(0 1 2 3 255)"))

     (test (write-data write-shared '(#u8() #u8(0 1 2 3 255)))
           '("#u8()" "#u8(0 1 2 3 255)"))

     (test (write-data write-simple '(#u8() #u8(0 1 2 3 255)))
           '("#u8()" "#u8(0 1 2 3 255)"))

     (test (write-data display '(() (1 . 2) (3 . (4 . (5 . ())))))
           '("()" "(1 . 2)" "(3 4 5)"))

     (test (write-data write '(() (1 . 2) (3 . (4 . (5 . ())))))
           '("()" "(1 . 2)" "(3 4 5)"))

     (test (write-data write-shared '(() (1 . 2) (3 . (4 . (5 . ())))))
           '("()" "(1 . 2)" "(3 4 5)"))

     (test (write-data write-simple '(() (1 . 2) (3 . (4 . (5 . ())))))
           '("()" "(1 . 2)" "(3 4 5)"))

     (test (write-data display '(#() #(a) #(19 21 c)))
           '("#()" "#(a)" "#(19 21 c)"))

     (test (write-data write '(#() #(a) #(19 21 c)))
           '("#()" "#(a)" "#(19 21 c)"))

     (test (write-data write-shared '(#() #(a) #(19 21 c)))
           '("#()" "#(a)" "#(19 21 c)"))

     (test (write-data write-simple '(#() #(a) #(19 21 c)))
           '("#()" "#(a)" "#(19 21 c)"))

     (run-write-tests-using-read)

     )))
