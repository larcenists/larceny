;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests this (scheme read) procedure:
;;;
;;;     read
;;;
;;; What could be simpler?

(define-library (tests scheme read)
  (export run-read-tests)
  (import (scheme base)
          (scheme read)
          (tests scheme test))

  (begin

   (define (read-data str)
     (let loop ((p (open-input-string str))
                (xs '()))
       (let ((x (read p)))
         (if (eof-object? x)
             (reverse xs)
             (loop p (cons x xs))))))

   (define (read-datum str)
     (car (read-data str)))

   ;; FIXME

   (define (run-read-tests-full-unicode) #t)

   (define (run-read-tests-exact-closed) #t)

   (define (run-read-tests-exact-complex) #t)

   (define (run-read-tests-ieee-float) #t)

   (define (run-read-tests-ratios) #t)

   (define (run-read-tests-inexact) #t)

   (define (run-read-tests-inexact-complex) #t)


   (define (run-read-tests)

     ;; <boolean>

     (test (read-data "#t #f #true #false #T #F #TRUE #FALSE #tRuE #False")
           '(#t #f #t #f #t #f #t #f #t #f))

     ;; <string>

     (test "The quick red fox jumped over the lazy dog."
           (list->string
            (map integer->char
                 '(84 104 101 32
                   113 117 105 99 107 32
                   114 101 100 32
                   102 111 120 32
                   106 117 109 112 101 100 32
                   111 118 101 114 32
                   116 104 101 32
                   108 97 122 121 32
                   100 111 103 46))))

     (test "\r\n\t\b\a\|\"\\"
           (list->string
            (map integer->char
                 '(13 10 9 8 7 124 34 92))))

     (test "\x7f;\x4c;\x61;\x72;\x63;\x65;\x6e;\x79;\x0;#x21;"
           (list->string
            (map integer->char
                 '(127 76 97 114 99 101 110 121 0 35 120 50 49 59))))

     ;; <character>

     (test (read-data "#\\a #\\Z #\\0 #\\9 #\\` #\\' #\\\" #\\~ #\\! #\\=")
           '(           #\a  #\Z  #\0  #\9  #\`  #\'   #\"  #\~  #\!  #\=))

     (test (read-data "#\\  #\\\t #\\\n #\\\r")
           (map integer->char '(32 9 10 13)))

     (test (read-data "#\\alarm #\\backspace #\\delete #\\escape #\\newline")
           (map integer->char '(7 8 127 27 10)))

     (test (read-data "  #\\null #\\return #\\space #\\tab  ")
           (map integer->char '(0 13 32 9)))

     (test (read-data "#\\x0 #\\x00 #\\x1 #\\x20 #\\x5f #\\x7c #\\x7f")
           (map integer->char '(0 0 1 32 95 124 127)))

     (run-read-tests-full-unicode)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; <identifier>
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;; <identifier> ::= <initial> <subsequent>*

     (test (read-data "a ! $ % & * / : < = > ? ^ _ ~ @")
           (map string->symbol
                '("a" "!" "$" "%" "&" "*" "/" ":"
                  "<" "=" ">" "?" "^" "_" "~" "@")))

     ;; <subsequent> ::= <initial> | <digit> | <special subsequent>

     (test (read-data "Z: !z $0 %/ &? *^ /~ :@ <9 =+ >- ?@ ^+- _-+ ~@- @@@@@")
           (map string->symbol
                '("Z:" "!z" "$0" "%/" "&?" "*^" "/~" ":@"
                  "<9" "=+" ">-" "?@" "^+-" "_-+" "~@-" "@@@@@")))

     ;; <identifier> ::= <vertical line> <symbol element>* <vertical line>

     (test (read-datum "|;alskjdf;aqwjepojq-1945apgf ;bna]as|")
           (string->symbol ";alskjdf;aqwjepojq-1945apgf ;bna]as"))

     ;; |\a\b\t\n\r\|\"\\|

     (test (read-datum (string #\|
                        #\\ #\a
                        #\\ #\b
                        #\\ #\t
                        #\\ #\n
                        #\\ #\r
                        #\\ #\|
                        #\\ #\"
                        #\\ #\\
                        #\|))
           (string->symbol
            (string #\alarm #\backspace #\tab #\newline #\return
                    #\| #\" #\\)))

     (test (read-data "|\\\\\\\|\\\" a| |\\\"\\\|\\\\ b|")
           (map string->symbol
                (list (string #\\ #\| #\" #\space #\a)
                      (string #\" #\| #\\ #\space #\b))))

     (test (read-datum "|\x000;\x01;\x2;\t\r\x41;\n\t\x7e;\x7f;|")
          (string->symbol
           (string #\null #\x1 #\x2 #\tab #\return #\A
                   #\newline #\tab #\~ #\delete)))

     ;; <peculiar identifier>

     ;; <explicit sign>

     (test (read-data "+ -") '(+ -))

     ;; <explicit sign> <sign subsequent> <subsequent>*

     (test (read-data "+: -@ +- -- +@ -@ +$$ -@3 +-4 --5 +@_ -@.")
           (map string->symbol
                '("+:" "-@" "+-" "--" "+@" "-@" "+$$"
                  "-@3" "+-4" "--5" "+@_" "-@.")))

     ;; <explicit sign> . <dot subsequent> <subsequent>*

     (test (read-data "+.! -.+ +.. -.. +.@ -.@")
           (map string->symbol
                '("+.!" "-.+" "+.." "-.." "+.@" "-.@")))

     (test (read-data "+.<.+ -.++. +..:? -..- +.@&. -.@24")
           (map string->symbol
                '("+.<.+" "-.++." "+..:?" "-..-" "+.@&." "-.@24")))

     ;; . <dot subsequent> <subsequent>*

     (test (read-data "._ .+ .- .@ .. ._. .+. .-. .@. ...")
           (map string->symbol
                '("._" ".+" ".-" ".@" ".."
                  "._." ".+." ".-." ".@." "...")))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; <number>
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (run-read-tests-exact-closed)

     (run-read-tests-exact-complex)

     (run-read-tests-ieee-float)

     (run-read-tests-ratios)

     (run-read-tests-inexact)

     (run-read-tests-inexact-complex)

     ;; small exact integers

     (test (read-data "0 +0 -0 00 +0000 -00000 001 2 -003 007 97 1001")
           '(0 0 0 0 0 0 1 2 -3 7 97 1001))

     (test (read-data (string-append "#e0 #e+0 #e-0 #e00 #e+0000 #e-00000 "
                                     "#e001 #e2 #e-003 #e007 #e97 #e1001"))
           '(0 0 0 0 0 0 1 2 -3 7 97 1001))

     (test (read-data (string-append "#d0 #d+0 #d-0 #d00 #d+0000 #d-00000 "
                                     "#d001 #d2 #d-003 #d007 #d97 #d1001"))
           '(0 0 0 0 0 0 1 2 -3 7 97 1001))

     (test (read-data
            (string-append "#e#d0 #e#d+0 #e#d-0 #e#d00 #e#d+00 #e#d-0000 "
                           "#e#d01 #e#d2 #e#d-3 #e#d007 #e#d97 #e#d1001"))
           '(0 0 0 0 0 0 1 2 -3 7 97 1001))

     (test (read-data
            (string-append "#d#e0 #d#e+0 #d#e-0 #d#e00 #d#e+00 #d#e-0000 "
                           "#d#e01 #d#e2 #d#e-3 #d#e007 #d#e97 #d#e1001"))
           '(0 0 0 0 0 0 1 2 -3 7 97 1001))

     (test (read-data (string-append "#x0 #x+0 #x-0 #x00 #x+0000 #x-00000 "
                                     "#x001 #x2 #x-003 #x007 #x97 #x1001 "
                                     "#xabcd #x-fedc"))
           '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244))

     (test (read-data
            (string-append "#e#x0 #e#x+0 #e#x-0 #e#x00 #e#x+00 #e#x-0000 "
                           "#e#x01 #e#x2 #e#x-3 #e#x007 #e#x97 #e#x1001 "
                           "#xabcd #x-fedc"))
           '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244))

     (test (read-data
            (string-append "#x#e0 #x#e+0 #x#e-0 #x#e00 #x#e+00 #x#e-0000 "
                           "#x#e01 #x#e2 #x#e-3 #x#e007 #x#e97 #x#e1001 "
                           "#xabcd #x-fedc"))
           '(0 0 0 0 0 0 1 2 -3 7 151 4097 43981 -65244))

     (test (read-data (string-append "#o0 #o+0 #o-0 #o00 #o+0000 #o-00000 "
                                     "#o001 #o2 #o-003 #o007 #o77 #o1001"))
           '(0 0 0 0 0 0 1 2 -3 7 63 513))

     (test (read-data
            (string-append "#e#o0 #e#o+0 #e#o-0 #e#o00 #e#o+00 #e#o-0000 "
                           "#e#o01 #e#o2 #e#o-3 #e#o007 #e#o77 #e#o1001"))
           '(0 0 0 0 0 0 1 2 -3 7 63 513))

     (test (read-data
            (string-append "#o#e0 #o#e+0 #o#e-0 #o#e00 #o#e+00 #o#e-0000 "
                           "#o#e01 #o#e2 #o#e-3 #o#e007 #o#e77 #o#e1001"))
           '(0 0 0 0 0 0 1 2 -3 7 63 513))

     (test (read-data (string-append "#b0 #b+0 #b-0 #b00 #b+000 #b-000 "
                                     "#b001 #b1001 #b-111111 #b+10101010"))
           '(0 0 0 0 0 0 1 9 -63 170))

     (test (read-data
            (string-append "#e#b0 #e#b+0 #e#b-0 #e#b00 #e#b+000 #e#b-000 "
                           "#e#b001 #e#b1001 #e#b-111111 #e#b+10101010"))
           '(0 0 0 0 0 0 1 9 -63 170))

     (test (read-data
            (string-append "#b#e0 #b#e+0 #b#e-0 #b#e00 #b#e+000 #b#e-000 "
                           "#b#e001 #b#e1001 #b#e-111111 #b#e+10101010"))
           '(0 0 0 0 0 0 1 9 -63 170))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; Other tokens and comments.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (read-data "( ) #( ) #u8( ) 'x `x ,x ,@x (a . b)")
           (list (list)
                 (vector)
                 (bytevector)
                 '(quote x)
                 '(quasiquote x)
                 '(unquote x)
                 '(unquote-splicing x)
                 (cons 'a 'b)))

     (test (read-data "yes; this is a comment \nbut;\r\n this is;too\rnot")
           '(yes but this is not))

     (test (read-data "1 #;2 3 #;(4 5) 6 #;(7 (8)) 9")
           '(1 3 6 9))

     (test (read-data
            "#|yes; this is |#a comment #|\nbut;\r\n|# this is #;too\rnot")
           '(a comment this is not))

     (test (read-data "and #|they #|nest|# like|# this")
           '(and this))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; External representations of data.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (test (read-data "#u8() #u8(0 1 2 3 255)")
           (list (bytevector) (bytevector 0 1 2 3 255)))

     (test (read-data "() (1 . 2) (3 . (4 . (5 . ())))")
           (list (list) (cons 1 2) (list 3 4 5)))

     (test (read-data "#() #(a) #(19 21 c)")
           (list (vector) (vector 'a) (vector 19 21 'c)))

     (test (read-datum "#125=#(#213=(1 2 #125# 4 . #213#) (#125#) #213#)")
           (let* ((x (list 1 2 3 4 5))
                  (v (vector x x x)))
             (set-car! (cddr x) v)
             (set-cdr! (cdr (cdr (cdr x))) x)
             (vector-set! v 1 (list v))
             v))

     )))
