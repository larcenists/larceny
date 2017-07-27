;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$

#!r7rs            ;; FIXME: for now, assumes and tests R7RS lexical conventions

(read-r7rs-weirdness?)     ;; allows R7RS lexical syntax in output string ports

(define (run-print-tests)
  (display "Print") (newline)
  (basic-print-tests)

  ; There is little point to testing Unicode issues
  ; if Unicode strings aren't supported in the system
  ; we're testing.

  (let* ((string-rep (cdr (assq 'string-representation (system-features))))
         (unicode? (not (eq? string-rep 'flat1))))
    (if unicode?
        (begin
         ;(basic-unicode-print-tests)        ; FIXME
         ;(exhaustive-unicode-print-tests)   ; FIXME
         ))))

(define (print-test name printer x expected)
  (let ((out (open-output-string)))
    (printer x out)
    (test name (get-output-string out) expected)))

;;; In many cases, the standards allow more than one way to print an object.
;;; These tests show how we want objects to be printed in Larceny.
;;; FIXME: Larceny developers can express their taste by changing these tests.

(define (basic-print-tests)
  (allof "basic print tests"

   (print-test "R7RS one-character identifiers"
               write
               '(a z A Z ! $ % ^ * / : < = > ? ^ + ~ @ + -)
               "(a z A Z ! $ % ^ * / : < = > ? ^ + ~ @ + -)")

   ;; Note: There is no R7RS-compatible output syntax for these identifiers
   ;; that could be read by a strict R6RS read procedure, so we might as
   ;; well have the write procedure produce the most readable R7RS syntax.
   ;;
   ;; FIXME:  The results of the following test aren't mandated by R7RS,
   ;; so it's commented out and replaced by two tests the current write
   ;; procedure can pass.

   ' ; FIXME
   (print-test "R7RS peculiar identifiers"
               write
               '(+ - ++ -@ +: -? +%1 -!. +@@ +.- -.@ -.. .. ... ..+ .@)
               "(+ - ++ -@ +: -? +%1 -!. +@@ +.- -.@ -.. .. ... ..+ .@)")

   (print-test "R7RS peculiar identifiers"
               write
               '(+ - .. ... ..+ .@)
               "(+ - .. ... ..+ .@)")

   (print-test "R7RS peculiar identifiers"
               (lambda (x out)
                 (let* ((q (open-output-string))
                        (s (begin (write x q) (get-output-string q)))
                        (p (open-input-string s)))
                   (if (equal? x (read p))
                       (display "okay" out)
                       (display "not okay" out))))
               '(+ - ++ -@ +: -? +%1 -!. +@@ +.- -.@ -.. .. ... ..+ .@)
               "okay")

   (print-test "R7RS two-character identifiers"
               write
               '(!0 %. @@ :> /. !! $$ %% ** // :: << ?? ^^ __ ~~)
               "(!0 %. @@ :> /. !! $$ %% ** // :: << ?? ^^ __ ~~)")

   (print-test "R7RS escaped identifiers with normal characters"
               write
               '(|| |"a"b| |09| |{[}]| |20c| |5.4| |+i|)
               "(|| |\x22;a\x22;b| |09| |{[}]| |20c| |5.4| |+i|)")

   (print-test "R7RS escaped identifiers with whitespace"
               write
               '(| a b| | 9 | |    | |\a\b\t\n\r| |5\t4| |4 5| |a\|b|)
               (string-append
                "(\| a b\| \| 9 \| \|    \| "
                "\|\\a\\b\\t\\n\\r\| \|5\\t4\| \|4 5\| \|a\\\|b\|)"))

   (print-test "R7RS escaped identifiers with weird characters"
               write
               '(|a\|b| |\x0;\x1;\x2;\x3;\x7f;|)
               "(\|a\\\|b\| \|\\x0;\\x1;\\x2;\\x3;\\x7f;\|)")

   (print-test "R7RS booleans"
               write
               '(#t #f #T #F #true #false #TRUE #FALSE #tRUe #False)
               "(#t #f #t #f #t #f #t #f #t #f)")

   (print-test "#!true and #!false"
               write
               '(#!true #!false)
               "(#t #f)")

   (print-test "radix prefixes"
               write
               '(27 #o27 #d27 #x27 0101 #b0101 #o0101 #d0101 #x0101)
               "(27 23 27 39 101 5 65 101 257)")

   (print-test "exactness and radix prefixes"
               write
               '(#e27 #e#o27 #e#d27 #e#x27
                 #e0101 #e#b0101 #e#o0101 #e#d0101 #e#x0101
                 #o#e27 #d#e27 #x#e27
                 #e0101 #b#e0101 #o#e0101 #d#e0101 #x#e0101)
               (string-append
                "(27 23 27 39 101 "
                "5 65 101 257 "
                "23 27 39 101 "
                "5 65 101 257)"))

   (print-test "exactness and radix prefixes"
               write
               '(#i27 #i#o27 #i#d27 #i#x27
                 #i0101 #i#b0101 #i#o0101 #i#d0101 #i#x0101
                 #o#i27 #d#i27 #x#i27
                 #i0101 #b#i0101 #o#i0101 #d#i0101 #x#i0101)
               (string-append
                "(27.0 23.0 27.0 39.0 101.0 "
                "5.0 65.0 101.0 257.0 "
                "23.0 27.0 39.0 101.0 "
                "5.0 65.0 101.0 257.0)"))

   ;; FIXME: for now, all NaNs print as +nan.0

   (print-test "infinities and NaNs"
               write
               '(+inf.0 -inf.0 +nan.0 -nan.0 +INF.0 -iNf.0 +NaN.0 -nAN.0)
               "(+inf.0 -inf.0 +nan.0 +nan.0 +inf.0 -inf.0 +nan.0 +nan.0)")

   (print-test "purely imaginary numbers"
               write
               '(+i -i -inf.0i +nan.0i -3i -4/3i #i#o-10i #x#e+bi)
               "(0+1i 0-1i 0.0-inf.0i 0.0+nan.0i 0-3i 0-4/3i 0.0-8.0i 0+11i)")

   (print-test "complex number with infinite or NaN imaginary part"
               write
               '(3+inf.0i 4-nan.0i -5-INF.0i -6+NaN.0i)
               "(3.0+inf.0i 4.0+nan.0i -5.0-inf.0i -6.0+nan.0i)")

   (print-test "complex numbers"
               write
               '(0+i 0-i -4+i +5-i 5-6i +7+8i 9@0)
               "(0+1i 0-1i -4+1i 5-1i 5-6i 7+8i 9.0+0.0i)")

   (print-test "inexact complex numbers"
               write
               '(0.0+i 0.0-i -4.0+i +5.0-i 5.0-6i +7+8.0i 9@0.0)
               (string-append
                "(0.0+1.0i 0.0-1.0i -4.0+1.0i 5.0-1.0i "
                "5.0-6.0i 7.0+8.0i 9.0+0.0i)"))

   (print-test "exact fractions"
               write
               '(-1/10 1/10 #b1/10 #o-1/10 #d1/10 #x-1/10)
               "(-1/10 1/10 1/2 -1/8 1/10 -1/16)")

   (print-test "inexact fractions"
               write
               '(#i-1/10 #i1/10 #i#b1/10 #o#i-1/10 #i#d1/10 #x#i-1/10)
               "(-0.1 0.1 0.5 -0.125 0.1 -0.0625)")

   (print-test "decimal notations"
               write
               '(#e1e6 1e7 1.e-8 .1e9 #e.1e-9)
               "(1000000 10000000.0 1.0e-8 100000000.0 1/10000000000)")

   (print-test "characters"
               write
               '(#\  #\\ #\x #\x7)
               "(#\\space #\\\\ #\\x #\\alarm)")               

   (print-test "character names"
               write
               '(#\alarm #\backspace #\delete #\escape #\newline
                 #\null #\return #\space #\tab)
               (string-append
                "(#\\alarm #\\backspace #\\delete #\\escape "
                "#\\newline #\\null #\\return #\\space #\\tab)"))

   (print-test "strings"
               write
               '("" "a" "\"" "\\" "\\\|\a\b\t\n\r")
               "(\"\" \"a\" \"\\\"\" \"\\\\\" \"\\\\\|\\a\\b\\t\\n\\r\")")

   (print-test "bytevectors"
               write
               '(#u8() #u8(0 1 254 255))
               "(#u8() #u8(0 1 254 255))")

   (print-test "lists"
               write
               '(() (a) (a . b) (a b c . d) (a b c . (d)) (a b c . (((d)))))
               "(() (a) (a . b) (a b c . d) (a b c d) (a b c ((d))))")

   (print-test "vectors"
               write
               '(#() #(a) #(1 2) #(3.4 5.6 7.8) ("hello" there))
               "(#() #(a) #(1 2) #(3.4 5.6 7.8) (\"hello\" there))")

   (print-test "abbreviations"
               write
               '('a `b ,c ,@d ''d '`'e ',',f ',@g)
               "('a `b ,c ,@d ''d '`'e ',',f ',@g)")

   (print-test "circular lists and vectors"
               write
               (let* ((v (make-vector 3 '*))
                      (x (list 1 2 v 4 5)))
                 (vector-set! v 0 (list x v))
                 (vector-set! v 1 x)
                 (vector-set! v 2 (vector v x))
                 (set-cdr! (cdddr x) (cdr x))
                 v)
               "#1=#((#2=(1 . #3=(2 #1# 4 . #3#)) #1#) #2# #(#1# #2#))")

   (print-test "immutable texts"
               write
               (map string->text
                    '("" "abc" "def\"\x00bb;\x00ab;g"))
               (string-append
                "(\x00ab;\x00bb; \x00ab;abc\x00bb; "
                "\x00ab;def\\\"\\xbb;\\xab;g\xbb;)"))

   (print-test "immutable texts (write/read/write)"
               (lambda (x q2)
                 (let ((q1 (open-output-string)))
                   (write x q1)
                   (write (read (open-input-string
                                 (get-output-string q1)))
                          q2)))
               (map string->text
                    '("" "abc" "def\"\x00bb;\x00ab;g"))
               (string-append
                "(\x00ab;\x00bb; \x00ab;abc\x00bb; "
                "\x00ab;def\\\"\\xbb;\\xab;g\xbb;)"))

   ))

; eof
