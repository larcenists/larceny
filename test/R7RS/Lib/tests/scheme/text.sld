;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; The copyright notices above are taken from srfi-135-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme text) procedures:
;;;
;;;     text?
;;;     textual?
;;;     textual-null?
;;;     textual-every
;;;     textual-any
;;;
;;;     make-text
;;;     text
;;;     text-tabulate
;;;     text-unfold
;;;     text-unfold-right
;;;
;;;     textual->text
;;;     textual->string
;;;     textual->vector
;;;     textual->list
;;;     string->text
;;;     vector->text
;;;     list->text
;;;     reverse-list->text
;;;     textual->utf8
;;;     textual->utf16be
;;;     textual->utf16
;;;     textual->utf16le
;;;     utf8->text
;;;     utf16be->text
;;;     utf16->text
;;;     utf16le->text
;;;
;;;     text-length
;;;     textual-length
;;;     text-ref
;;;     textual-ref
;;;     subtext
;;;     subtextual
;;;     textual-copy
;;;     textual-take
;;;     textual-take-right
;;;     textual-drop
;;;     textual-drop-right
;;;     textual-pad
;;;     textual-pad-right 
;;;     textual-trim
;;;     textual-trim-right
;;;     textual-trim-both
;;;
;;;     textual-replace
;;;
;;;     textual=?
;;;     textual-ci=?
;;;     textual<?
;;;     textual-ci<?
;;;     textual>?
;;;     textual-ci>?
;;;     textual<=?
;;;     textual-ci<=?
;;;     textual>=?
;;;     textual-ci>=?
;;;
;;;     textual-prefix-length
;;;     textual-suffix-length
;;;     textual-prefix?
;;;     textual-suffix?    
;;;
;;;     textual-index
;;;     textual-index-right
;;;     textual-skip
;;;     textual-skip-right
;;;     textual-contains
;;;     textual-contains-right
;;;
;;;     textual-upcase
;;;     textual-downcase
;;;     textual-foldcase
;;;     textual-titlecase
;;;
;;;     textual-append
;;;     textual-concatenate
;;;     textual-concatenate-reverse
;;;     textual-join
;;;
;;;     textual-fold
;;;     textual-fold-right
;;;     textual-map
;;;     textual-for-each
;;;     textual-map-index
;;;     textual-for-each-index
;;;     textual-count
;;;     textual-filter
;;;     textual-remove
;;;
;;;     textual-replicate
;;;     textual-split



(define-library (tests scheme text)
  (export run-text-tests)
  (import (scheme base)
          (scheme text)
          (tests scheme test)
          (scheme char))

  ;; Adapted from srfi-135-test.sps7

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define-syntax test/=
     (syntax-rules ()
      ((test/= expr1 expr2)
       (test-assert (= expr1 expr2)))))

   ;; Help functions for testing.

   (define (as-text . args)
     (textual-concatenate
      (map (lambda (x)
             (cond ((text? x) x)
                   ((string? x) (string->text x))
                   ((char? x) (text x))
                   (else
                    (error "as-text: illegal argument" x))))
           args)))

   (define (result=? str txt)
     (and (text? txt)
          (textual=? str txt)))

   ;; Unicode is a strong motivation for immutable texts, so we ought
   ;; to use at least some non-ASCII strings for testing.
   ;; Some systems would blow up if this file were to contain non-ASCII
   ;; characters, however, so we have to be careful here.
   ;;
   ;; FIXME: need more tests with really high code points

   (cond-expand ((or sagittarius
                     chibi
                     full-unicode-strings)
                 (define ABC
                   (as-text
                    (list->string (map integer->char
                                       '(#x3b1 #x3b2 #x3b3)))))
                 (define ABCDEF
                   (as-text
                    (list->string
                     (map integer->char
                          '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066)))))
                 (define DEFABC
                   (as-text
                    (list->string
                     (map integer->char
                          '(#x064 #x0c9 #x066 #x0c0 #x062 #x0c7)))))
                 (define eszett (integer->char #xDF))
                 (define fuss (text #\F #\u eszett))
                 (define chaos0
                   (as-text
                    (list->string (map integer->char
                                       '(#x39E #x391 #x39F #x3A3)))))
                 (define chaos1
                   (as-text
                    (list->string (map integer->char
                                       '(#x3BE #x3B1 #x3BF #x3C2)))))
                 (define chaos2
                   (as-text
                    (list->string (map integer->char
                                       '(#x3BE #x3B1 #x3BF #x3C3)))))
                 (define beyondBMP
                   (as-text
                    (list->string
                     (map integer->char
                          '(#x61 #xc0 #x3bf
                                 #x1d441 #x1d113 #x1d110 #x7a))))))
                (else
                 (define ABC (as-text "abc"))
                 (define ABCDEF (as-text "ABCdef"))
                 (define DEFABC (as-text "defabc"))))


   (define (run-text-tests)

     ;; Predicates

     (test-assert (text? (text)))

     (test-assert (not (text? (string))))

     (test-assert (not (text? #\a)))

     (test-assert (textual? (text)))

     (test-assert (textual? (string)))

     (test-assert (not (textual? #\a)))

     (test-assert (textual-null? (text)))

     (test-assert (not (textual-null? ABC)))


     (test-assert
      (eqv? #t (textual-every (lambda (c) (if (char? c) c #f))
                              (text))))

     (test-assert
      (eqv? #\c (textual-every (lambda (c) (if (char? c) c #f))
                               (as-text "abc"))))

     (test-assert
      (eqv? #f (textual-every (lambda (c) (if (char>? c #\b) c #f))
                              (as-text "abc"))))

     (test-assert
      (eqv? #\c (textual-every (lambda (c) (if (char>? c #\b) c #f))
                               (as-text "abc") 2)))

     (test-assert
      (eqv? #t (textual-every (lambda (c) (if (char>? c #\b) c #f))
                              (as-text "abc") 1 1)))

     (test-assert
      (eqv? #f (textual-any (lambda (c) (if (char? c) c #f))
                            (text))))

     (test-assert
      (eqv? #\a (textual-any (lambda (c) (if (char? c) c #f))
                             (as-text "abc"))))

     (test-assert
      (eqv? #\c (textual-any (lambda (c) (if (char>? c #\b) c #f))
                             (as-text "abc"))))

     (test-assert
      (eqv? #\c (textual-any (lambda (c) (if (char>? c #\b) c #f))
                             (as-text "abc") 2)))

     (test-assert
      (eqv? #f (textual-any (lambda (c) (if (char>? c #\b) c #f))
                            (as-text "abc") 0 2)))


     (test-assert
      (eqv? #t (textual-every (lambda (c) (if (char? c) c #f)) "")))

     (test-assert
      (eqv? #\c (textual-every (lambda (c) (if (char? c) c #f)) "abc")))

     (test-assert
      (eqv? #f (textual-every (lambda (c) (if (char>? c #\b) c #f)) "abc")))

     (test-assert
      (eqv? #\c (textual-every (lambda (c) (if (char>? c #\b) c #f))
                               "abc" 2)))

     (test-assert
      (eqv? #t (textual-every (lambda (c) (if (char>? c #\b) c #f))
                              "abc" 1 1)))

     (test-assert
      (eqv? #f (textual-any (lambda (c) (if (char? c) c #f)) "")))

     (test-assert
      (eqv? #\a (textual-any (lambda (c) (if (char? c) c #f)) "abc")))

     (test-assert
      (eqv? #\c (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc")))

     (test-assert
      (eqv? #\c (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)))

     (test-assert
      (eqv? #f (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2)))


     ;; Constructors

     (test-assert
      (result=? ""
                (text-tabulate (lambda (i)
                                 (integer->char (+ i (char->integer #\a))))
                               0)))

     (test-assert
      (result=? "abc"
                (text-tabulate (lambda (i)
                                 (integer->char (+ i (char->integer #\a))))
                               3)))


     (test-assert
      (result=? "abc"
                (let ((p (open-input-string "abc")))
                  (text-unfold eof-object?
                               values
                               (lambda (x) (read-char p))
                               (read-char p)))))

     (test-assert
      (result=? "" (text-unfold null? car cdr '())))

     (test-assert
      (result=? "abc"
                (text-unfold null? car cdr (string->list "abc"))))

     (test-assert
      (result=? "def"
                (text-unfold null? car cdr '() (string->text "def"))))

     (test-assert
      (result=? "defabcG"
                (text-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             (string->text "def")
                             (lambda (x) (if (null? x) (text #\G) "")))))

     (test-assert
      (result=? "" (text-unfold-right null? car cdr '())))

     (test-assert
      (result=? "cba"
                (text-unfold-right null? car cdr (string->list "abc"))))

     (test-assert
      (result=? "def"
                (text-unfold-right null? car cdr '() (string->text "def"))))

     (test-assert
      (result=? "Gcbadef"
                (text-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   (string->text "def")
                                   (lambda (x) (if (null? x) (text #\G) "")))))


     (test-assert
      (result=? "def"
                (text-unfold null? car cdr '() "def")))

     (test-assert
      (result=? "defabcG"
                (text-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             "def"
                             (lambda (x) (if (null? x) "G" "")))))

     (test-assert
      (result=? "dabcG"
                (text-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             #\d
                             (lambda (x) (if (null? x) "G" "")))))

     (test-assert
      (result=? (string-append "%="
                               (make-string 200 #\*)
                               "A B C D E F G H I J K L M "
                               "N O P Q R S T U V W X Y Z "
                               (make-string (* 200 (- (char->integer #\a)
                                                      (char->integer #\Z)
                                                      1))
                                            #\*)
                               "abcdefghijklmnopqrstuvwxyz"
                               " ")
                (text-unfold (lambda (n) (char>? (integer->char n) #\z))
                             (lambda (n)
                               (let ((c (integer->char n)))
                                 (cond ((char<=? #\a c #\z) c)
                                       ((char<=? #\A c #\Z) (text c #\space))
                                       (else (make-string 200 #\*)))))
                             (lambda (n) (+ n 1))
                             (char->integer #\@)
                             "%="
                             (lambda (n) #\space))))

     (test-assert
      (result=? "def"
                (text-unfold-right null? car cdr '() "def")))

     (test-assert
      (result=? "Gcbadef"
                (text-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   "def"
                                   (lambda (x) (if (null? x) "G" "")))))

     (test-assert
      (result=? "Gcbad"
                (text-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   #\d
                                   (lambda (x) (if (null? x) "G" "")))))

     (test-assert
      (result=? (string-append " "
                               (list->string
                                (reverse
                                 (string->list "abcdefghijklmnopqrstuvwxyz")))
                               (make-string (* 200 (- (char->integer #\a)
                                                      (char->integer #\Z)
                                                      1))
                                            #\*)
                               "Z Y X W V U T S R Q P O N "
                               "M L K J I H G F E D C B A "
                               (make-string 200 #\*)
                               "%=")
                (text-unfold-right
                 (lambda (n) (char>? (integer->char n) #\z))
                 (lambda (n)
                   (let ((c (integer->char n)))
                     (cond ((char<=? #\a c #\z) c)
                           ((char<=? #\A c #\Z) (text c #\space))
                           (else (make-string 200 #\*)))))
                 (lambda (n) (+ n 1))
                 (char->integer #\@)
                 "%="
                 (lambda (n) #\space))))

     (test-assert
      (result=? " The English alphabet: abcdefghijklmnopqrstuvwxyz "
                (text-unfold-right (lambda (n) (< n (char->integer #\A)))
                                   (lambda (n)
                                     (char-downcase (integer->char n)))
                                   (lambda (n) (- n 1))
                                   (char->integer #\Z)
                                   #\space
                                   (lambda (n) " The English alphabet: "))))


     ;; Conversion

     (test-assert (let ((txt (textual->text "str")))
                    (and (text? txt)
                         (textual=? txt "str"))))

     (test-assert (let ((txt (textual->text (text #\s #\t #\r))))
                    (and (text? txt)
                         (textual=? txt "str"))))


     (test-assert (string=? "" (textual->string (text))))

     (test-assert (string=? "" (textual->string (text) 0)))

     (test-assert (string=? "" (textual->string (text) 0 0)))

     (test-assert (string=? "abc" (textual->string (text #\a #\b #\c))))

     (test-assert (string=? "" (textual->string (text #\a #\b #\c) 3)))

     (test-assert (string=? "bc" (textual->string (text #\a #\b #\c) 1 3)))


     (test-assert (string=? "" (textual->string "")))

     (test-assert (string=? "" (textual->string "" 0)))

     (test-assert (string=? "" (textual->string "" 0 0)))

     (test-assert (string=? "abc" (textual->string "abc")))

     (test-assert (string=? "" (textual->string "abc" 3)))

     (test-assert (string=? "bc" (textual->string "abc" 1 3)))


     (test '#() (textual->vector (text)))

     (test '#() (textual->vector (text) 0))

     (test '#() (textual->vector (text) 0 0))

     (test '#(#\a #\b #\c) (textual->vector (text #\a #\b #\c)))

     (test '#() (textual->vector (text #\a #\b #\c) 3))

     (test '#(#\b #\c) (textual->vector (text #\a #\b #\c) 1 3))


     (test '#() (textual->vector ""))

     (test '#() (textual->vector "" 0))

     (test '#() (textual->vector "" 0 0))

     (test '#(#\a #\b #\c) (textual->vector "abc"))

     (test '#() (textual->vector "abc" 3))

     (test '#(#\b #\c) (textual->vector "abc" 1 3))


     (test '() (textual->list (text)))

     (test '() (textual->list (text) 0))

     (test '() (textual->list (text) 0 0))

     (test '(#\a #\b #\c) (textual->list (text #\a #\b #\c)))

     (test '() (textual->list (text #\a #\b #\c) 3))

     (test '(#\b #\c) (textual->list (text #\a #\b #\c) 1 3))


     (test '() (textual->list ""))

     (test '() (textual->list "" 0))

     (test '() (textual->list "" 0 0))

     (test '(#\a #\b #\c) (textual->list "abc"))

     (test '() (textual->list "abc" 3))

     (test '(#\b #\c) (textual->list "abc" 1 3))


     (test-assert (result=? "" (string->text "")))

     (test-assert (result=? "" (string->text "" 0)))

     (test-assert (result=? "" (string->text "" 0 0)))

     (test-assert (result=? "abc" (string->text "abc")))

     (test-assert (result=? "bc" (string->text "abc" 1)))

     (test-assert (result=? "" (string->text "abc" 3)))

     (test-assert (result=? "b" (string->text "abc" 1 2)))

     (test-assert (result=? "bc" (string->text "abc" 1 3)))


     (test-assert (result=? "" (vector->text '#())))

     (test-assert (result=? "" (vector->text '#() 0)))

     (test-assert (result=? "" (vector->text '#() 0 0)))

     (test-assert (result=? "abc" (vector->text '#(#\a #\b #\c))))

     (test-assert (result=? "bc" (vector->text '#(#\a #\b #\c) 1)))

     (test-assert (result=? "" (vector->text '#(#\a #\b #\c) 3)))

     (test-assert (result=? "b" (vector->text '#(#\a #\b #\c) 1 2)))

     (test-assert (result=? "bc" (vector->text '#(#\a #\b #\c) 1 3)))


     (test-assert (result=? "" (list->text '())))

     (test-assert (result=? "" (list->text '() 0)))

     (test-assert (result=? "" (list->text '() 0 0)))

     (test-assert (result=? "abc" (list->text '(#\a #\b #\c))))

     (test-assert (result=? "bc" (list->text '(#\a #\b #\c) 1)))

     (test-assert (result=? "" (list->text '(#\a #\b #\c) 3)))

     (test-assert (result=? "b" (list->text '(#\a #\b #\c) 1 2)))

     (test-assert (result=? "bc" (list->text '(#\a #\b #\c) 1 3)))


     (test-assert (result=? "" (reverse-list->text '())))

     (test-assert (result=? "cba" (reverse-list->text '(#\a #\b #\c))))


     (test '#u8(97 98 99)
           (textual->utf8 (as-text "abc")))

     (test '#u8(97 98 99)
           (textual->utf8 "abc"))

     (test '#u8(97 98 99 121 121 121 122 122 122)
           (textual->utf8 (as-text "xxxabcyyyzzz") 3))

     (test '#u8(97 98 99 121 121 121 122 122 122)
           (textual->utf8 "xxxabcyyyzzz" 3))

     (test '#u8(97 98 99)
           (textual->utf8 (as-text "xxxabcyyyzzz") 3 6))

     (test '#u8(97 98 99)
           (textual->utf8 "xxxabcyyyzzz" 3 6))


     (test '#u8(254 255 0 97 0 98 0 99)
           (textual->utf16 (as-text "abc")))

     (test '#u8(254 255 0 97 0 98 0 99)
           (textual->utf16 "abc"))

     (test '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
           (textual->utf16 (as-text "xxxabcyyyzzz") 3))

     (test '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
           (textual->utf16 "xxxabcyyyzzz" 3))

     (test '#u8(254 255 0 97 0 98 0 99)
           (textual->utf16 (as-text "xxxabcyyyzzz") 3 6))

     (test '#u8(254 255 0 97 0 98 0 99)
           (textual->utf16 "xxxabcyyyzzz" 3 6))


     (test '#u8(0 97 0 98 0 99)
           (textual->utf16be (as-text "abc")))

     (test '#u8(0 97 0 98 0 99)
           (textual->utf16be "abc"))

     (test '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
           (textual->utf16be (as-text "xxxabcyyyzzz") 3))

     (test '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
           (textual->utf16be "xxxabcyyyzzz" 3))

     (test '#u8(0 97 0 98 0 99)
           (textual->utf16be (as-text "xxxabcyyyzzz") 3 6))

     (test '#u8(0 97 0 98 0 99)
           (textual->utf16be "xxxabcyyyzzz" 3 6))


     (test '#u8(97 0 98 0 99 0)
           (textual->utf16le (as-text "abc")))

     (test '#u8(97 0 98 0 99 0)
           (textual->utf16le "abc"))

     (test '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
           (textual->utf16le (as-text "xxxabcyyyzzz") 3))

     (test '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
           (textual->utf16le "xxxabcyyyzzz" 3))

     (test '#u8(97 0 98 0 99 0)
           (textual->utf16le (as-text "xxxabcyyyzzz") 3 6))

     (test '#u8(97 0 98 0 99 0)
           (textual->utf16le "xxxabcyyyzzz" 3 6))


     (test-assert
      (result=? "abc"
                (utf8->text '#u8(97 98 99))))

     (test-assert
      (result=? "abcyyyzzz"
                (utf8->text '#u8(0 1 2 97 98 99 121 121 121 122 122 122) 3)))

     (test-assert
      (result=? "abc"
                (utf8->text '#u8(41 42 43 97 98 99 100 101 102) 3 6)))


     (test-assert
      (result=? "abc"
                (utf16->text '#u8(254 255 0 97 0 98 0 99))))

     (test-assert
      (result=? "abc"
                (utf16->text '#u8(255 254 97 0 98 0 99 0))))

     (test-assert
      (result=? "abc"
                (utf16->text (textual->utf16 "abc") 2)))

     (test-assert
      (result=? "bcdef"
                (utf16->text (textual->utf16 "abcdef") 4)))

     (test-assert
      (result=? "bcd"
                (utf16->text (textual->utf16 "abcdef") 4 10)))


     (test-assert
      (result=? "abc"
                (utf16be->text '#u8(0 97 0 98 0 99))))

     (test-assert
      (result=? "bc"
                (utf16be->text (textual->utf16be "abc") 2)))

     (test-assert
      (result=? "bcd"
                (utf16be->text (textual->utf16be "abcdef") 2 8)))


     (test-assert
      (result=? "abc"
                (utf16le->text '#u8(97 0 98 0 99 0))))

     (test-assert
      (result=? "bc"
                (utf16le->text (textual->utf16le "abc") 2)))

     (test-assert
      (result=? "bcd"
                (utf16le->text (textual->utf16le "abcdef") 2 8)))


     (cond-expand
      ((or sagittarius
           chibi
           full-unicode-strings)

       (test '#u8(97 195 128 206 191
                     240 157 145 129 240 157 132 147 240 157 132 144 122)
             (textual->utf8 beyondBMP))

       (let ((bv (textual->utf16 beyondBMP)))
         (test-assert
          (or (equal? bv
                      '#u8(254 255 0 97 0 192 3 191
                           216 53 220 65 216 52 221 19 216 52 221 16 0 122))
              (equal? bv
                      '#u8(255 254 97 0 192 0 191 3
                           53 216 65 220 52 216 19 221 52 216 16 221 122 0)))))

       (test '#u8(0 97 0 192 3 191
                    216 53 220 65 216 52 221 19 216 52 221 16 0 122)
             (textual->utf16be beyondBMP))

       (test '#u8(97 0 192 0 191 3
                     53 216 65 220 52 216 19 221 52 216 16 221 122 0)
             (textual->utf16le beyondBMP))

       (test-assert
        (textual=?
         beyondBMP
         (utf8->text
          '#u8(97 195 128 206 191
                  240 157 145 129 240 157 132 147 240 157 132 144 122))))

       (test-assert
        (textual=? beyondBMP
                   (utf16->text (textual->utf16 beyondBMP))))

       (test-assert
        (textual=? beyondBMP
                   (utf16->text (textual->utf16 beyondBMP) 2)))
       
       (test-assert
        (textual=? beyondBMP
                   (utf16be->text (textual->utf16be beyondBMP))))

       (test-assert
        (textual=? beyondBMP
                   (utf16le->text (textual->utf16le beyondBMP))))

       (test-assert
        (result=? (string-append (string (integer->char #xfeff)) "abc")
                  (utf16be->text '#u8(254 255 0 97 0 98 0 99))))

       (test-assert
        (result=? (string-append (string (integer->char #xfeff)) "abc")
                  (utf16le->text '#u8(255 254 97 0 98 0 99 0))))
       )

      (else))

     ;; Selection

     (test/= 0 (text-length (text)))

     (test/= 6 (text-length ABCDEF))

     (test/= 1234 (text-length (make-text 1234 (text-ref ABC 0))))


     (test-assert (char=? #\a (text-ref (text #\a #\b #\c) 0)))

     (test-assert (char=? #\c (text-ref (text #\a #\b #\c) 2)))

     (test-assert (char=? (string-ref (textual->string ABCDEF) 3)
                          (text-ref ABCDEF 3)))


     (test/= 0 (textual-length (text)))

     (test/= 6 (textual-length ABCDEF))

     (test/= 1234 (textual-length (make-text 1234 (text-ref ABC 0))))


     (test-assert (char=? #\a (textual-ref (text #\a #\b #\c) 0)))

     (test-assert (char=? #\c (textual-ref (text #\a #\b #\c) 2)))

     (test-assert (char=? (string-ref (textual->string ABCDEF) 3)
                          (textual-ref ABCDEF 3)))


     (test-assert (result=? ""
                            (subtext (text) 0 0)))

     (test-assert (result=? ""
                            (subtext (string->text "abcdef") 0 0)))

     (test-assert (result=? ""
                            (subtext (string->text "abcdef") 4 4)))

     (test-assert (result=? ""
                            (subtext (string->text "abcdef") 6 6)))

     (test-assert (result=? "abcd"
                            (subtext (string->text "abcdef") 0 4)))

     (test-assert (result=? "cde"
                            (subtext (string->text "abcdef") 2 5)))

     (test-assert (result=? "cdef"
                            (subtext (string->text "abcdef") 2 6)))

     (test-assert (result=? "abcdef"
                            (subtext (string->text "abcdef") 0 6)))


     (test-assert (result=? ""
                            (subtextual (text) 0 0)))

     (test-assert (result=? ""
                            (subtextual (string->text "abcdef") 0 0)))

     (test-assert (result=? ""
                            (subtextual (string->text "abcdef") 4 4)))

     (test-assert (result=? ""
                            (subtextual (string->text "abcdef") 6 6)))

     (test-assert (result=? "abcd"
                            (subtextual (string->text "abcdef") 0 4)))

     (test-assert (result=? "cde"
                            (subtextual (string->text "abcdef") 2 5)))

     (test-assert (result=? "cdef"
                            (subtextual (string->text "abcdef") 2 6)))

     (test-assert (result=? "abcdef"
                            (subtextual (string->text "abcdef") 0 6)))


     (test-assert (result=? ""
                            (subtextual "" 0 0)))

     (test-assert (result=? ""
                            (subtextual "abcdef" 0 0)))

     (test-assert (result=? ""
                            (subtextual "abcdef" 4 4)))

     (test-assert (result=? ""
                            (subtextual "abcdef" 6 6)))

     (test-assert (result=? "abcd"
                            (subtextual "abcdef" 0 4)))

     (test-assert (result=? "cde"
                            (subtextual "abcdef" 2 5)))

     (test-assert (result=? "cdef"
                            (subtextual "abcdef" 2 6)))

     (test-assert (result=? "abcdef"
                            (subtextual "abcdef" 0 6)))


     (test-assert (result=? ""
                            (textual-copy (text))))

     (test-assert (let* ((txt (string->text "abcdef"))
                         (copy (textual-copy txt)))
                    (and (result=? "abcdef"
                                   copy)
                         (not (eqv? txt copy)))))


     (test-assert (result=? ""
                            (textual-copy "")))

     (test-assert (result=? "abcdef"
                            (textual-copy "abcdef")))


     (test-assert (result=? ""
                            (textual-copy (text) 0)))

     (test-assert (result=? "abcdef"
                            (textual-copy (string->text "abcdef") 0)))

     (test-assert (result=? "ef"
                            (textual-copy (string->text "abcdef") 4)))

     (test-assert (result=? ""
                            (textual-copy (string->text "abcdef") 6)))


     (test-assert (result=? ""
                            (textual-copy "" 0)))

     (test-assert (result=? "abcdef"
                            (textual-copy "abcdef" 0)))

     (test-assert (result=? "ef"
                            (textual-copy "abcdef" 4)))

     (test-assert (result=? ""
                            (textual-copy "abcdef" 6)))


     (test-assert (result=? ""
                            (textual-copy (text) 0 0)))

     (test-assert (result=? ""
                            (textual-copy (string->text "abcdef") 0 0)))

     (test-assert (result=? ""
                            (textual-copy (string->text "abcdef") 4 4)))

     (test-assert (result=? ""
                            (textual-copy (string->text "abcdef") 6 6)))

     (test-assert (result=? "abcd"
                            (textual-copy (string->text "abcdef") 0 4)))

     (test-assert (result=? "cde"
                            (textual-copy (string->text "abcdef") 2 5)))

     (test-assert (result=? "cdef"
                            (textual-copy (string->text "abcdef") 2 6)))

     (test-assert (result=? "abcdef"
                            (textual-copy (string->text "abcdef") 0 6)))


     (test-assert (result=? ""
                            (textual-copy "" 0 0)))

     (test-assert (result=? ""
                            (textual-copy "abcdef" 0 0)))

     (test-assert (result=? ""
                            (textual-copy "abcdef" 4 4)))

     (test-assert (result=? ""
                            (textual-copy "abcdef" 6 6)))

     (test-assert (result=? "abcd"
                            (textual-copy "abcdef" 0 4)))

     (test-assert (result=? "cde"
                            (textual-copy "abcdef" 2 5)))

     (test-assert (result=? "cdef"
                            (textual-copy "abcdef" 2 6)))

     (test-assert (result=? "abcdef"
                            (textual-copy "abcdef" 0 6)))


     (test-assert
      (result=? "" (textual-take (text) 0)))

     (test-assert
      (result=? "" (textual-take (string->text "abcdef") 0)))

     (test-assert
      (result=? "ab" (textual-take (string->text "abcdef") 2)))

     (test-assert
      (result=? "" (textual-drop (string->text "") 0)))

     (test-assert
      (result=? "abcdef" (textual-drop (string->text "abcdef") 0)))

     (test-assert
      (result=? "cdef" (textual-drop (string->text "abcdef") 2)))

     (test-assert
      (result=? "" (textual-take-right (text) 0)))

     (test-assert
      (result=? "" (textual-take-right (string->text "abcdef") 0)))

     (test-assert
      (result=? "ef" (textual-take-right (string->text "abcdef") 2)))

     (test-assert
      (result=? "" (textual-drop-right (text) 0)))

     (test-assert
      (result=? "abcdef"
                (textual-drop-right (string->text "abcdef") 0)))

     (test-assert
      (result=? "abcd"
                (textual-drop-right (string->text "abcdef") 2)))


     (test-assert (result=? "" (textual-take "" 0)))

     (test-assert (result=? "" (textual-take "abcdef" 0)))

     (test-assert (result=? "ab" (textual-take "abcdef" 2)))

     (test-assert (result=? "" (textual-drop "" 0)))

     (test-assert (result=? "abcdef" (textual-drop "abcdef" 0)))

     (test-assert (result=? "cdef" (textual-drop "abcdef" 2)))

     (test-assert (result=? "" (textual-take-right "" 0)))

     (test-assert (result=? "" (textual-take-right "abcdef" 0)))

     (test-assert (result=? "ef" (textual-take-right "abcdef" 2)))

     (test-assert (result=? "" (textual-drop-right "" 0)))

     (test-assert (result=? "abcdef" (textual-drop-right "abcdef" 0)))

     (test-assert (result=? "abcd" (textual-drop-right "abcdef" 2)))


     (test-assert
      (result=? "" 
                (textual-pad (string->text "") 0)))

     (test-assert
      (result=? "     " 
                (textual-pad (string->text "") 5)))

     (test-assert
      (result=? "  325" 
                (textual-pad (string->text "325") 5)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "71325") 5)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "8871325") 5)))

     (test-assert
      (result=? "" 
                (textual-pad (string->text "") 0 #\*)))

     (test-assert
      (result=? "*****" 
                (textual-pad (string->text "") 5 #\*)))

     (test-assert
      (result=? "**325" 
                (textual-pad (string->text "325") 5 #\*)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "71325") 5 #\*)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "8871325") 5 #\*)))

     (test-assert
      (result=? "" 
                (textual-pad (string->text "") 0 #\* 0)))

     (test-assert
      (result=? "*****" 
                (textual-pad (string->text "") 5 #\* 0)))

     (test-assert
      (result=? "**325" 
                (textual-pad (string->text "325") 5 #\* 0)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "71325") 5 #\* 0)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "8871325") 5 #\* 0)))

     (test-assert
      (result=? "***25" 
                (textual-pad (string->text "325") 5 #\* 1)))

     (test-assert
      (result=? "*1325" 
                (textual-pad (string->text "71325") 5 #\* 1)))

     (test-assert
      (result=? "71325" 
                (textual-pad (string->text "8871325") 5 #\* 1)))

     (test-assert
      (result=? "" 
                (textual-pad (string->text "") 0 #\* 0 0)))

     (test-assert
      (result=? "*****" 
                (textual-pad (string->text "") 5 #\* 0 0)))

     (test-assert
      (result=? "**325" 
                (textual-pad (string->text "325") 5 #\* 0 3)))

     (test-assert
      (result=? "**713" 
                (textual-pad (string->text "71325") 5 #\* 0 3)))

     (test-assert
      (result=? "**887" 
                (textual-pad (string->text "8871325") 5 #\* 0 3)))

     (test-assert
      (result=? "***25" 
                (textual-pad (string->text "325") 5 #\* 1 3)))

     (test-assert
      (result=? "**132" 
                (textual-pad (string->text "71325") 5 #\* 1 4)))

     (test-assert
      (result=? "*8713" 
                (textual-pad (string->text "8871325") 5 #\* 1 5)))

     (test-assert
      (result=? "" 
                (textual-pad-right (string->text "") 0)))

     (test-assert
      (result=? "     " 
                (textual-pad-right (string->text "") 5)))

     (test-assert
      (result=? "325  " 
                (textual-pad-right (string->text "325") 5)))

     (test-assert
      (result=? "71325" 
                (textual-pad-right (string->text "71325") 5)))

     (test-assert
      (result=? "88713" 
                (textual-pad-right (string->text "8871325") 5)))

     (test-assert
      (result=? "" 
                (textual-pad-right (string->text "") 0 #\*)))

     (test-assert
      (result=? "*****" 
                (textual-pad-right (string->text "") 5 #\*)))

     (test-assert
      (result=? "325**" 
                (textual-pad-right (string->text "325") 5 #\*)))

     (test-assert
      (result=? "71325" 
                (textual-pad-right (string->text "71325") 5 #\*)))

     (test-assert
      (result=? "88713" 
                (textual-pad-right (string->text "8871325") 5 #\*)))

     (test-assert
      (result=? "" 
                (textual-pad-right (string->text "") 0 #\* 0)))

     (test-assert
      (result=? "*****" 
                (textual-pad-right (string->text "") 5 #\* 0)))

     (test-assert
      (result=? "325**" 
                (textual-pad-right (string->text "325") 5 #\* 0)))

     (test-assert
      (result=? "71325" 
                (textual-pad-right (string->text "71325") 5 #\* 0)))

     (test-assert
      (result=? "88713" 
                (textual-pad-right (string->text "8871325") 5 #\* 0)))

     (test-assert
      (result=? "25***" 
                (textual-pad-right (string->text "325") 5 #\* 1)))

     (test-assert
      (result=? "1325*" 
                (textual-pad-right (string->text "71325") 5 #\* 1)))

     (test-assert
      (result=? "87132" 
                (textual-pad-right (string->text "8871325") 5 #\* 1)))

     (test-assert
      (result=? "" 
                (textual-pad-right (string->text "") 0 #\* 0 0)))

     (test-assert
      (result=? "*****" 
                (textual-pad-right (string->text "") 5 #\* 0 0)))

     (test-assert
      (result=? "325**" 
                (textual-pad-right (string->text "325") 5 #\* 0 3)))

     (test-assert
      (result=? "713**" 
                (textual-pad-right (string->text "71325") 5 #\* 0 3)))

     (test-assert
      (result=? "887**" 
                
                (textual-pad-right (string->text "8871325") 5 #\* 0 3)))

     (test-assert
      (result=? "25***" 
                (textual-pad-right (string->text "325") 5 #\* 1 3)))

     (test-assert
      (result=? "132**" 
                (textual-pad-right (string->text "71325") 5 #\* 1 4)))

     (test-assert
      (result=? "8713*" 
                (textual-pad-right (string->text "8871325") 5 #\* 1 5)))


     (test-assert (result=? "" (textual-pad "" 0)))

     (test-assert (result=? "     " (textual-pad "" 5)))

     (test-assert (result=? "  325" (textual-pad "325" 5)))

     (test-assert (result=? "71325" (textual-pad "71325" 5)))

     (test-assert (result=? "71325" (textual-pad "8871325" 5)))

     (test-assert (result=? "" (textual-pad "" 0 #\*)))

     (test-assert (result=? "*****" (textual-pad "" 5 #\*)))

     (test-assert (result=? "**325" (textual-pad "325" 5 #\*)))

     (test-assert (result=? "71325" (textual-pad "71325" 5 #\*)))

     (test-assert (result=? "71325" (textual-pad "8871325" 5 #\*)))

     (test-assert (result=? "" (textual-pad "" 0 #\* 0)))

     (test-assert (result=? "*****" (textual-pad "" 5 #\* 0)))

     (test-assert (result=? "**325" (textual-pad "325" 5 #\* 0)))

     (test-assert (result=? "71325" (textual-pad "71325" 5 #\* 0)))

     (test-assert (result=? "71325" (textual-pad "8871325" 5 #\* 0)))

     (test-assert (result=? "***25" (textual-pad "325" 5 #\* 1)))

     (test-assert (result=? "*1325" (textual-pad "71325" 5 #\* 1)))

     (test-assert (result=? "71325" (textual-pad "8871325" 5 #\* 1)))

     (test-assert (result=? "" (textual-pad "" 0 #\* 0 0)))

     (test-assert (result=? "*****" (textual-pad "" 5 #\* 0 0)))

     (test-assert (result=? "**325" (textual-pad "325" 5 #\* 0 3)))

     (test-assert (result=? "**713" (textual-pad "71325" 5 #\* 0 3)))

     (test-assert (result=? "**887" (textual-pad "8871325" 5 #\* 0 3)))

     (test-assert (result=? "***25" (textual-pad "325" 5 #\* 1 3)))

     (test-assert (result=? "**132" (textual-pad "71325" 5 #\* 1 4)))

     (test-assert (result=? "*8713" (textual-pad "8871325" 5 #\* 1 5)))

     (test-assert (result=? "" (textual-pad-right "" 0)))

     (test-assert (result=? "     " (textual-pad-right "" 5)))

     (test-assert (result=? "325  " (textual-pad-right "325" 5)))

     (test-assert (result=? "71325" (textual-pad-right "71325" 5)))

     (test-assert (result=? "88713" (textual-pad-right "8871325" 5)))

     (test-assert (result=? "" (textual-pad-right "" 0 #\*)))

     (test-assert (result=? "*****" (textual-pad-right "" 5 #\*)))

     (test-assert (result=? "325**" (textual-pad-right "325" 5 #\*)))

     (test-assert (result=? "71325" (textual-pad-right "71325" 5 #\*)))

     (test-assert (result=? "88713" (textual-pad-right "8871325" 5 #\*)))

     (test-assert (result=? "" (textual-pad-right "" 0 #\* 0)))

     (test-assert (result=? "*****" (textual-pad-right "" 5 #\* 0)))

     (test-assert (result=? "325**" (textual-pad-right "325" 5 #\* 0)))

     (test-assert (result=? "71325" (textual-pad-right "71325" 5 #\* 0)))

     (test-assert (result=? "88713" (textual-pad-right "8871325" 5 #\* 0)))

     (test-assert (result=? "25***" (textual-pad-right "325" 5 #\* 1)))

     (test-assert (result=? "1325*" (textual-pad-right "71325" 5 #\* 1)))

     (test-assert (result=? "87132" (textual-pad-right "8871325" 5 #\* 1)))

     (test-assert (result=? "" (textual-pad-right "" 0 #\* 0 0)))

     (test-assert (result=? "*****" (textual-pad-right "" 5 #\* 0 0)))

     (test-assert (result=? "325**" (textual-pad-right "325" 5 #\* 0 3)))

     (test-assert (result=? "713**" (textual-pad-right "71325" 5 #\* 0 3)))

     (test-assert (result=? "887**" (textual-pad-right "8871325" 5 #\* 0 3)))

     (test-assert (result=? "25***" (textual-pad-right "325" 5 #\* 1 3)))

     (test-assert (result=? "132**" (textual-pad-right "71325" 5 #\* 1 4)))

     (test-assert (result=? "8713*" (textual-pad-right "8871325" 5 #\* 1 5)))


     (test-assert
      (result=? ""
                (textual-trim (string->text ""))))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim (string->text "  a  b  c  "))))

     (test-assert
      (result=? ""
                (textual-trim (string->text "")
                              char-whitespace?)))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim (string->text "  a  b  c  ")
                              char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char?)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "")
                              char-whitespace? 0)))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim (string->text "  a  b  c  ")
                              char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 0)))

     (test-assert
      (result=? "b  c  "
                (textual-trim (string->text "  a  b  c  ")
                              char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 3)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 0 11)))

     (test-assert
      (result=? "b  c  "
                (textual-trim (string->text "  a  b  c  ")
                              char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 0 8)))

     (test-assert
      (result=? "b  "
                (textual-trim (string->text "  a  b  c  ")
                              char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim (string->text "  a  b  c  ") char? 3 8)))


     (test-assert
      (result=? ""
                (textual-trim-right (string->text ""))))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right (string->text "  a  b  c  "))))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "") char-whitespace?)))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right (string->text "  a  b  c  ")
                                    char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char?)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "") char-whitespace? 0)))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right (string->text "  a  b  c  ")
                                    char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 0)))

     (test-assert
      (result=? "  b  c"
                (textual-trim-right (string->text "  a  b  c  ")
                                    char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 3)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 0 11)))

     (test-assert
      (result=? "  b  c"
                (textual-trim-right (string->text "  a  b  c  ")
                                    char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 0 8)))

     (test-assert
      (result=? "  b"
                (textual-trim-right (string->text "  a  b  c  ")
                                    char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim-right (string->text "  a  b  c  ") char? 3 8)))


     (test-assert
      (result=? ""
                (textual-trim-both (string->text ""))))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both (string->text "  a  b  c  "))))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "") char-whitespace?)))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both (string->text "  a  b  c  ")
                                   char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char?)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "") char-whitespace? 0)))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both (string->text "  a  b  c  ")
                                   char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 0)))

     (test-assert
      (result=? "b  c"
                (textual-trim-both (string->text "  a  b  c  ")
                                   char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 3)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 0 11)))

     (test-assert
      (result=? "b  c"
                (textual-trim-both (string->text "  a  b  c  ")
                                   char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 0 8)))

     (test-assert
      (result=? "b"
                (textual-trim-both (string->text "  a  b  c  ")
                                   char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim-both (string->text "  a  b  c  ") char? 3 8)))


     (test-assert
      (result=? ""
                (textual-trim "")))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim "  a  b  c  ")))

     (test-assert
      (result=? ""
                (textual-trim "" char-whitespace?)))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim "  a  b  c  " char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char?)))

     (test-assert
      (result=? ""
                (textual-trim "" char-whitespace? 0)))

     (test-assert
      (result=? "a  b  c  "
                (textual-trim "  a  b  c  " char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 0)))

     (test-assert
      (result=? "b  c  "
                (textual-trim "  a  b  c  " char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 3)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 0 11)))

     (test-assert
      (result=? "b  c  "
                (textual-trim "  a  b  c  " char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 0 8)))

     (test-assert
      (result=? "b  "
                (textual-trim "  a  b  c  " char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim "  a  b  c  " char? 3 8)))


     (test-assert
      (result=? ""
                (textual-trim-right "")))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right "  a  b  c  ")))

     (test-assert
      (result=? ""
                (textual-trim-right "" char-whitespace?)))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right "  a  b  c  " char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char?)))

     (test-assert
      (result=? ""
                (textual-trim-right "" char-whitespace? 0)))

     (test-assert
      (result=? "  a  b  c"
                (textual-trim-right "  a  b  c  " char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 0)))

     (test-assert
      (result=? "  b  c"
                (textual-trim-right "  a  b  c  " char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 3)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 0 11)))

     (test-assert
      (result=? "  b  c"
                (textual-trim-right "  a  b  c  " char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 0 8)))

     (test-assert
      (result=? "  b"
                (textual-trim-right "  a  b  c  " char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim-right "  a  b  c  " char? 3 8)))


     (test-assert
      (result=? ""
                (textual-trim-both "")))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both "  a  b  c  ")))

     (test-assert
      (result=? ""
                (textual-trim-both "" char-whitespace?)))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both "  a  b  c  " char-whitespace?)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char?)))

     (test-assert
      (result=? ""
                (textual-trim-both "" char-whitespace? 0)))

     (test-assert
      (result=? "a  b  c"
                (textual-trim-both "  a  b  c  " char-whitespace? 0)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 0)))

     (test-assert
      (result=? "b  c"
                (textual-trim-both "  a  b  c  " char-whitespace? 3)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 3)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 0 11)))

     (test-assert
      (result=? "b  c"
                (textual-trim-both "  a  b  c  " char-whitespace? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 3 11)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 0 8)))

     (test-assert
      (result=? "b"
                (textual-trim-both "  a  b  c  " char-whitespace? 3 8)))

     (test-assert
      (result=? ""
                (textual-trim-both "  a  b  c  " char? 3 8)))


     ;; Replacement

     (test-assert
      (result=? "It's lots of fun to code it up in Scheme."
                (textual-replace (as-text "It's easy to code it up in Scheme.")
                                 (as-text "lots of fun")
                                 5 9)))

     (test-assert
      (result=? "The miserable perl programmer endured daily ridicule."
                (textual-replace "The TCL programmer endured daily ridicule."
                                 (as-text "another miserable perl drone")
                                 4 7 8 22)))

     (test-assert
      (result=? "It's really easy to code it up in Scheme."
                (textual-replace (as-text "It's easy to code it up in Scheme.")
                                 "really "
                                 5 5)))

     (test-assert
      (result=? "Runs in O(1) time." ; for texts (using sample implementations)
                (textual-replace "Runs in O(n) time." (text #\1) 10 11)))

     ;; Comparison
     ;;
     ;; The comparison tests aren't perfectly black-box because the
     ;; specification of these comparison procedures allows them to
     ;; use an ordering other than the usual lexicographic ordering.
     ;; The sample implementations use lexicographic ordering, however,
     ;; and a test program that discourages implementations from using
     ;; orderings that differ from the usual on such simple cases is
     ;; probably doing a public service.

     (test #t (textual=? (as-text "Strasse") (as-text "Strasse")))

     (test #t (textual=? "Strasse" (as-text "Strasse") "Strasse"))

     (test #f (textual<? (as-text "z") (as-text "z")))
     (test #t (textual<? (as-text "z") "zz"))
     (test #f (textual<? (as-text "z") (as-text "Z")))
     (test #t (textual<=? (as-text "z") "zz"))
     (test #f (textual<=? "z" "Z"))
     (test #t (textual<=? "z" (as-text "z")))

     (test #f (textual<? "z" (as-text "z")))
     (test #f (textual>? (as-text "z") "zz"))
     (test #t (textual>? "z" (as-text "Z")))
     (test #f (textual>=? (as-text "z") "zz"))
     (test #t (textual>=? "z" "Z"))
     (test #t (textual>=? (as-text "z") (as-text "z")))


     (let* ((w "a")
            (x "abc")
            (y "def")
            (z (text #\a #\b #\c)))

       (test (textual=? x y z)                           #f)
       (test (textual=? x x z)                           #t)
       (test (textual=? w x y)                           #f)
       (test (textual=? y x w)                           #f)

       (test (textual<? x y z)                           #f)
       (test (textual<? x x z)                           #f)
       (test (textual<? w x y)                           #t)
       (test (textual<? y x w)                           #f)

       (test (textual>? x y z)                           #f)
       (test (textual>? x x z)                           #f)
       (test (textual>? w x y)                           #f)
       (test (textual>? y x w)                           #t)

       (test (textual<=? x y z)                          #f)
       (test (textual<=? x x z)                          #t)
       (test (textual<=? w x y)                          #t)
       (test (textual<=? y x w)                          #f)

       (test (textual>=? x y z)                          #f)
       (test (textual>=? x x z)                          #t)
       (test (textual>=? w x y)                          #f)
       (test (textual>=? y x w)                          #t)


       (test (textual=? x x)                             #t)
       (test (textual=? w x)                             #f)
       (test (textual=? y x)                             #f)

       (test (textual<? x x)                             #f)
       (test (textual<? w x)                             #t)
       (test (textual<? y x)                             #f)

       (test (textual>? x x)                             #f)
       (test (textual>? w x)                             #f)
       (test (textual>? y x)                             #t)

       (test (textual<=? x x)                            #t)
       (test (textual<=? w x)                            #t)
       (test (textual<=? y x)                            #f)

       (test (textual>=? x x)                            #t)
       (test (textual>=? w x)                            #f)
       (test (textual>=? y x)                            #t))


     (test #t (textual-ci<? "a" "Z"))
     (test #t (textual-ci<? "A" "z"))
     (test #f (textual-ci<? "Z" "a"))
     (test #f (textual-ci<? "z" "A"))
     (test #f (textual-ci<? "z" "Z"))
     (test #f (textual-ci<? "Z" "z"))
     (test #f (textual-ci>? "a" "Z"))
     (test #f (textual-ci>? "A" "z"))
     (test #t (textual-ci>? "Z" "a"))
     (test #t (textual-ci>? "z" "A"))
     (test #f (textual-ci>? "z" "Z"))
     (test #f (textual-ci>? "Z" "z"))
     (test #t (textual-ci=? "z" "Z"))
     (test #f (textual-ci=? "z" "a"))
     (test #t (textual-ci<=? "a" "Z"))
     (test #t (textual-ci<=? "A" "z"))
     (test #f (textual-ci<=? "Z" "a"))
     (test #f (textual-ci<=? "z" "A"))
     (test #t (textual-ci<=? "z" "Z"))
     (test #t (textual-ci<=? "Z" "z"))
     (test #f (textual-ci>=? "a" "Z"))
     (test #f (textual-ci>=? "A" "z"))
     (test #t (textual-ci>=? "Z" "a"))
     (test #t (textual-ci>=? "z" "A"))
     (test #t (textual-ci>=? "z" "Z"))
     (test #t (textual-ci>=? "Z" "z"))

     ;; The full-unicode feature doesn't imply full Unicode in strings,
     ;; so these tests might fail even in a conforming implementation.
     ;; Implementations that support full Unicode strings often have
     ;; this feature, however, even though it isn't listed in the R7RS.

     (cond-expand
      (full-unicode-strings
       (test #f (textual=? ABCDEF DEFABC))
       (test #f (textual=? DEFABC ABCDEF))
       (test #t (textual=? DEFABC DEFABC))

       (test #f (textual<? ABCDEF DEFABC))
       (test #t (textual<? DEFABC ABCDEF))
       (test #f (textual<? DEFABC DEFABC))

       (test #t (textual>? ABCDEF DEFABC))
       (test #f (textual>? DEFABC ABCDEF))
       (test #f (textual>? DEFABC DEFABC))

       (test #f (textual<=? ABCDEF DEFABC))
       (test #t (textual<=? DEFABC ABCDEF))
       (test #t (textual<=? DEFABC DEFABC))

       (test #t (textual>=? ABCDEF DEFABC))
       (test #f (textual>=? DEFABC ABCDEF))
       (test #t (textual>=? DEFABC DEFABC))

       (test #f (textual=? "Fuss" fuss))
       (test #f (textual=? "Fuss" "Fuss" fuss))
       (test #f (textual=? "Fuss" fuss "Fuss"))
       (test #f (textual=? fuss "Fuss" "Fuss"))
       (test #t (textual<? "z" (as-text eszett)))
       (test #f (textual<? (as-text eszett) "z"))
       (test #t (textual<=? "z" (as-text eszett)))
       (test #f (textual<=? (as-text eszett) "z"))
       (test #f (textual>? "z" (as-text eszett)))
       (test #t (textual>? (as-text eszett) "z"))
       (test #f (textual>=? "z" (as-text eszett)))
       (test #t (textual>=? (as-text eszett) "z"))
       (test-assert (textual-ci=? fuss "Fuss"))
       (test-assert (textual-ci=? fuss "FUSS"))
       (test-assert (textual-ci=? chaos0 chaos1 chaos2)))
      (else))


     ;; Prefixes and suffixes

     (test/= 0 (textual-prefix-length ABC ABCDEF))

     (test/= 0 (textual-prefix-length ABCDEF ABC))

     (test/= 0 (textual-prefix-length ABCDEF DEFABC))

     (test/= 6 (textual-prefix-length DEFABC DEFABC))

     (test/= 6 (textual-prefix-length (textual->string DEFABC) DEFABC))

     (test/= 6 (textual-prefix-length DEFABC (textual->string DEFABC)))

     (test/= 6 (textual-prefix-length (textual->string DEFABC)
                                      (textual->string DEFABC)))

     (test/= 0 (textual-prefix-length (as-text "") (as-text "")))

     (test/= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))

     (test/= 0 (textual-prefix-length (as-text "aisle") (as-text "")))

     (test/= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))

     (test/= 1 (textual-prefix-length (as-text "aisle")
                                      (as-text "aabbccddee")))

     (test/= 0 (textual-prefix-length (as-text "bail")
                                      (as-text "aabbccddee")))

     (test/= 4 (textual-prefix-length (as-text "prefix")
                                      (as-text "preface")))

     (test/= 0 (textual-prefix-length (as-text "")
                                      (as-text "")
                                      0))

     (test/= 0 (textual-prefix-length (as-text "")
                                      (as-text "aabbccddee")
                                      0))

     (test/= 0 (textual-prefix-length (as-text "aisle")
                                      (as-text "")
                                      0))

     (test/= 1 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 0))

     (test/= 0 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 0))

     (test/= 4 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 1))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 1))

     (test/= 1 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 1))

     (test/= 0 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 1))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "") 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "aabbccddee") 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 0 4))

     (test/= 1 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 0 4))

     (test/= 0 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 0 1))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 1 4))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4))

     (test/= 1 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 1 4))

     (test/= 0 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 1 5))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "") 0 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "aabbccddee") 0 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 0 4 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 0 4 2))

     (test/= 1 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 0 1 2))

     (test/= 0 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 0 5 1))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 1 4 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4 3))

     (test/= 0 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 1 4 3))

     (test/= 3 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 1 5 1))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "") 0 0 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "") (as-text "aabbccddee") 0 0 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 0 4 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") "aabbccddee" 0 4 2 10))

     (test/= 1 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 0 1 2 10))

     (test/= 0 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 0 5 1 6))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "") 1 4 0 0))

     (test/= 0 (textual-prefix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4 3 3))

     (test/= 0 (textual-prefix-length
                (as-text "bail") (as-text "aabbccddee") 1 4 3 6))

     (test/= 3 (textual-prefix-length
                (as-text "prefix") (as-text "preface") 1 5 1 7))


     (test/= 0 (textual-suffix-length ABC ABCDEF))

     (test/= 0 (textual-suffix-length ABCDEF ABC))

     (test/= 0 (textual-suffix-length ABCDEF DEFABC))

     (test/= 6 (textual-suffix-length DEFABC DEFABC))

     (test/= 6 (textual-suffix-length (textual->string DEFABC) DEFABC))

     (test/= 6 (textual-suffix-length DEFABC (textual->string DEFABC)))

     (test/= 6 (textual-suffix-length
                (textual->string DEFABC) (textual->string DEFABC)))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "")))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee")))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "")))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee")))

     (test/= 1 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee")))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee")))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface")))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "") 0))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee") 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 0))

     (test/= 1 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 0))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 0))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 1))

     (test/= 1 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 1))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 1))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 1))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "") 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee") 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 0 4))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 0 4))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 0 1))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 1 4))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4))

     (test/= 1 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 1 5))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 1 4))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 1 5))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "") 0 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee") 0 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 0 4 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 0 4 2))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 0 1 2))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 0 5 1))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 1 4 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4 3))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 1 4 3))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 1 5 1))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "") 0 0 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "") (as-text "aabbccddee") 0 0 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 0 4 0 0))

     (test/= 1 (textual-suffix-length
                "aisle" (as-text "aabbccddee") 0 5 2 10))

     (test/= 1 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 0 1 2 4))

     (test/= 0 (textual-suffix-length
                (as-text "place") (as-text "preface") 0 5 1 6))

     (test/= 2 (textual-suffix-length
                (as-text "place") (as-text "preface") 0 4 1 6))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "") 1 4 0 0))

     (test/= 0 (textual-suffix-length
                (as-text "aisle") (as-text "aabbccddee") 1 4 3 3))

     (test/= 0 (textual-suffix-length
                (as-text "bail") (as-text "aabbccddee") 1 4 3 6))

     (test/= 3 (textual-suffix-length
                (as-text "place") (as-text "preface") 1 5 1 7))


     (test-deny   (textual-prefix? ABC ABCDEF))

     (test-deny   (textual-prefix? ABCDEF ABC))

     (test-deny   (textual-prefix? ABCDEF DEFABC))

     (test-assert (textual-prefix? DEFABC DEFABC))

     (test-assert (textual-prefix? (textual->string DEFABC) DEFABC))

     (test-assert (textual-prefix? DEFABC (textual->string DEFABC)))

     (test-assert (textual-prefix? (textual->string DEFABC)
                                   (textual->string DEFABC)))

     (test-assert (textual-prefix? (as-text "") (as-text "")))

     (test-assert (textual-prefix? (as-text "") (as-text "abc")))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc")))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc")))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc")))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc")))

     (test-assert (textual-prefix? (as-text "abc") (as-text "abc")))

     (test-deny   (textual-suffix? ABC ABCDEF))

     (test-deny   (textual-suffix? ABCDEF ABC))

     (test-deny   (textual-suffix? ABCDEF DEFABC))

     (test-assert (textual-suffix? DEFABC DEFABC))

     (test-assert (textual-suffix? (textual->string DEFABC) DEFABC))

     (test-assert (textual-suffix? DEFABC (textual->string DEFABC)))

     (test-assert (textual-suffix? (as-text "") (as-text "")))

     (test-assert (textual-suffix? (as-text "") (as-text "abc")))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc")))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc")))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc")))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc")))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc")))

     (test-assert (textual-prefix? (as-text "") (as-text "") 0))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc") 0))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0))

     (test-assert (textual-prefix? (as-text "abc") (as-text "abc") 0))

     (test-assert (textual-suffix? (as-text "") (as-text "") 0))

     (test-assert (textual-suffix? (as-text "") (as-text "abc") 0))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc") 0))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 0))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 2))

     (test-assert (textual-prefix? (as-text "ac") (as-text "abc") 2))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 2))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 2))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 2))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 2))


     (test-assert (textual-prefix? (as-text "") (as-text "") 0 0))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc") 0 1))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 1))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 2))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0 2))

     (test-assert (textual-prefix? (as-text "abc") (as-text "abc") 0 3))

     (test-assert (textual-suffix? (as-text "") (as-text "") 0 0))

     (test-assert (textual-suffix? (as-text "") (as-text "abc") 0 0))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0 1))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0 1))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 1 2))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc") 0 2))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0 2))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 0 3))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 2 2))

     (test-assert (textual-prefix? (as-text "ac") (as-text "abc") 2 2))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 2 3))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 2 2))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 2 2))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 2 3))


     (test-assert (textual-prefix? (as-text "") (as-text "") 0 0 0))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0 0))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0 0))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc") 0 1 0))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0))

     (test-assert (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0))

     (test-assert (textual-suffix? (as-text "") (as-text "") 0 0 0))

     (test-assert (textual-suffix? (as-text "") (as-text "abc") 0 0 0))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0 1 0))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0 1 0))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0))

     (test-assert (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0 1))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0 1))

     (test-assert (textual-prefix? (as-text "c") (as-text "abc") 0 1 2))

     (test-deny   (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2))

     (test-deny   (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0 1 2))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0 1 1))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1))

     (test-deny   (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2))


     (test-assert (textual-prefix? (as-text "") (as-text "") 0 0 0 0))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0 0 3))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 3))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 3))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 3))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0 3))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0 3))

     (test-assert (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 3))

     (test-assert (textual-suffix? (as-text "") (as-text "abc") 0 0 0 3))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0 1 0 3))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 3))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 3))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0 3))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0 3))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0 3))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0 3))

     (test-assert (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0 3))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0 3))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0 3))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0 3))

     (test-assert (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0 3))

     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0 1 3))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0 1 3))

     (test-assert (textual-prefix? (as-text "c") (as-text "abc") 0 1 2 3))

     (test-deny   (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2 3))

     (test-deny   (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1 3))

     (test-deny   (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1 3))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1 3))

     (test-deny   (textual-suffix? (as-text "a") (as-text "abc") 0 1 2 3))

     (test-assert (textual-suffix? (as-text "c") (as-text "abc") 0 1 1 3))

     (test-assert (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2 3))

     (test-assert (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1 3))

     (test-deny   (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2 3))


     (test-assert (textual-prefix? (as-text "") (as-text "abc") 0 0 0 2))

     (test-assert (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 2))

     (test-deny   (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 2))

     (test-assert (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 2))

     (test-deny   (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 2))

     (test-assert (textual-suffix? (as-text "") (as-text "abc") 0 0 0 2))

     (test-deny   (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 2))

     (test-deny   (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 2))


;;; Searching

     (test-assert
      (eqv? #f (textual-index (as-text "") char?)))

     (test-assert
      (eqv? 0 (textual-index (as-text "abcdef") char?)))

     (test-assert
      (eqv? 4 (textual-index (as-text "abcdef") (lambda (c) (char>? c #\d)))))

     (test-assert
      (eqv? #f (textual-index (as-text "abcdef") char-whitespace?)))

     (test-assert
      (eqv? #f (textual-index-right (as-text "") char?)))

     (test-assert
      (eqv? 5 (textual-index-right (as-text "abcdef") char?)))

     (test-assert
      (eqv? 5 (textual-index-right (as-text "abcdef")
                                   (lambda (c) (char>? c #\d)))))


     (test-assert
      (eqv? #f (textual-index-right (as-text "abcdef") char-whitespace?)))

     (test-assert
      (eqv? #f (textual-skip (as-text "") string?)))

     (test-assert
      (eqv? 0 (textual-skip (as-text "abcdef") string?)))

     (test-assert
      (eqv? 4 (textual-skip (as-text "abcdef") (lambda (c) (char<=? c #\d)))))

     (test-assert
      (eqv? #f (textual-skip (as-text "abcdef") char?)))

     (test-assert
      (eqv? #f (textual-skip-right (as-text "") string?)))

     (test-assert
      (eqv? 5 (textual-skip-right (as-text "abcdef") string?)))

     (test-assert
      (eqv? 5 (textual-skip-right (as-text "abcdef")
                                  (lambda (c) (char<=? c #\d)))))

     (test-assert
      (eqv? #f (textual-skip-right (as-text "abcdef") char?)))


     (test-assert
      (eqv? 2 (textual-index "abcdef" char? 2)))

     (test-assert
      (eqv? 4 (textual-index "abcdef" (lambda (c) (char>? c #\d)) 2)))

     (test-assert
      (eqv? #f (textual-index "abcdef" char-whitespace? 2)))

     (test-assert
      (eqv? 5 (textual-index-right "abcdef" char? 2)))

     (test-assert
      (eqv? 5 (textual-index-right "abcdef"
                                   (lambda (c)
                                     (char>? c #\d)) 2)))

     (test-assert
      (eqv? #f (textual-index-right "abcdef" char-whitespace? 2)))

     (test-assert
      (eqv? 2 (textual-skip "abcdef" string? 2)))

     (test-assert
      (eqv? 4 (textual-skip "abcdef"
                            (lambda (c)
                              (char<=? c #\d)) 2)))

     (test-assert
      (eqv? #f (textual-skip "abcdef" char? 2)))

     (test-assert
      (eqv? 5 (textual-skip-right "abcdef" string? 2)))

     (test-assert
      (eqv? 5 (textual-skip-right "abcdef"
                                  (lambda (c)
                                    (char<=? c #\d)) 2)))

     (test-assert
      (eqv? #f (textual-skip-right "abcdef" char? 2)))


     (test-assert
      (eqv? 2 (textual-index (as-text "abcdef") char? 2 5)))

     (test-assert
      (eqv? 4 (textual-index (as-text "abcdef")
                             (lambda (c) (char>? c #\d)) 2 5)))

     (test-assert
      (eqv? #f (textual-index (as-text "abcdef") char-whitespace? 2 5)))

     (test-assert
      (eqv? 4 (textual-index-right (as-text "abcdef") char? 2 5)))

     (test-assert
      (eqv? 4 (textual-index-right (as-text "abcdef")
                                   (lambda (c)
                                     (char>? c #\d)) 2 5)))

     (test-assert
      (eqv? #f (textual-index-right (as-text "abcdef")
                                    char-whitespace? 2 5)))


     (test-assert
      (eqv? 2 (textual-skip (as-text "abcdef") string? 2 5)))

     (test-assert
      (eqv? 4 (textual-skip (as-text "abcdef")
                            (lambda (c) (char<=? c #\d)) 2 5)))

     (test-assert
      (eqv? #f (textual-skip (as-text "abcdef") char? 2 5)))

     (test-assert
      (eqv? 4 (textual-skip-right (as-text "abcdef") string? 2 5)))

     (test-assert
      (eqv? 4 (textual-skip-right (as-text "abcdef")
                                  (lambda (c)
                                    (char<=? c #\d)) 2 5)))

     (test-assert
      (eqv? #f (textual-skip-right (as-text "abcdef") char? 2 5)))


     (test-assert
      (eqv? 0 (textual-contains (as-text "") (as-text ""))))

     (test-assert
      (eqv? 0 (textual-contains (as-text "abcdeffffoo") (as-text ""))))

     (test-assert
      (eqv? 0 (textual-contains (as-text "abcdeffffoo") (as-text "a"))))

     (test-assert
      (eqv? 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff"))))

     (test-assert
      (eqv? 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff"))))

     (test-assert
      (eqv? 8 (textual-contains (as-text "abcdeffffoo") (as-text "foo"))))

     (test-assert
      (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "efffoo"))))

     (test-assert
      (eqv? 0 (textual-contains-right (as-text "") (as-text ""))))

     (test-assert
      (eqv? 11 (textual-contains-right (as-text "abcdeffffoo") (as-text ""))))

     (test-assert
      (eqv? 0 (textual-contains-right (as-text "abcdeffffoo") (as-text "a"))))

     (test-assert
      (eqv? 7 (textual-contains-right (as-text "abcdeffffoo") (as-text "ff"))))

     (test-assert
      (eqv? 4 (textual-contains-right (as-text "abcdeffffoo") (as-text "eff"))))

     (test-assert
      (eqv? 8 (textual-contains-right (as-text "abcdeffffoo") (as-text "foo"))))

     (test-assert
      (eqv? #f (textual-contains-right (as-text "abcdeffffoo")
                                       (as-text "efffoo"))))


     (test-assert
      (eqv? 0 (textual-contains "" "" 0)))

     (test-assert
      (eqv? 2 (textual-contains "abcdeffffoo" "" 2)))

     (test-assert
      (eqv? #f (textual-contains "abcdeffffoo" "a" 2)))

     (test-assert
      (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2)))

     (test-assert
      (eqv? 4 (textual-contains "abcdeffffoo" "eff" 2)))

     (test-assert
      (eqv? 8 (textual-contains "abcdeffffoo" "foo" 2)))

     (test-assert
      (eqv? #f (textual-contains "abcdeffffoo" "efffoo" 2)))

     (test-assert
      (eqv? 0 (textual-contains-right "" "" 0)))

     (test-assert
      (eqv? 11 (textual-contains-right "abcdeffffoo" "" 2)))

     (test-assert
      (eqv? #f (textual-contains-right "abcdeffffoo" "a" 2)))

     (test-assert
      (eqv? 7 (textual-contains-right "abcdeffffoo" "ff" 2)))

     (test-assert
      (eqv? 4 (textual-contains-right "abcdeffffoo" "eff" 2)))

     (test-assert
      (eqv? 8 (textual-contains-right "abcdeffffoo" "foo" 2)))

     (test-assert
      (eqv? #f (textual-contains-right "abcdeffffoo" "efffoo" 2)))


     (test-assert
      (eqv? 0 (textual-contains (as-text "") (as-text "") 0 0)))

     (test-assert
      (eqv? 2 (textual-contains (as-text "abcdeffffoo") (as-text "") 2 10)))

     (test-assert
      (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "a") 2 10)))

     (test-assert
      (eqv? 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff") 2 10)))

     (test-assert
      (eqv? 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff") 2 10)))

     (test-assert
      (eqv? #f (textual-contains (as-text "abcdeffffoo")
                                          (as-text "foo")
                                          2 10)))

     (test-assert
      (eqv? #f (textual-contains (as-text "abcdeffffoo")
                                 (as-text "efffoo")
                                 2 10)))

     (test-assert
      (eqv? 0 (textual-contains-right (as-text "") (as-text "") 0 0)))

     (test-assert
      (eqv? 10 (textual-contains-right (as-text "abcdeffffoo")
                                       (as-text "")
                                       2 10)))

     (test-assert
      (eqv? #f (textual-contains-right (as-text "abcdeffffoo")
                                       (as-text "a")
                                       2 10)))

     (test-assert
      (eqv? 7 (textual-contains-right (as-text "abcdeffffoo")
                                      (as-text "ff")
                                      2 10)))

     (test-assert
      (eqv? 4 (textual-contains-right (as-text "abcdeffffoo")
                                      (as-text "eff")
                                      2 10)))

     (test-assert
      (eqv? #f (textual-contains-right (as-text "abcdeffffoo")
                                       "foo" 2 10)))

     (test-assert
      (eqv? #f (textual-contains-right "abcdeffffoo" (as-text "efffoo") 2 10)))


     (test-assert
      (eqv? 0 (textual-contains "" "" 0 0 0)))

     (test-assert
      (eqv? 2 (textual-contains "abcdeffffoo" "" 2 10 0)))

     (test-assert
      (eqv? 2 (textual-contains "abcdeffffoo" "a" 2 10 1)))

     (test-assert
      (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2 10 1)))

     (test-assert
      (eqv? 5 (textual-contains "abcdeffffoo" "eff" 2 10 1)))

     (test-assert
      (eqv? #f (textual-contains "abcdeffffoo" "foo" 2 10 1)))

     (test-assert
      (eqv? #f (textual-contains "abcdeffffoo" "efffoo" 2 10 1)))

     (test-assert
      (eqv? 0 (textual-contains-right "" "" 0 0 0)))

     (test-assert
      (eqv? 10 (textual-contains-right "abcdeffffoo" "" 2 10 0)))

     (test-assert
      (eqv? 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1)))

     (test-assert
      (eqv? 8 (textual-contains-right "abcdeffffoo" "ff" 2 10 1)))

     (test-assert
      (eqv? 7 (textual-contains-right "abcdeffffoo" "eff" 2 10 1)))

     (test-assert
      (eqv? #f (textual-contains-right "abcdeffffoo" "foo" 2 10 1)))

     (test-assert
      (eqv? #f (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1)))


     (test-assert
      (eqv? 0 (textual-contains "" "" 0 0 0 0)))

     (test-assert
      (eqv? 2 (textual-contains "abcdeffffoo" "" 2 10 0 0)))

     (test-assert
      (eqv? 2 (textual-contains "abcdeffffoo" "a" 2 10 1 1)))

     (test-assert
      (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2 10 1 2)))

     (test-assert
      (eqv? 5 (textual-contains "abcdeffffoo" "eff" 2 10 1 2)))

     (test-assert
      (eqv? 9 (textual-contains "abcdeffffoo" "foo" 2 10 1 2)))

     (test-assert
      (eqv? 4 (textual-contains "abcdeffffoo" "efffoo" 2 10 0 2)))

     (test-assert
      (eqv? 0 (textual-contains-right "" "" 0 0 0 0)))

     (test-assert
      (eqv? 10 (textual-contains-right "abcdeffffoo" "" 2 10 0 0)))

     (test-assert
      (eqv? 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1 1)))

     (test-assert
      (eqv? 8  (textual-contains-right "abcdeffffoo" "ff" 2 10 1 2)))

     (test-assert
      (eqv? 8 (textual-contains-right "abcdeffffoo" "eff" 2 10 1 2)))

     (test-assert
      (eqv? 9 (textual-contains-right "abcdeffffoo" "foo" 2 10 1 2)))

     (test-assert
      (eqv? 7 (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1 3)))


     ;; Case conversion

     ;; FIXME: should test some non-ASCII cases here.

     (test-assert
      (result=? "1234STRIKES" (textual-upcase (as-text "1234Strikes"))))

     (test-assert
      (result=? "1234STRIKES" (textual-upcase (as-text "1234strikes"))))

     (test-assert
      (result=? "1234STRIKES" (textual-upcase (as-text "1234STRIKES"))))

     (test-assert
      (result=? "1234strikes" (textual-downcase (as-text "1234Strikes"))))

     (test-assert
      (result=? "1234strikes" (textual-downcase (as-text "1234strikes"))))

     (test-assert
      (result=? "1234strikes" (textual-downcase (as-text "1234STRIKES"))))

     (test-assert
      (result=? "1234strikes" (textual-foldcase (as-text "1234Strikes"))))

     (test-assert
      (result=? "1234strikes" (textual-foldcase (as-text "1234strikes"))))

     (test-assert
      (result=? "1234strikes" (textual-foldcase (as-text "1234STRIKES"))))

     (test-assert
      (result=? "And With Three Strikes You Are Out"
                (textual-titlecase
                 (as-text "and with THREE STRIKES you are oUT"))))

     ;; Concatenation

     (test-assert (result=? "" (textual-append)))

     (test-assert (result=? "abcdef"
                            
                            (textual-append (as-text "")
                                            (as-text "a")
                                            (as-text "bcd")
                                            "" "ef" "" "")))

     (test-assert (result=? "" (textual-concatenate '())))

     (test-assert
      (result=? "abcdef"
                (textual-concatenate
                 (map string->text '("" "a" "bcd" "" "ef" "" "")))))

     ;; textual-concatenate is likely to have special cases for longer texts.

     (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
            (str1 alphabet)
            (str10 (apply string-append
                          (vector->list (make-vector 10 str1))))
            (str100 (apply string-append
                           (vector->list (make-vector 10 str10))))
            (str100-500 (substring str100 100 500))
            (str600-999 (substring str100 600 999))
            (alph1 (textual-copy alphabet))
            (alph10 (textual-concatenate
                     (vector->list (make-vector 10 alph1))))
            (alph100 (textual-concatenate
                      (vector->list (make-vector 10 alph10))))
            (t100-500 (subtext alph100 100 500))
            (t600-999 (subtext alph100 600 999)))

       (test-assert (result=? str10 alph10))

       (test-assert (result=? str100 alph100))

       (test-assert (result=? str100-500 t100-500))

       (test-assert (result=? str600-999 t600-999))

       ;; concatenating a short text with a long text

       (test-assert
        (result=? (string-append str1 str600-999)
                  (textual-concatenate (list alph1 t600-999))))

       (test-assert
        (result=? (string-append str1 str600-999)
                  (textual-concatenate (list alph1 (textual-copy t600-999)))))

       (test-assert
        (result=? (string-append str600-999 str1)
                  (textual-concatenate (list t600-999 alph1))))

       (test-assert
        (result=? (string-append str600-999 str1)
                  (textual-concatenate (list (textual-copy t600-999) alph1)))))


     (test-assert
      (result=? "" (textual-concatenate-reverse '())))

     (test-assert
      (result=? "efbcda"
                (textual-concatenate-reverse
                 (map string->text '("" "a" "bcd" "" "ef" "" "")))))

     (test-assert
      (result=? "huh?"
                (textual-concatenate-reverse '() "huh?")))

     (test-assert
      (result=? "efbcdaxy"
                (textual-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")
                                             "xy")))

     (test-assert
      (result=? "huh"
                (textual-concatenate-reverse '() "huh?" 3)))

     (test-assert
      (result=? "efbcdax"
                (textual-concatenate-reverse
                 '("" "a" "bcd" "" "ef" "" "") "x" 1)))


     (test-assert
      (result=? "" (textual-join '())))

     (test-assert
      (result=? " ab cd  e f "
                (textual-join
                 (map string->text '("" "ab" "cd" "" "e" "f" "")))))

     (test-assert
      (result=? ""
                (textual-join '() "")))

     (test-assert
      (result=? "abcdef"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "")))

     (test-assert
      (result=? ""
                (textual-join '() "xyz")))

     (test-assert
      (result=? "xyzabxyzcdxyzxyzexyzfxyz"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz")))

     (test-assert
      (result=? ""
                (textual-join '() "" 'infix)))

     (test-assert
      (result=? "abcdef"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'infix)))

     (test-assert
      (result=? ""
                (textual-join '() "xyz" 'infix)))

     (test-assert
      (result=? "xyzabxyzcdxyzxyzexyzfxyz"
                (textual-join
                 '("" "ab" "cd" "" "e" "f" "") (as-text "xyz") 'infix)))

     (test 'horror
           (guard (exn (#t 'horror))
                  (textual-join '() "" 'strict-infix)))

     (test-assert
      (result=? "abcdef"
                (textual-join
                 '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix)))

     (test 'wham
           (guard (exn (else 'wham))
                  (textual-join '() "xyz" 'strict-infix)))

     (test-assert
      (result=? "xyzabxyzcdxyzxyzexyzfxyz"
                (textual-join
                 '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix)))

     (test-assert
      (result=? ""
                (textual-join '() "" 'suffix)))

     (test-assert
      (result=? "abcdef"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix)))

     (test-assert
      (result=? ""
                (textual-join '() "xyz" 'suffix)))

     (test-assert
      (result=? "xyzabxyzcdxyzxyzexyzfxyzxyz"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix)))

     (test-assert
      (result=? ""
                (textual-join '() "" 'prefix)))

     (test-assert
      (result=? "abcdef"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix)))

     (test-assert
      (result=? ""
                (textual-join '() "xyz" 'prefix)))

     (test-assert
      (result=? "xyzxyzabxyzcdxyzxyzexyzfxyz"
                (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix)))


     ;; Fold & map & friends

     (test/= 8
             (textual-fold (lambda (c count)
                             (if (char-whitespace? c)
                                 (+ count 1)
                                 count))
                           0
                           (as-text " ...a couple of spaces in this one... ")))

     (test/= 7
             (textual-fold (lambda (c count)
                             (if (char-whitespace? c)
                                 (+ count 1)
                                 count))
                           0
                           " ...a couple of spaces in this one... "
                           1))

     (test/= 6
             (textual-fold (lambda (c count)
                             (if (char-whitespace? c)
                                 (+ count 1)
                                 count))
                           0
                           " ...a couple of spaces in this one... "
                           1
                           32))

     (test (string->list "abcdef")
           (textual-fold-right cons '() "abcdef"))

     (test (string->list "def")
           (textual-fold-right cons '() (as-text "abcdef") 3))

     (test (string->list "cde")
           (textual-fold-right cons '() (as-text "abcdef") 2 5))

     (test-assert
      (string=? "aabraacaadaabraa"
                (let* ((s (as-text "abracadabra"))
                       (ans-len (textual-fold (lambda (c sum)
                                                (+ sum
                                                   (if (char=? c #\a) 2 1)))
                                              0 s))
                       (ans (make-string ans-len)))
                  (textual-fold (lambda (c i)
                                  (let ((i (if (char=? c #\a)
                                               (begin (string-set! ans i #\a)
                                                      (+ i 1))
                                               i)))
                                    (string-set! ans i c)
                                    (+ i 1)))
                                0 s)
                  ans)))


     (test-assert (result=? "abc" (textual-map string (as-text "abc"))))

     (test-assert (result=? "ABC" (textual-map char-upcase "abc")))

     (test-assert (result=? "Hear-here!"
                            (textual-map (lambda (c0 c1 c2)
                                           (case c0
                                             ((#\1) c1)
                                             ((#\2) (string c2))
                                             ((#\-) (text #\- c1))))
                                         (string->text "1222-1111-2222")
                                         (string->text "Hi There!")
                                         (string->text "Dear John"))))

     (test-assert (string=? "abc"
                            (let ((q (open-output-string)))
                              (textual-for-each (lambda (c) (write-char c q))
                                                (as-text "abc"))
                              (get-output-string q))))

     (test '("cfi" "beh" "adg")
           (let ((x '()))
             (textual-for-each (lambda (c1 c2 c3)
                                 (set! x (cons (string c1 c2 c3) x)))
                               "abc"
                               (as-text "defxyz")
                               (as-text "ghijklmnopqrstuvwxyz"))
             x))

     (test-assert
      (result=? "abc"
                (textual-map-index
                 (lambda (i)
                   (integer->char (+ i (char->integer #\a))))
                 "xyz")))

     (test-assert
      (result=? "def"
                (textual-map-index
                 (lambda (i)
                   (integer->char (+ i (char->integer #\a))))
                 "xyz***" 3)))

     (test-assert
      (result=? "cde"
                (textual-map-index
                 (lambda (i)
                   (integer->char (+ i (char->integer #\a))))
                 "......" 2 5)))

     (test '(101 100 99 98 97)
           (let ((s (as-text "abcde"))
                 (v '()))
             (textual-for-each-index
              (lambda (i)
                (set! v (cons (char->integer (textual-ref s i)) v)))
              s)
             v))

     (test '(101 100 99)
           (let ((s (as-text "abcde"))
                 (v '()))
             (textual-for-each-index
              (lambda (i)
                (set! v (cons (char->integer (textual-ref s i)) v)))
              s 2)
             v))

     (test '(99 98)
           (let ((s (as-text "abcde"))
                 (v '()))
             (textual-for-each-index
              (lambda (i)
                (set! v (cons (char->integer (textual-ref s i)) v)))
              s 1 3)
             v))

     (test/= 6 (textual-count "abcdef" char?))

     (test/= 4 (textual-count "counting  whitespace, again "
                              char-whitespace? 5))

     (test/= 3 (textual-count "abcdefwxyz"
                              (lambda (c) (odd? (char->integer c)))
                              2 8))


     (test-assert
      (result=? "aiueaaaoi"
                (textual-filter
                 (lambda (c) (memv c (textual->list "aeiou")))
                 (as-text "What is number, that man may know it?"))))

     (test-assert
      (result=? "And wmn, tht sh my knw nmbr?"
                (textual-remove
                 (lambda (c) (memv c (textual->list "aeiou")))
                 "And woman, that she may know number?")))

     (test-assert
      (result=? "iueaaaoi"
                (textual-filter
                 (lambda (c) (memv c (textual->list "aeiou")))
                 (as-text "What is number, that man may know it?")
                 4)))

     (test-assert
      (result=? "mn, tht sh my knw nmbr?"
                (textual-remove
                 (lambda (c) (memv c (textual->list "aeiou")))
                 "And woman, that she may know number?"
                 6)))

     (test-assert
      (result=? "aaao"
                (textual-filter
                 (lambda (c) (memv c (textual->list "aeiou")))
                 (as-text "What is number, that man may know it?")
                 16 32)))

     (test-assert
      (result=? "And woman, that sh may know"
                (textual-remove
                 (lambda (c) (memv c (textual->list "eiu")))
                 "And woman, that she may know number?"
                 0 28)))


     ;; Replication and splitting

     (test-assert
      (result=? "cdefabcdefabcd"
                (textual-replicate "abcdef" -4 10)))

     (test-assert
      (result=? "bcdefbcdefbcd"
                (textual-replicate "abcdef" 90 103 1)))

     (test-assert
      (result=? "ecdecdecde"
                (textual-replicate "abcdef" -13 -3 2 5)))

     (test '() (map textual->string (textual-split "" "")))

     (test '("a" "b" "c") (map textual->string (textual-split "abc" "")))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " ")))

     (test '("" "there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***")))

     (test '() (map textual->string (textual-split "" "" 'infix)))

     (test '("a" "b" "c")
           (map textual->string (textual-split "abc" "" 'infix)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'infix)))

     (test '("" "there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'infix)))

     (test 'error
           (guard (exn (else 'error))
                  (map textual->string
                       (textual-split "" "" 'strict-infix))))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'strict-infix)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'strict-infix)))

     (test '("" "there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'strict-infix)))

     (test '()
           (map textual->string
                (textual-split "" "" 'prefix)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'prefix)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'prefix)))

     (test '("there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'prefix)))

     (test '()
           (map textual->string
                (textual-split "" "" 'suffix)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'suffix)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'suffix)))

     (test '("" "there" "ya" "go")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'suffix)))


     (test '()
           (map textual->string
                (textual-split "" "" 'infix #f)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'infix #f)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'infix #f)))

     (test '("" "there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'infix #f)))

     (test 'error
           (guard (exn (else 'error))
                  (map textual->string
                       (textual-split "" "" 'strict-infix #f))))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'strict-infix #f)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'strict-infix #f)))

     (test '("" "there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***"
                               "***"
                               'strict-infix #f)))

     (test '()
           (map textual->string
                (textual-split "" "" 'prefix #f)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'prefix #f)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'prefix #f)))

     (test '("there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'prefix #f)))

     (test '()
           (map textual->string
                (textual-split "" "" 'suffix #f)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'suffix #f)))

     (test '("too" "" "much" "" "data")
           (map textual->string
                (textual-split "too  much  data" " " 'suffix #f)))

     (test '("" "there" "ya" "go")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'suffix #f)))


     (test 'error
           (guard (exn (else 'error))
                  (map textual->string
                       (textual-split "" "" 'strict-infix 3))))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'strict-infix 3)))

     (test '("too" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'strict-infix 3)))

     (test '("" "there" "ya" "go***")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'strict-infix 3)))

     (test '()
           (map textual->string
                (textual-split "" "" 'prefix 3)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'prefix 3)))

     (test '("too" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'prefix 3)))

     (test '("there" "ya" "go***")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'prefix 3)))

     (test '()
           (map textual->string
                (textual-split "" "" 'suffix 3)))

     (test '("a" "b" "c")
           (map textual->string
                (textual-split "abc" "" 'suffix 3)))

     (test '("too" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'suffix 3)))

     (test '("" "there" "ya" "go***")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'suffix 3)))


     (test 'error
           (guard (exn (else 'error))
                  (map textual->string
                       (textual-split "" "" 'strict-infix 3 0))))

     (test '("b" "c")
           (map textual->string
                (textual-split "abc" "" 'strict-infix 3 1)))

     (test '("oo" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'strict-infix 3 1)))

     (test '("**there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***"
                               "***"
                               'strict-infix 3 1)))

     (test '()
           (map textual->string
                (textual-split "" "" 'prefix 3 0)))

     (test '("b" "c")
           (map textual->string
                (textual-split "abc" "" 'prefix 3 1)))

     (test '("oo" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'prefix 3 1)))

     (test '("**there" "ya" "go" "")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'prefix 3 1)))

     (test '()
           (map textual->string
                (textual-split "" "" 'suffix 3 0)))

     (test '("b" "c")
           (map textual->string
                (textual-split "abc" "" 'suffix 3 1)))

     (test '("oo" "" "much" " data")
           (map textual->string
                (textual-split "too  much  data" " " 'suffix 3 1)))

     (test '("**there" "ya" "go")
           (map textual->string
                (textual-split "***there***ya***go***" "***" 'suffix 3 1)))


     (test 'error
           (guard (exn (else 'error))
                  (map textual->string
                       (textual-split "" "" 'strict-infix 3 0 0))))

     (test '("b")
           (map textual->string
                (textual-split "abc" "" 'strict-infix 3 1 2)))

     (test '("oo" "" "much" " ")
           (map textual->string
                (textual-split "too  much  data" " " 'strict-infix 3 1 11)))

     (test '()
           (map textual->string
                (textual-split "" "" 'prefix 3 0 0)))

     (test '("b")
           (map textual->string
                (textual-split "abc" "" 'prefix 3 1 2)))

     (test '("oo" "" "much" " ")
           (map textual->string
                (textual-split "too  much  data" " " 'prefix 3 1 11)))

     (test '()
           (map textual->string
                (textual-split "" "" 'suffix 3 0 0)))

     (test '("b")
           (map textual->string
                (textual-split "abc" "" 'suffix 3 1 2)))

     (test '("oo" "" "much" " ")
           (map textual->string
                (textual-split "too  much  data" " " 'suffix 3 1 11)))

     )))
