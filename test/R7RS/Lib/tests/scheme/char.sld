;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme char) procedures:
;;;
;;;     char-downcase
;;;     char-foldcase
;;;     char-upcase
;;;
;;;     char-ci<=?
;;;     char-ci<?
;;;     char-ci=?
;;;     char-ci>=?
;;;     char-ci>?
;;;
;;;     char-alphabetic?
;;;     char-lower-case?
;;;     char-numeric?
;;;     char-upper-case?
;;;     char-whitespace?
;;;
;;;     digit-value
;;;
;;;     string-ci<=?
;;;     string-ci<?
;;;     string-ci=?
;;;     string-ci>=?
;;;     string-ci>?
;;;
;;;     string-downcase
;;;     string-foldcase
;;;     string-upcase


(define-library (tests scheme char)
  (export run-char-tests)
  (import (scheme base)
          (scheme char)
          (tests scheme test))

  (cond-expand
   (full-unicode (include "char.body.scm"))
   ((not full-unicode)
    (begin
     (define (run-char-tests-for-unicode) #t))))

  (begin

   (define (run-char-tests)

     (test (char-upcase #\i) #\I)
     (test (char-downcase #\i) #\i)
     (test (char-foldcase #\i) #\i)

     (test (char-ci<? #\z #\Z) #f)
     (test (char-ci<? #\Z #\z) #f)
     (test (char-ci<? #\a #\Z) #t)
     (test (char-ci<? #\Z #\a) #f)
     (test (char-ci<=? #\z #\Z) #t)
     (test (char-ci<=? #\Z #\z) #t)
     (test (char-ci<=? #\a #\Z) #t)
     (test (char-ci<=? #\Z #\a) #f)
     (test (char-ci=? #\z #\a) #f)
     (test (char-ci=? #\z #\Z) #t)
     (test (char-ci>? #\z #\Z) #f)
     (test (char-ci>? #\Z #\z) #f)
     (test (char-ci>? #\a #\Z) #f)
     (test (char-ci>? #\Z #\a) #t)
     (test (char-ci>=? #\Z #\z) #t)
     (test (char-ci>=? #\z #\Z) #t)
     (test (char-ci>=? #\z #\Z) #t)
     (test (char-ci>=? #\a #\z) #f)

     (test (char-alphabetic? #\a) #t)
     (test (char-alphabetic? #\1) #f)
     (test (char-numeric? #\1) #t)
     (test (char-numeric? #\a) #f)
     (test (char-whitespace? #\space) #t)
     (test (char-whitespace? #\a) #f)
     (test (char-upper-case? #\a) #f)
     (test (char-upper-case? #\A) #t)
     (test (char-lower-case? #\a) #t)
     (test (char-lower-case? #\A) #f)

     (test (string-upcase "Hi") "HI")
     (test (string-upcase "HI") "HI")
     (test (string-downcase "Hi") "hi")
     (test (string-downcase "hi") "hi")
     (test (string-foldcase "Hi") "hi")
     (test (string-foldcase "HI") "hi")
     (test (string-foldcase "hi") "hi")

     (test (string-downcase "STRASSE")  "strasse")
    
     (test (string-ci<? "a" "Z") #t)
     (test (string-ci<? "A" "z") #t)
     (test (string-ci<? "Z" "a") #f)
     (test (string-ci<? "z" "A") #f)
     (test (string-ci<? "z" "Z") #f)
     (test (string-ci<? "Z" "z") #f)
     (test (string-ci>? "a" "Z") #f)
     (test (string-ci>? "A" "z") #f)
     (test (string-ci>? "Z" "a") #t)
     (test (string-ci>? "z" "A") #t)
     (test (string-ci>? "z" "Z") #f)
     (test (string-ci>? "Z" "z") #f)
     (test (string-ci=? "z" "Z") #t)
     (test (string-ci=? "z" "a") #f)
     (test (string-ci<=? "a" "Z") #t)
     (test (string-ci<=? "A" "z") #t)
     (test (string-ci<=? "Z" "a") #f)
     (test (string-ci<=? "z" "A") #f)
     (test (string-ci<=? "z" "Z") #t)
     (test (string-ci<=? "Z" "z") #t)
     (test (string-ci>=? "a" "Z") #f)
     (test (string-ci>=? "A" "z") #f)
     (test (string-ci>=? "Z" "a") #t)
     (test (string-ci>=? "z" "A") #t)
     (test (string-ci>=? "z" "Z") #t)
     (test (string-ci>=? "Z" "z") #t)

     (let* ((w #\a)
            (x #\N)
            (y #\z)
            (z (integer->char (+ 13 (char->integer w)))))

       (test (char-ci=? x y z)                          #f)
       (test (char-ci=? x x z)                          #t)
       (test (char-ci=? w x y)                          #f)
       (test (char-ci=? y x w)                          #f)

       (test (char-ci<? x y z)                          #f)
       (test (char-ci<? x x z)                          #f)
       (test (char-ci<? w x y)                          #t)
       (test (char-ci<? y x w)                          #f)

       (test (char-ci>? x y z)                          #f)
       (test (char-ci>? x x z)                          #f)
       (test (char-ci>? w x y)                          #f)
       (test (char-ci>? y x w)                          #t)

       (test (char-ci<=? x y z)                         #f)
       (test (char-ci<=? x x z)                         #t)
       (test (char-ci<=? w x y)                         #t)
       (test (char-ci<=? y x w)                         #f)

       (test (char-ci>=? x y z)                         #f)
       (test (char-ci>=? x x z)                         #t)
       (test (char-ci>=? w x y)                         #f)
       (test (char-ci>=? y x w)                         #t)


       (test (char-ci=? x x)                            #t)
       (test (char-ci=? w x)                            #f)
       (test (char-ci=? y x)                            #f)

       (test (char-ci<? x x)                            #f)
       (test (char-ci<? w x)                            #t)
       (test (char-ci<? y x)                            #f)

       (test (char-ci>? x x)                            #f)
       (test (char-ci>? w x)                            #f)
       (test (char-ci>? y x)                            #t)

       (test (char-ci<=? x x)                           #t)
       (test (char-ci<=? w x)                           #t)
       (test (char-ci<=? y x)                           #f)

       (test (char-ci>=? x x)                           #t)
       (test (char-ci>=? w x)                           #f)
       (test (char-ci>=? y x)                           #t))

     (test (map digit-value (string->list "0123456789abcDEF"))
           '(0 1 2 3 4 5 6 7 8 9 #f #f #f #f #f #f))

     (run-char-tests-for-unicode)

     ;;
     )))
