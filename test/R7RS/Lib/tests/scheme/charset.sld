;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme charset) procedures and constants:
;;;
;;;     char-set?
;;;     char-set=
;;;     char-set<=
;;;     char-set-hash 
;;;     char-set-cursor
;;;     char-set-ref
;;;     char-set-cursor-next
;;;     end-of-char-set?
;;;     char-set-fold
;;;     char-set-unfold
;;;     char-set-unfold!
;;;     char-set-for-each
;;;     char-set-map
;;;     char-set-copy
;;;     char-set
;;;  
;;;     list->char-set
;;;     string->char-set 
;;;     list->char-set!
;;;     string->char-set! 
;;;  
;;;     char-set-filter
;;;     char-set-filter!
;;;     ucs-range->char-set
;;;     ucs-range->char-set!
;;;     ->char-set
;;;  
;;;     char-set->list
;;;     char-set->string
;;;  
;;;     char-set-size
;;;     char-set-count
;;;     char-set-contains?
;;;     char-set-every
;;;     char-set-any
;;;  
;;;     char-set-adjoin
;;;     char-set-adjoin!
;;;     char-set-delete 
;;;     char-set-delete!
;;;  
;;;     char-set-complement
;;;     char-set-complement!
;;;     char-set-union
;;;     char-set-union!
;;;     char-set-intersection
;;;     char-set-intersection!
;;;  
;;;     char-set-difference
;;;     char-set-difference!
;;;     char-set-xor
;;;     char-set-xor! char-set-diff+intersection!
;;;     char-set-diff+intersection
;;;     char-set-diff+intersection!
;;;  
;;;     char-set:lower-case
;;;     char-set:upper-case
;;;     char-set:title-case
;;;     char-set:letter
;;;     char-set:digit
;;;     char-set:letter+digit
;;;     char-set:graphic
;;;     char-set:printing
;;;     char-set:whitespace
;;;     char-set:iso-control
;;;     char-set:punctuation
;;;     char-set:symbol
;;;     char-set:hex-digit
;;;     char-set:blank
;;;     char-set:ascii
;;;     char-set:empty
;;;     char-set:full

(define-library (tests scheme charset)
  (export run-charset-tests)
  (import (scheme base)
          (scheme char)
          (scheme charset)
          (tests scheme test))

  ;; Adapted from srfi-14-test.sps

  (cond-expand
   ((or full-unicode
        full-unicode-strings)
    (begin (define (supports-full-unicode?) #t)))
   (else
    (begin (define (supports-full-unicode?) #f))))

  (begin

   (define (run-charset-tests)

     (define (remv x lst)
       (cond ((null? lst)
              '())
             ((eqv? x (car lst))
              (remv x (cdr lst)))
             (else
              (cons (car lst)
                    (remv x (cdr lst))))))

     (if (supports-full-unicode?)
         (test (>= (char-set-size char-set:full)
                   (- #x110000 (- #xe000 #xd800)))
               #t))

     (test (char-set? (char-set))                                           #t)
     (test (char-set? (string->char-set "abcde"))                           #t)
     (test (char-set? 37)                                                   #f)
     (test (char-set? "abcde")                                              #f)

     (test (char-set= (char-set) (char-set))                                #t)
     (test (char-set= (char-set #\a #\b #\c) (char-set #\c #\b #\a))        #t)
     (test (char-set= (char-set #\a #\b #\c) (char-set #\c #\d #\a))        #f)
     (test (char-set=)                                                      #t)
     (test (char-set= (char-set))                                           #t)

     (test (char-set<= (char-set) (char-set))                               #t)
     (test (char-set<= (char-set #\a #\b) (char-set #\a #\b #\c))           #t)
     (test (char-set<= (char-set #\a #\b #\c) (char-set #\a #\b))           #f)
     (test (char-set<=)                                                     #t)
     (test (char-set<= (char-set))                                          #t)

     (test (let ((h (char-set-hash (char-set #\a #\b #\c) 3737)))
             (and (<= 0 h) (< h 3737)))
           #t)
     (test (equal? (char-set-hash (char-set #\a #\b #\c))
                   (char-set-hash (char-set #\b #\c #\a)))
           #t)

     (test (let ((cs (char-set #\G #\a #\T #\e #\c #\h)))
             (let lp ((cur (char-set-cursor cs)) (ans '()))
               (if (end-of-char-set? cur)
                   ans
                   (lp (char-set-cursor-next cs cur)
                       (cons (char-set-ref cs cur) ans)))))
           '(#\h #\e #\c #\a #\T #\G))

     (test (let ((ms (char-set-fold cons '() (char-set #\a #\b #\c #\T))))
             (and (memv #\a ms)
                  (memv #\b ms)
                  (memv #\c ms)
                  (memv #\T ms)
                  (= (length ms) 4)))
           #t)

     (test (char-set=
            (char-set-unfold null? car cdr (string->list "abracadabra"))
            (string->char-set "abracadabra"))
           #t)
     (test (char-set= (char-set-unfold null? car cdr
                                       (string->list "abracadabra")
                                       (char-set #\f))
                      (string->char-set "abracadabraf"))
           #t)
     (test (char-set= (char-set-unfold! null? car cdr
                                        (string->list "abracadabra")
                                        (char-set #\f))
                      (string->char-set "abracadabraf"))
           #t)

     (test (let ((chars (string->list "fnord")))
             (null? (begin
                     (char-set-for-each (lambda (c)
                                          (if (not (memv c chars))
                                              (error "char-set-for-each" c))
                                          (set! chars (remv c chars)))
                                        (list->char-set chars))
                     chars)))
           #t)

     (test (let ((chars (string->list "fnord")))
             (let ((newchars (char-set-map (lambda (c)
                                             (if (not (memv c chars))
                                                 (error "char-set-map" c))
                                             (set! chars (remv c chars))
                                             c)
                                           (list->char-set chars))))
               (char-set= (string->char-set "fnord") newchars)))
           #t)

     ;; Shivers's tests.

     (let-syntax ((test/true (syntax-rules ()
                              ((test form ...)
                               (begin (test form #t) ...)))))
       (let ((vowel? (lambda (c) 
                       (member c '(#\a #\e #\i #\o #\u)))))

         (test/true

          (not (char-set? 5))
     
          (char-set? (char-set #\a #\e #\i #\o #\u))
     
          (char-set=)
          (char-set= (char-set))
     
          (char-set= (char-set #\a #\e #\i #\o #\u)
                     (string->char-set "ioeauaiii"))
     
          (not (char-set= (char-set #\e #\i #\o #\u)
                          (string->char-set "ioeauaiii")))
     
          (char-set<=)
          (char-set<= (char-set))
     
          (char-set<= (char-set #\a #\e #\i #\o #\u)
                      (string->char-set "ioeauaiii"))
     
          (char-set<= (char-set #\e #\i #\o #\u)
                      (string->char-set "ioeauaiii"))
     
          (<= 0 (char-set-hash char-set:graphic 100) 99)
     
          (= 4 (char-set-fold (lambda (c i) (+ i 1)) 0
                              (char-set #\e #\i #\o #\u #\e #\e)))
     
          (char-set= (string->char-set "eiaou2468013579999")
                     (char-set-unfold null? car cdr
                                      '(#\a #\e #\i #\o #\u #\u #\u)
                                      (char-set-intersection char-set:digit
                                                             char-set:ascii)))
     
          (char-set= (string->char-set "eiaou246801357999")
                     (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                       (string->char-set "0123456789")))
     
          (not (char-set= (string->char-set "eiaou246801357")
                          (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                            (string->char-set "0123456789"))))
     
          (let ((cs (string->char-set "0123456789")))
            (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                               (string->char-set "02468000"))
            (char-set= cs (string->char-set "97531")))
     
          (not (let ((cs (string->char-set "0123456789")))
                 (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                                    (string->char-set "02468"))
                 (char-set= cs (string->char-set "7531"))))
     
          (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                     (string->char-set "IOUAEEEE"))
     
          (not (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                          (string->char-set "OUAEEEE")))
     
          (char-set= (char-set-copy (string->char-set "aeiou"))
                     (string->char-set "aeiou"))
     
          (char-set= (char-set #\x #\y) (string->char-set "xy"))
          (not (char-set= (char-set #\x #\y #\z) (string->char-set "xy")))
     
          (char-set= (string->char-set "xy") (list->char-set '(#\x #\y)))
          (not (char-set= (string->char-set "axy") (list->char-set '(#\x #\y))))
     
          (char-set= (string->char-set "xy12345")
                     (list->char-set '(#\x #\y) (string->char-set "12345")))
          (not (char-set= (string->char-set "y12345")
                          (list->char-set '(#\x #\y)
                                          (string->char-set "12345"))))

          (char-set= (string->char-set "xy12345")
                     (list->char-set! '(#\x #\y) (string->char-set "12345")))
          (not (char-set= (string->char-set "y12345")
                          (list->char-set! '(#\x #\y)
                                           (string->char-set "12345"))))

          (char-set= (string->char-set "aeiou12345")
                     (char-set-filter vowel?
                                      char-set:ascii
                                      (string->char-set "12345")))
          (not (char-set= (string->char-set "aeou12345")
                          (char-set-filter vowel?
                                           char-set:ascii
                                           (string->char-set "12345"))))

          (char-set= (string->char-set "aeiou12345")
                     (char-set-filter! vowel?
                                       char-set:ascii
                                       (string->char-set "12345")))
          (not (char-set= (string->char-set "aeou12345")
                          (char-set-filter! vowel?
                                            char-set:ascii
                                            (string->char-set "12345"))))


          (char-set= (string->char-set "abcdef12345")
                     (ucs-range->char-set 97 103 #t (string->char-set "12345")))
          (not (char-set= (string->char-set "abcef12345")
                          (ucs-range->char-set 97
                                               103
                                               #t
                                               (string->char-set "12345"))))

          (char-set= (string->char-set "abcdef12345")
                     (ucs-range->char-set! 97 103 #t (string->char-set "12345")))
          (not (char-set= (string->char-set "abcef12345")
                          (ucs-range->char-set! 97
                                                103
                                                #t
                                                (string->char-set "12345"))))


          (char-set= (->char-set #\x)
                     (->char-set "x")
                     (->char-set (char-set #\x)))
     
          (not (char-set= (->char-set #\x)
                          (->char-set "y")
                          (->char-set (char-set #\x))))
     
          (= 10 (char-set-size
                 (char-set-intersection char-set:ascii char-set:digit)))

          (= 5 (char-set-count vowel? char-set:ascii))
     
          (equal? '(#\x) (char-set->list (char-set #\x)))
          (not (equal? '(#\X) (char-set->list (char-set #\x))))
     
          (equal? "x" (char-set->string (char-set #\x)))
          (not (equal? "X" (char-set->string (char-set #\x))))
     
          (char-set-contains? (->char-set "xyz") #\x)
          (not (char-set-contains? (->char-set "xyz") #\a))
     
          (char-set-every char-lower-case? (->char-set "abcd"))
          (not (char-set-every char-lower-case? (->char-set "abcD")))
          (char-set-any char-lower-case? (->char-set "abcd"))
          (not (char-set-any char-lower-case? (->char-set "ABCD")))
     
          (char-set= (->char-set "ABCD")
                     (let ((cs (->char-set "abcd")))
                       (let lp ((cur (char-set-cursor cs)) (ans '()))
                         (if (end-of-char-set? cur) (list->char-set ans)
                             (lp (char-set-cursor-next cs cur)
                                 (cons (char-upcase (char-set-ref cs cur))
                                       ans))))))


          (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                     (->char-set "123xa"))
          (not (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                          (->char-set "123x")))
          (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                     (->char-set "123xa"))
          (not (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                          (->char-set "123x")))
     
          (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                     (->char-set "13"))
          (not (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                          (->char-set "13a")))
          (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                     (->char-set "13"))
          (not (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                          (->char-set "13a")))

          (char-set= (char-set-intersection char-set:hex-digit
                                            (char-set-complement char-set:digit))
                     (->char-set "abcdefABCDEF"))
          (char-set= (char-set-intersection! (char-set-complement!
                                              (->char-set "0123456789"))
                                             char-set:hex-digit)
                     (->char-set "abcdefABCDEF"))

          (char-set= (char-set-union char-set:hex-digit
                                     (->char-set "abcdefghijkl"))
                     (->char-set "abcdefABCDEFghijkl0123456789"))
          (char-set= (char-set-union! (->char-set "abcdefghijkl")
                                      char-set:hex-digit)
                     (->char-set "abcdefABCDEFghijkl0123456789"))
     
          (char-set= (char-set-difference (->char-set "abcdefghijklmn")
                                          char-set:hex-digit)
                     (->char-set "ghijklmn"))
          (char-set= (char-set-difference! (->char-set "abcdefghijklmn")
                                           char-set:hex-digit)
                     (->char-set "ghijklmn"))
     
          (char-set= (char-set-xor (->char-set "0123456789")
                                   char-set:hex-digit)
                     (->char-set "abcdefABCDEF"))
          (char-set= (char-set-xor! (->char-set "0123456789")
                                    char-set:hex-digit)
                     (->char-set "abcdefABCDEF"))
     
          (call-with-values (lambda ()
                              (char-set-diff+intersection char-set:hex-digit
                                                          char-set:letter))
            (lambda (d i)
              (and (char-set= d (->char-set "0123456789"))
                   (char-set= i (->char-set "abcdefABCDEF")))))
     
          (call-with-values (lambda ()
                              (char-set-diff+intersection!
                               (char-set-copy char-set:hex-digit)
                               (char-set-copy char-set:letter)))
            (lambda (d i)
              (and (char-set= d (->char-set "0123456789"))
                   (char-set= i (->char-set "abcdefABCDEF")))))))))

   ))
