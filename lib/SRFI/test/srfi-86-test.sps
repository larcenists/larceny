; Test suite for SRFI-86
;
; $Id$
;
; Very basic tests, taken directly from SRFI 86.

(import (rnrs base)
        (rnrs io simple)
        (srfi :6 basic-string-ports)
        (srfi :86 mu-and-nu))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (equal? (alet (a (mu 1 2) ((b c) (mu 3 4)))
              (list a b c))
            '((1 2) 3 4))
    (fail 'ex1))

(or (equal? (let ((p (open-output-string)))
              (alet ((a (begin (display "1st" p) 1))
                     (b c (mu (begin (display "2nd" p) 2) 3))
                     (d (begin (display "3rd" p) 4))
                     ((e . f) (mu (begin (display "4th" p) 5) 6)))
                (vector (get-output-string p)
                        (list a b c d e f))))
            '#("1st2nd3rd4th" (1 2 3 4 5 (6))))
    (fail 'ex2))

(or (equal? (alet* (((a b) (mu 1 2))
                    ((c d e) a (+ a b c) (+ a b c d))
                    ((f . g) (mu 5 6 7))
                    ((h i j . k) e 9 10 h i j))
               (list a b c d e f g h i j k))
            '(1 2 1 4 8 5 (6 7) 8 9 10 (8 9 10)))
    (fail 'ex3))

(or (equal? (alet* tag ((a 1)
                        (a b b c (mu (+ a 2) 4 5 6))
                        ((d e e) b 5 (+ a b c)))
              (if (< a 10) (tag a 10 b c c d e d) (list a b c d e)))
            '(10 6 6 5 5))
    (fail 'ex4))

(or (equal? (alet* ((a 1)
                    ((b 2) (b c c (mu 3 4 5))
                           ((d e d (mu a b c)) . intag) . tag)
                    (f 6))
              (if (< d 10)
                  (intag d e 10)
                  (if (< c 10)
                      (tag b 11 c 12 a b d intag)
                      (list a b c d e f))))
            '(1 11 12 10 3 6))
    (fail 'ex5))

(or (let ((p (open-output-string)))
      (and (equal? (alet ((exit)
                          (a (begin (display "1st" p) 1))
                          (b c (mu (begin (display "2nd" p) 2)
                                   (begin (display "3rd" p) 3))))
                     (display (list a b c) p)
                     (exit 10)
                     (display "end" p))
                   10)
           (equal? (get-output-string p) "1st2nd3rd(1 2 3)")))
    (fail 'ex6))

(or (let ((p (open-output-string)))
      (and (equal? (alet ((and (a (begin (display "1st" p) 1))
                               (b (begin (display "2nd" p) 2))
                               (c (begin (display "false" p) #f))
                               (d (begin (display "3nd" p) 3))))
                     (list a b c d))
                   #f)
           (equal? (get-output-string p) "1st2ndfalse")))
    (fail 'ex7))

(or (equal? ((lambda (str . rest)
               (alet* ((len (string-length str))
                       (opt rest
                            (start 0 (integer? start)
                                     (if (< start 0)
                                         0
                                         (if (< len start) len start))) ;true
                            (end len (integer? end)
                                     (if (< end start)
                                         start
                                         (if (< len end) len end)))));true
                 (substring str start end)))
             "abcdefg" 1 20)
            "bcdefg")
    (fail 'ex8a))

(or (equal? ((lambda (str . rest)
               (alet* ((len (string-length str))
                       (min (apply min rest))
                       (cat rest
                            (start 0 (= start min)
                             (if (< start 0)
                                 0
                                 (if (< len start) len start)));true
                            (end len (integer? end)
                                 (if (< end start)
                                     start
                                     (if (< len end) len end)))));true
                 (substring str start end)))
             "abcdefg" 20 1)
            "bcdefg")
    (fail 'ex8b))

(or (equal? ((lambda (str . rest)
               (alet ((cat rest
                           (start 0
                                  (and (list? start) (= 2 (length start))
                                       (eq? 'start (car start)))
                                  (cadr start)) ; true
                           (end (string-length str)
                                (and (list? end)
                                     (= 2 (length end))
                                     (eq? 'end (car end)))
                                (cadr end)))) ; true
                 (substring str start end)))
             "abcdefg" '(end 6) '(start 1))
            "bcdef")
    (fail 'ex8c))

(define rest-list '(a 10 cc 30 40 b 20))

(or (equal? (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) . d)) (list a b c d))
            '(10 2 30 (40 b 20)))
    (fail 'ex9a))

(or (equal?
     (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) #f . d)) (list a b c d))
     '(10 2 30 (40 b 20)))
    (fail 'ex9b))

(or (equal?
     (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) #t . d)) (list a b c d))
     '(10 20 30 (40)))
    (fail 'ex9c))

(define rest (list 'a 10 'd 40 "c" 30 50 'b 20))

(or (equal?
     (alet ((key rest (a 1) (b 2) ((c "c") 3) . d)) (list a b c d))
     '(10 2 30 (d 40 50 b 20)))
    (fail 'ex9d))

(or (equal?
     (alet ((key rest (a 1) (b 2) ((c "c") 3) #f . d)) (list a b c d))
     '(10 2 3 (d 40 "c" 30 50 b 20)))
    (fail 'ex9e))

(or (equal?
     (alet ((key rest (a 1) (b 2) ((c "c") 3) #t . d)) (list a b c d))
     '(10 20 30 (d 40 50)))
    (fail 'ex9f))

(or (equal?
     ((lambda (m . n)
        (alet* ((opt n (a 10) (b 20) (c 30) . d)
                (key d (x 100) (y 200) (a 300)))
          (list m a b c x y)))
      0 1 2 3 'a 30 'y 20 'x 10)
     '(0 30 2 3 10 20))
    (fail 'ex9g))

(or (equal?
     ((lambda (m . n)
        (alet* ((key n (x 100) (y 200) (a 300) . d)
                (opt d (a 10) (b 20) (c 30)))
          (list m a b c x y)))
      0 'a 30 'y 20 'x 10 1 2 3)
     '(0 1 2 3 10 20))
    (fail 'ex9h))

(or (equal?
     (alet* ((a 1)
             (rec (a 2) (b 3) (b (lambda () c)) (c a))
             (d 50))
       (list a (b) c d))
     '(2 2 2 50))
    (fail 'ex10))

(or (equal?
     (alet ((a b (mu 1 2))
            (values c d (values 3 4)) ;This is different from SRFI 71.
            ((e f) (mu 5 6))
            ((values g h) (values 7 8))
            ((i j . k) (nu 9 '(10 11 12)))
            ((values l m . n) (apply values 13 '(14 15 16)))
            o (mu 17 18)
            ((values . p) (values 19 20)))
       (list a b c d e f g h i j k l m n o p))
     '(1 2 3 4 5 6 7 8 9 10 (11 12) 13 14 (15 16) (17 18) (19 20)))
    (fail 'ex11))

(or (equal?
     (alet ((a 1)
            (() (define a 10) (define b 100))
            (b a))
       (list a b))
     '(1 10))
    (fail 'ex12a))

(or (equal?
     (alet* ((a 1)
             (() (define a 10) (define b 100))
             (b a))
       (list a b))
     '(10 10))
    (fail 'ex12b))

(writeln "Done.")
