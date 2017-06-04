;; Copyright (C) John Cowan 2013. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; The copyright notice above is taken from srfi-116-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme ilist) procedures:
;;;
;;;     iq
;;;     ipair
;;;     ilist
;;;     xipair
;;;     ipair*
;;;     make-ilist
;;;     ilist-tabulate
;;;     iiota
;;;     ipair?
;;;     proper-ilist?
;;;     ilist?
;;;     dotted-ilist?
;;;     not-ipair?
;;;     null-ilist?
;;;     ilist=
;;;     icar
;;;     icdr
;;;     ilist-ref
;;;     ifirst
;;;     isecond
;;;     ithird
;;;     ifourth
;;;     ififth
;;;     isixth
;;;     iseventh
;;;     ieighth
;;;     ininth
;;;     itenth
;;;     icaar
;;;     icadr
;;;     icdar
;;;     icddr
;;;     icaaar
;;;     icaadr
;;;     icadar
;;;     icaddr
;;;     icdaar
;;;     icdadr
;;;     icddar
;;;     icdddr
;;;     icaaaar
;;;     icaaadr
;;;     icaadar
;;;     icaaddr
;;;     icadaar
;;;     icadadr
;;;     icaddar
;;;     icadddr
;;;     icdaaar
;;;     icdaadr
;;;     icdadar
;;;     icdaddr
;;;     icddaar
;;;     icddadr
;;;     icdddar
;;;     icddddr
;;;     icar+icdr
;;;     itake
;;;     idrop
;;;     ilist-tail
;;;     itake-right
;;;     idrop-right
;;;     isplit-at
;;;     ilast
;;;     last-ipair
;;;     ilength
;;;     iappend
;;;     iconcatenate
;;;     ireverse
;;;     iappend-reverse
;;;     izip
;;;     iunzip1
;;;     iunzip2
;;;     iunzip3
;;;     iunzip4
;;;     iunzip5
;;;     icount
;;;     imap
;;;     ifor-each
;;;     ifold
;;;     iunfold
;;;     ipair-fold
;;;     ireduce 
;;;     ifold-right
;;;     iunfold-right
;;;     ipair-fold-right
;;;     ireduce-right 
;;;     iappend-map
;;;     ipair-for-each
;;;     ifilter-map
;;;     imap-in-order
;;;     ifilter
;;;     ipartition
;;;     iremove
;;;     imember
;;;     imemq
;;;     imemv
;;;     ifind
;;;     ifind-tail
;;;     iany
;;;     ievery
;;;     ilist-index
;;;     itake-while
;;;     idrop-while
;;;     ispan
;;;     ibreak
;;;     idelete
;;;     idelete-duplicates 
;;;     iassoc
;;;     iassq
;;;     iassv
;;;     ialist-cons
;;;     ialist-delete
;;;     replace-icar
;;;     replace-icdr
;;;     pair->ipair
;;;     ipair->pair
;;;     list->ilist
;;;     ilist->list
;;;     tree->itree
;;;     itree->tree
;;;     gtree->itree
;;;     gtree->tree
;;;     iapply
;;;
;;;     ipair-comparator
;;;     ilist-comparator
;;;     make-ipair-comparator
;;;     make-ilist-comparator
;;;     make-improper-ilist-comparator
;;;     make-icar-comparator
;;;     make-icdr-comparator


(define-library (tests scheme ilist)
  (export run-ilist-tests)
  (import (scheme base)
          (scheme ilist)
          (tests scheme test)
          (scheme comparator))

  ;; Adapted from srfi-116-test.sps

  (begin

   (define (iequal? x y)
     (cond ((and (ipair? x) (ipair? y))
            (and (iequal? (icar x) (icar y))
                 (iequal? (icdr x) (icdr y))))
           ((and (pair? x) (pair? y))
            (and (iequal? (car x) (car y))
                 (iequal? (cdr x) (cdr y))))
           ((and (vector? x)
                 (vector? y))
            (iequal? (vector->list x) (vector->list y)))
           (else
            (equal? x y))))

   (define-syntax itest
     (syntax-rules ()
      ((itest expr expected)
       (test (iequal? expr expected) #t))))

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

   (define (run-ilist-tests)

     ;;

     ;; test-group "ilists"

     ;; test-group "ilists/constructors"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (let ()
       (define abc (ilist 'a 'b 'c))
       (define abc-dot-d (ipair* 'a 'b 'c 'd))
       (itest (icar abc) 'a)
       (itest (icadr abc) 'b)
       (itest (icaddr abc) 'c)
       (itest (xipair 1 2) (ipair 2 1))
       (itest (icdddr abc-dot-d) 'd)
       (itest (make-ilist 4 'c) (iq c c c c))
       (itest (ilist-tabulate 4 values) (iq 0 1 2 3))
       (itest (iiota 5) (iq 0 1 2 3 4)))

     ;; test-group "ilists/predicates"

     (test-assert (ipair? (ipair 1 2)))
     (test-assert (proper-ilist? '()))
     (test-assert (proper-ilist? (iq 1 2 3)))
     (test-assert (ilist? '()))
     (test-assert (ilist? (iq 1 2 3)))
     (test-assert (dotted-ilist? (ipair 1 2)))
     (test-assert (dotted-ilist? 2))
     (test-assert (null-ilist? '()))
     (test-assert (not (null-ilist? (iq 1 2 3))))
     (test-error (null-ilist? 'a))
     (test-assert (not-ipair? 'a))
     (test-assert (not (not-ipair? (ipair 'a 'b))))
     (test-assert (ilist= = (iq 1 2 3) (iq 1 2 3)))
     (test-assert (not (ilist= = (iq 1 2 3 4) (iq 1 2 3))))
     (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3 4))))
     (test-assert (ilist= = (iq 1 2 3) (iq 1 2 3)))
     (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3 4) (iq 1 2 3 4))))
     (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3) (iq 1 2 3 4))))

     ;; test-group "ilist/cxrs"

     (let ()
       (define ab (ipair 'a 'b))
       (define cd (ipair 'c 'd))
       (define ef (ipair 'e 'f))
       (define gh (ipair 'g 'h))
       (define abcd (ipair ab cd))
       (define efgh (ipair ef gh))
       (define abcdefgh (ipair abcd efgh))
       (define ij (ipair 'i 'j))
       (define kl (ipair 'k 'l))
       (define mn (ipair 'm 'n))
       (define op (ipair 'o 'p))
       (define ijkl (ipair ij kl))
       (define mnop (ipair mn op))
       (define ijklmnop (ipair ijkl mnop))
       (define abcdefghijklmnop (ipair abcdefgh ijklmnop))
       (itest (icaar abcd) 'a)
       (itest (icdar abcd) 'b)
       (itest (icadr abcd) 'c)
       (itest (icddr abcd) 'd)
       (itest (icaaar abcdefgh) 'a)
       (itest (icdaar abcdefgh) 'b)
       (itest (icadar abcdefgh) 'c)
       (itest (icddar abcdefgh) 'd)
       (itest (icaadr abcdefgh) 'e)
       (itest (icdadr abcdefgh) 'f)
       (itest (icaddr abcdefgh) 'g)
       (itest (icdddr abcdefgh) 'h)
       (itest (icaaaar abcdefghijklmnop) 'a)
       (itest (icdaaar abcdefghijklmnop) 'b)
       (itest (icadaar abcdefghijklmnop) 'c)
       (itest (icddaar abcdefghijklmnop) 'd)
       (itest (icaadar abcdefghijklmnop) 'e)
       (itest (icdadar abcdefghijklmnop) 'f)
       (itest (icaddar abcdefghijklmnop) 'g)
       (itest (icdddar abcdefghijklmnop) 'h)
       (itest (icaaadr abcdefghijklmnop) 'i)
       (itest (icdaadr abcdefghijklmnop) 'j)
       (itest (icadadr abcdefghijklmnop) 'k)
       (itest (icddadr abcdefghijklmnop) 'l)
       (itest (icaaddr abcdefghijklmnop) 'm)
       (itest (icdaddr abcdefghijklmnop) 'n)
       (itest (icadddr abcdefghijklmnop) 'o)
       (itest (icddddr abcdefghijklmnop) 'p))

     ;; test-group "ilists/selectors"

     (let ()
       (define ten (ilist 1 2 3 4 5 6 7 8 9 10))
       (define abcde (iq a b c d e))
       (define dotted (ipair 1 (ipair 2 (ipair 3 'd))))
       (itest (ilist-ref (iq a b c d) 2) 'c)
       (itest (ifirst ten) 1)
       (itest (isecond ten) 2)
       (itest (ithird ten) 3)
       (itest (ifourth ten) 4)
       (itest (ififth ten) 5)
       (itest (isixth ten) 6)
       (itest (iseventh ten) 7)
       (itest (ieighth ten) 8)
       (itest (ininth ten) 9)
       (itest (itenth ten) 10)
       (test-error (ilist-ref '() 2))
       (itest (call-with-values (lambda () (icar+icdr (ipair 1 2))) list) '(1 2))
       (itest (itake abcde 2) (iq a b))
       (itest (idrop abcde 2) (iq c d e))
       (itest (ilist-tail abcde 2) (iq c d e))
       (itest (itake dotted 2) (iq 1 2))
       (itest (idrop dotted 2) (ipair 3 'd))
       (itest (ilist-tail dotted 2) (ipair 3 'd))
       (itest (idrop dotted 3) 'd)
       (itest (ilist-tail dotted 3) 'd)
       (itest (iappend (itake abcde 4) (idrop abcde 4)) abcde)
       (itest (itake-right abcde 2) (iq d e))
       (itest (idrop-right abcde 2) (iq a b c))
       (itest (itake-right dotted 2) (ipair 2 (ipair 3 'd)))
       (itest (idrop-right dotted 2) (iq 1))
       (itest (itake-right dotted 0) 'd)
       (itest (idrop-right dotted 0) (iq 1 2 3))
       (itest (call-with-values (lambda () (isplit-at abcde 3)) iappend) abcde)
       (itest (ilast (iq a b c)) 'c)
       (itest (last-ipair (iq a b c)) (iq c)))

     ;; test-group "ilists/misc"

     (itest (ilength '()) 0)
     (itest (ilength (iq 1 2 3)) 3)
     (itest (iappend (iq x) (iq y)) (iq x y))
     (itest (iappend (iq a b) (iq c d)) (iq a b c d))
     (itest (iappend '() (iq a)) (iq a))
     (itest (iappend (iq x y)) (iq x y))
     (itest (iappend) '())
     (itest (iconcatenate (iq (a b) (c d))) (iq a b c d))
     (itest (ireverse (iq a b c)) (iq c b a))
     (itest (ireverse (iq a (b c) d (e (f)))) (iq (e (f)) d (b c) a))
     (itest (iappend-reverse (iq 1 2) 'd) (ipair 2 (ipair 1 'd)))
     (itest (izip (iq one two three) (iq 1 2 3) (iq odd even odd))
            (iq (one 1 odd) (two 2 even) (three 3 odd)))
     (itest (izip (iq 1 2 3)) (iq (1) (2) (3)))
     (itest (iunzip1 (iq (1) (2) (3))) (iq 1 2 3))
     (itest (call-with-values
             (lambda () (iunzip2 (iq (1 one) (2 two) (3 three))))
             ilist)
            (iq (1 2 3) (one two three)))
     (itest (call-with-values
             (lambda () (iunzip3 (iq (1 one a) (2 two b) (3 three c))))
             ilist)
            (iq (1 2 3) (one two three) (a b c)))
     (itest (call-with-values
             (lambda () (iunzip4 (iq (1 one a 4) (2 two b 5) (3 three c 6))))
             ilist)
            (iq (1 2 3) (one two three) (a b c) (4 5 6)))
     (itest (call-with-values
            (lambda ()
              (iunzip5 (iq (1 one a 4 #t) (2 two b 5 #f) (3 three c 6 #t))))
            ilist)
            (iq (1 2 3) (one two three) (a b c) (4 5 6) (#t #f #t)))
     (itest (icount even? (iq 3 1 4 1 5 9 2 5 6)) 3)
     (itest (icount < (iq 1 2 4 8) (iq 2 4 6 8 10 12 14 16)) 3)

     ;; test-group "ilists/folds"

     ;; We have to be careful to test both single-list and multiple-list
     ;; code paths, as they may be different.

     (let ()
       (define lis (iq 1 2 3))
       (define (z x y ans) (ipair (ilist x y) ans))
       (define squares (iq 1 4 9 16 25 36 49 64 81 100))
       (itest (ifold + 0 lis) 6)
       (itest (ifold ipair '() lis) (iq 3 2 1))
       (itest (ifold
               (lambda (x count) (if (symbol? x) (+ count 1) count))
               0
               (iq a 0 b))
              2)
       (itest (ifold
               (lambda (s max-len) (max max-len (string-length s)))
               0
               (iq "ab" "abcd" "abc"))
              4)
       (itest (ifold
               (lambda (a b ans) (+ (* a b) ans))
                0
                (iq 1 2 3)
                (iq 4 5 6))
              32)
       (itest (ifold z '() (iq a b) (iq c d))
              (iq (b d) (a c)))
       (itest (ifold-right ipair '() lis) lis)
       (itest (ifold-right
               (lambda (x l) (if (even? x) (ipair x l) l))
                 '()
                 (iq 0 1 2 3 4))
              (iq 0 2 4))
       (itest (ifold-right z '() (iq a b) (iq c d))
              (iq (a c) (b d)))
       (itest (ipair-fold ipair '() (iq a b c))
              (iq (c) (b c) (a b c)))
       (itest (ipair-fold z '() (iq a b) (iq c d))
              (iq ((b) (d)) ((a b) (c d))))
       (itest (ipair-fold-right ipair '() (iq a b c))
              (iq (a b c) (b c) (c)))
       (itest (ipair-fold-right z '() (iq a b) (iq c d))
              (iq ((a b) (c d)) ((b) (d))))
       (itest (ireduce max 0 (iq 1 3 5 4 2 0)) 5)
       (itest (ireduce - 0 (iq 1 2)) 1)
       (itest (ireduce-right - 0 (iq 1 2)) -1)
       (itest (iunfold (lambda (x) (> x 10))
                       (lambda (x) (* x x))
                       (lambda (x) (+ x 1))
                       1)
              squares)
       (itest (iunfold-right zero? 
                             (lambda (x) (* x x))
                             (lambda (x) (- x 1))
                             10)
              squares)
       (itest (iunfold null-ilist? icar icdr (iq 1 2 3)) (iq 1 2 3))
       (itest (iunfold-right null-ilist? icar icdr (iq 1 2 3)) (iq 3 2 1))
       (itest (iunfold null-ilist? icar icdr (iq 1 2) (lambda (x) (iq 3 4)))
              (iq 1 2 3 4))
       (itest (imap icadr (iq (a b) (d e) (g h))) (iq b e h))
       (itest (imap-in-order icadr (iq (a b) (d e) (g h))) (iq b e h))
       (itest (imap + (iq 1 2 3) (iq 4 5 6)) (iq 5 7 9))
       (itest (imap-in-order + (iq 1 2 3) (iq 4 5 6)) (iq 5 7 9))
       (set! z
             (let ((count 0))
               (lambda (ignored) (set! count (+ count 1))    ; FIXME: what?
               count)))
       (itest (imap-in-order z (iq a b)) (iq 1 2))
       (itest (let ((v (make-vector 5)))
                (ifor-each (lambda (i)
                             (vector-set! v i (* i i)))
                           (iq 0 1 2 3 4))
                v)
              '#(0 1 4 9 16))
       (itest (let ((v (make-vector 5)))
                (ifor-each (lambda (i j)
                             (vector-set! v i (+ i j)))
                           (iq 0 1 2 3 4)
                           (iq 5 6 7 8 9))
                v)
              '#(5 7 9 11 13))
       (itest (iappend-map (lambda (x) (ilist x (- x))) (iq 1 3 8))
              (iq 1 -1 3 -3 8 -8))
       (itest (iappend-map ilist (iq 1 2 3) (iq 4 5 6))
              (iq 1 4 2 5 3 6))
       (itest (let ((v (make-vector 5)))
                (ipair-for-each (lambda (lis)
                                  (vector-set! v (icar lis) lis))
                                (iq 0 1 2 3 4))
                v)
              (vector (iq 0 1 2 3 4) (iq 1 2 3 4) (iq 2 3 4) (iq 3 4) (iq 4)))
       (itest (let ((v (make-vector 5)))
                (ipair-for-each (lambda (i j) (vector-set! v (icar i) j))
                                (iq 0 1 2 3 4)
                                (iq 5 6 7 8 9))
                v)
              (vector (iq 5 6 7 8 9) (iq 6 7 8 9) (iq 7 8 9) (iq 8 9) (iq 9)))
       (itest (ifilter-map (lambda (x) (and (number? x) (* x x)))
                           (iq a 1 b 3 c 7))
              (iq 1 9 49))
       (itest (ifilter-map
               (lambda (x y) (and (number? x) (number? y) (+ x y)))
               (iq 1 a 2 b 3 4)
               (iq 4 0 5 y 6 z))
              (iq 5 7 9)))

     ;; test-group "ilists/filtering"

     (itest (ifilter even? (iq 0 7 8 8 43 -4)) (iq 0 8 8 -4))
     (itest (call-with-values
             (lambda () (ipartition symbol? (iq one 2 3 four five 6)))
             list)
            (list (iq one four five) (iq 2 3 6)))
     (itest (iremove even? (iq 0 7 8 8 43 -4)) (iq 7 43))

     ;; test-group "ilists/searching"

     (itest (ifind even? (iq 1 2 3)) 2)
     (itest (iany  even? (iq 1 2 3)) #t)
     (itest (ifind even? (iq 1 7 3)) #f)
     (itest (iany  even? (iq 1 7 3)) #f)
   ; (test-error (ifind even? (ipair (1 (ipair 3 x)))))    ; FIXME: what is x?
   ; (test-error (iany  even? (ipair (1 (ipair 3 x)))))    ; FIXME: what is x?
     (itest (ifind even? (iq 3 1 4 1 5 9)) 4)
     (itest (ifind-tail even? (iq 3 1 37 -8 -5 0 0)) (iq -8 -5 0 0))
     (itest (itake-while even? (iq 2 18 3 10 22 9)) (iq 2 18))
     (itest (idrop-while even? (iq 2 18 3 10 22 9)) (iq 3 10 22 9))
     (itest (call-with-values
             (lambda () (ispan even? (iq 2 18 3 10 22 9)))
             list)
            (list (iq 2 18) (iq 3 10 22 9)))
     (itest (call-with-values
             (lambda () (ibreak even? (iq 3 1 4 1 5 9)))
             list)
            (list (iq 3 1) (iq 4 1 5 9)))
     (itest (iany integer? (iq a 3 b 2.7)) #t)
     (itest (iany integer? (iq a 3.1 b 2.7)) #f)
     (itest (iany < (iq 3 1 4 1 5) (iq 2 7 1 8 2)) #t)
     (itest (ievery integer? (iq 1 2 3 4 5)) #t)
     (itest (ievery integer? (iq 1 2 3 4.5 5)) #f)
     (itest (ievery < (iq 1 2 3) (iq 4 5 6)) #t)
     (itest (ilist-index even? (iq 3 1 4 1 5 9)) 2)
     (itest (ilist-index < (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)) 1)
     (itest (ilist-index = (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)) #f)
     (itest (imemq 'a (iq a b c)) (iq a b c))
     (itest (imemq 'b (iq a b c)) (iq b c))
     (itest (imemq 'a (iq b c d)) #f)
     (itest (imemq (ilist 'a) (iq b (a) c)) #f)

     ;; BUG: The following test is incorrect because imember uses equal?,
     ;; and SRFI 116 does not require (or recommend) equal? be extended
     ;; to traverse immutable lists.

   ; (itest (iq (a) c) (imember (ilist 'a) (iq b (a) c)))

     ;; Corrected test:

     (let ((x (iq (a))))
       (itest (imember x (ilist 'b x 'c)) (ilist x 'c)))

     (itest (imemv 101 (iq 100 101 102)) (iq 101 102))

     ;; test-group "ilists/deletion"

     (itest (idelete 3 (iq 1 2 3 4 5)) (iq 1 2 4 5))
     (itest (idelete 5 (iq 3 4 5 6 7) <) (iq 3 4 5))
     (itest (idelete-duplicates (iq a b a c a b c z)) (iq a b c z))

     ;; test-group "ilists/alists"

     (let ()
       (define e (iq (a 1) (b 2) (c 3)))
       (define e2 (iq (2 3) (5 7) (11 13)))
       (itest (iassq 'a e) (iq a 1))
       (itest (iassq 'b e) (iq b 2))
       (itest (iassq 'd e) #f)
       (itest (iassq (ilist 'a) (iq ((a)) ((b)) ((c)))) #f)

       ;; BUG: The following test is incorrect because iassoc uses equal?,
       ;; and SRFI 116 does not require (or recommend) equal? be extended
       ;; to traverse immutable lists.

       ;; (itest (iq (a)) (iassoc (ilist 'a) (iq ((a)) ((b)) ((c)))))

       ;; Corrected test:

       (let* ((y (iq a))
              (x (ilist y)))
         (itest (iassoc y (ilist x (iq (b)) (iq (c)))) x))

       (itest (iassv 5 e2) (iq 5 7))
       (itest (iassoc 5 e2 <) (iq 11 13))
       (itest (ialist-cons 1 (ilist 1) e2) (ipair (iq 1 1) e2))
       (itest (ialist-delete 5 e2) (iq (2 3) (11 13)))
       (itest (ialist-delete 5 e2 <) (iq (2 3) (5 7))))

     ;; test-group "ilists/replacers"

     (itest (replace-icar (ipair 2 3) 1) (ipair 1 3))
     (itest (replace-icdr (ipair 1 2) 3) (ipair 1 3))

     ;; test-group "ilists/conversion"

     (itest (pair->ipair '(1 . 2)) (ipair 1 2))
     (itest (ipair->pair (ipair 1 2)) '(1 . 2))
     (itest (list->ilist '(1 2 3)) (iq 1 2 3))
     (itest (ilist->list (iq 1 2 3)) '(1 2 3))
     (itest (list->ilist '(1 2 . 3)) (ipair 1 (ipair 2 3)))
     (itest (ilist->list (ipair 1 (ipair 2 3))) '(1 2 . 3))
     (itest (tree->itree '((1 . 2) . (3 . 4))) (ipair (ipair 1 2) (ipair 3 4)))
     (itest (itree->tree (ipair (ipair 1 2) (ipair 3 4))) '((1 . 2) . (3 . 4)))
     (itest (gtree->itree (cons (ipair 1 2) (ipair 3 4)))
            (ipair (ipair 1 2) (ipair 3 4)))
     (itest (gtree->tree (cons (ipair 1 2) (ipair 3 4)))
            '((1 . 2) . (3 . 4)))
     (itest (iapply + (iq 1 2 3)) 6)
     (itest (iapply + 1 2 (iq 3 4 5)) 15)

     (ilist-comparator-tests))

   (define (ilist-comparator-tests)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;
     ;;; Tests of (srfi 116 comparators), which were omitted from the
     ;;; reference implementation of (srfi 116).
     ;;;
     ;;; These tests were written by Marco Maggi.
     ;;;
     ;;; See Marco Maggi's post to comp.lang.scheme on 12 June 2015:
     ;;; https://groups.google.com/forum/#!topic/comp.lang.scheme/K3z5MGpHzg0
     ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;
     ;;; Definitions used by Marco Maggi.
     ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     (define-syntax assert
       (syntax-rules ()
        ((_ inv)
         (if (not inv)
             (error "assertion failed: " 'inv)))))
     
     (define-syntax check
       (syntax-rules (=>)
        ((_ expr => expected)
         (test expr expected))))
     
     (define-syntax try
       (syntax-rules (catch)
        ((_ expr (catch exn (whatever result1) (else result2)))
         (guard (exn
                 (#t result1))
          expr))))
     
     (define (non-negative-exact-integer? x)
       (and (exact-integer? x)
            (<= 0 x)))
     
     (define (void) '(#("")))
     
     (define (comparator-comparison-procedure comparator)
       (let ((= (comparator-equality-predicate comparator))
             (< (comparator-ordering-predicate comparator)))
         (lambda (x y)
           (cond ((= x y) 0)
                 ((< x y) -1)
                 (else +1)))))
     
     (define (comparator-compare comparator x y)
       (let ((= (comparator-equality-predicate comparator))
             (< (comparator-ordering-predicate comparator)))
         (cond ((= x y) 0)
               ((< x y) -1)
               (else +1))))
     
     (define exact-integer-comparator
       (make-comparator exact-integer?
                        =
                        <
                        (lambda (n) n)))
     
     (define real-comparator
       (make-comparator real?
                        =
                        <
                        (lambda (x) (exact (round x)))))
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;
     ;;; End of definitions used by Marco Maggi.
     ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     (let ()
     
          ;; test-group "comparator-accessors"
     
       (define-syntax doit
         (syntax-rules ()
           ((_ ?C ?a ?b)
            (begin
             (test-assert  ((comparator-type-test-predicate ?C) ?a))
              (test-assert  ((comparator-type-test-predicate ?C) ?b))
               (test-deny ((comparator-type-test-predicate ?C) (void)))
                (test-assert  ((comparator-equality-predicate ?C) ?a ?a))
                 (test-deny ((comparator-equality-predicate ?C) ?a ?b))
                  (check
                   ((comparator-comparison-procedure ?C) ?a ?a)
                    => 0)
                  (check
                   ((comparator-comparison-procedure ?C) ?a ?b)
                    => -1)
                  (check
                   ((comparator-comparison-procedure ?C) ?b ?a)
                    => +1)
                  (test-assert
                   (let ((x ((comparator-hash-function ?C) ?a)))
                     (and (exact-integer? x)
                          (<= 0 x))))
                  (test-assert
                   (let ((x ((comparator-hash-function ?C) ?b)))
                     (and (exact-integer? x)
                          (<= 0 x))))))
           ))
     
     ;;; --------------------------------------------------------------------
     
       (doit ipair-comparator (iq 1 . 2) (iq 3 . 4))
       (doit ilist-comparator (iq 1 2) (iq 3 4))
     
     ;;; --------------------------------------------------------------------
     ;;; pair comparison
     
       (let ((cmp (comparator-comparison-procedure ipair-comparator)))
         (check (cmp (iq 1 . 2) (iq 1 . 2)) =>  0)
         (check (cmp (iq 1 . 2) (iq 1 . 3)) => -1) ;2 < 3
         (check (cmp (iq 1 . 4) (iq 1 . 3)) => +1) ;4 > 3
         (check (cmp (iq 1 . 0) (iq 2 . 0)) => -1)
         (check (cmp (iq 3 . 0) (iq 2 . 0)) => +1)
         #f)
     
     ;;; --------------------------------------------------------------------
     ;;; list comparison
     
       (let ((cmp (comparator-comparison-procedure ilist-comparator)))
         (check (cmp (iq 1 2) (iq 1 2)) =>  0)
         (check (cmp (iq 1 2) (iq 1 3)) => -1) ;2 < 3
         (check (cmp (iq 1 4) (iq 1 3)) => +1) ;4 > 3
         (check (cmp (iq 1 0) (iq 2 0)) => -1)
         (check (cmp (iq 3 0) (iq 2 0)) => +1)
     
         (check (cmp (iq ) (iq )) => 0)
         (check (cmp (iq ) (iq 1))        => -1)
         (check (cmp (iq 1) (iq ))        => +1)
     
     ;; If first items are equal: compare the CADRs.
     ;;Here one of the CADRs is null.

         (check (cmp (iq 1 2) (iq 1))        => +1)
         (check (cmp (iq 1)   (iq 1 2))        => -1)

    ;;Lists  of  different length, but it does not matter because the CARs are
    ;;non-equal.

         (check (cmp (iq 1 2) (iq 2))        => -1)
         (check (cmp (iq 2)   (iq 1 2))        => +1)
         #f)
     
       #t)
     
        (let ()
     
          ;; test-group "comparator-applicators"
     
       (define-syntax doit
         (syntax-rules ()
           ((_ ?C ?a ?b)
            (begin
             (test-assert  (comparator-test-type ?C ?a))
             (test-assert  (comparator-test-type ?C ?b))
             (test-deny (comparator-test-type ?C (void)))
             (test-assert  (comparator-check-type ?C ?a))
             (test-assert  (comparator-check-type ?C ?b))
             (test-assert
              (try
                   (comparator-check-type ?C (void))
                   (catch E
                    ((&comparator-type-error)
                     #t)
                    (else #f))))
     ;       (test-assert  (comparator-equal? ?C ?a ?a))    ; FIXME
     ;       (test-deny (comparator-equal? ?C ?a ?b))       ; FIXME
             (check
              (comparator-compare ?C ?a ?a)
              => 0)
             (check
              (comparator-compare ?C ?a ?b)
              => -1)
             (check
              (comparator-compare ?C ?b ?a)
              => +1)
             (test-assert
              (let ((x (comparator-hash ?C ?a)))
                (and (exact-integer? x)
                     (<= 0 x))))
             (test-assert
              (let ((x (comparator-hash ?C ?b)))
                (and (exact-integer? x)
                     (<= 0 x))))))
           ))
     
     ;;; --------------------------------------------------------------------
     
       (doit ipair-comparator (iq 1 . 2) (iq 3 . 4))
       (doit ilist-comparator (iq 1 2) (iq 3 4))
     
     ;;; --------------------------------------------------------------------
     ;;; pair comparison
     
       (let ((cmp (comparator-comparison-procedure ipair-comparator)))
         (check (cmp (iq 1 . 2) (iq 1 . 2)) =>  0)
         (check (cmp (iq 1 . 2) (iq 1 . 3)) => -1) ;2 < 3
         (check (cmp (iq 1 . 4) (iq 1 . 3)) => +1) ;4 > 3
         (check (cmp (iq 1 . 0) (iq 2 . 0)) => -1)
         (check (cmp (iq 3 . 0) (iq 2 . 0)) => +1)
         #f)
     
     ;;; --------------------------------------------------------------------
     ;;; list comparison
     
       (let ((cmp (comparator-comparison-procedure ilist-comparator)))
         (check (cmp (iq 1 2) (iq 1 2)) =>  0)
         (check (cmp (iq 1 2) (iq 1 3)) => -1) ;2 < 3
         (check (cmp (iq 1 4) (iq 1 3)) => +1) ;4 > 3
         (check (cmp (iq 1 0) (iq 2 0)) => -1)
         (check (cmp (iq 3 0) (iq 2 0)) => +1)
     
         (check (cmp (iq ) (iq )) => 0)
         (check (cmp (iq ) (iq 1))        => -1)
         (check (cmp (iq 1) (iq ))        => +1)

         ;; If first items are equal: compare the CADRs.
         ;; Here one of the CADRs is null.

         (check (cmp (iq 1 2) (iq 1))        => +1)
         (check (cmp (iq 1)   (iq 1 2))        => -1)
     
         ;; Lists of different length, but it does not matter
         ;; because the CARs are non-equal.

         (check (cmp (iq 1 2) (iq 2))        => -1)
         (check (cmp (iq 2)   (iq 1 2))        => +1)
         #f)
     
       #t)
     
        (let ()
     
          ;; test-group "ipair-comparator"
     
       (define C
         (make-ipair-comparator exact-integer-comparator
                                real-comparator))
     
       ;; type test
       (test-assert  (comparator-test-type C (iq 1 . 2.0)))
       (test-assert  (comparator-test-type C (iq 1 . 2.0)))
       (test-deny (comparator-test-type C (iq )))
       (test-deny (comparator-test-type C (iq 1 . 2+1i)))
       (test-deny (comparator-test-type C "ciao"))
     
       ;; type check
       (test-assert  (comparator-check-type C (iq 1 . 2.0)))
       (test-assert
        (try
            (comparator-check-type C (void))
          (catch E
            ((&comparator-type-error)
             #t)
            (else E))))
     
       ;; comparison
       (check (comparator-compare C (iq 1 . 2.0) (iq 1 . 2.0))        => 0)
       (check (comparator-compare C (iq 1 . 2.0) (iq 1 . 3))          => -1)
       (check (comparator-compare C (iq 1 . 3)   (iq 1 . 2.0))        => +1)
     
       ;; hash
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 1 . 2.0))))
     
       #t)
     
        (let ()
     
          ;; test-group "icar-comparator"
     
       (define C
         (make-icar-comparator exact-integer-comparator))
     
       ;; type test
       (test-assert  (comparator-test-type C (iq 1 . 2.0)))
       (test-assert  (comparator-test-type C (iq 1 . 2.0)))
       (test-assert  (comparator-test-type C (iq 1 . 2+1i)))
       (test-deny (comparator-test-type C (iq 2.0 . 1)))
       (test-deny (comparator-test-type C (iq )))
       (test-deny (comparator-test-type C "ciao"))
     
       ;; type check
       (test-assert  (comparator-check-type C (iq 1 . 2.0)))
       (test-assert
        (try
            (comparator-check-type C (void))
          (catch E
            ((&comparator-type-error)
             #t)
            (else E))))
     
       ;; comparison
       (check (comparator-compare C (iq 1 . 2) (iq 1 . 3))        => 0)
       (check (comparator-compare C (iq 1 . 2) (iq 2 . 3))        => -1)
       (check (comparator-compare C (iq 2 . 2) (iq 1 . 2))        => +1)
     
       ;; hash
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 1 . 2.0))))
     
       #t)
     
        (let ()
     
          ;; test-group "icdr-comparator"
     
       (define C
         (make-icdr-comparator exact-integer-comparator))
     
       ;; type test
       (test-assert  (comparator-test-type C (iq 2.0 . 1)))
       (test-assert  (comparator-test-type C (iq 2.0 . 1)))
       (test-assert  (comparator-test-type C (iq 2+1i . 1)))
       (test-deny (comparator-test-type C (iq 1 . 2.0)))
       (test-deny (comparator-test-type C (iq )))
       (test-deny (comparator-test-type C "ciao"))
     
       ;; type check
       (test-assert  (comparator-check-type C (iq 2.0 . 1)))
       (test-assert
        (try
            (comparator-check-type C (void))
          (catch E
            ((&comparator-type-error)
             #t)
            (else E))))
     
       ;; comparison
       (check (comparator-compare C (iq 2 . 1) (iq 3 . 1))        => 0)
       (check (comparator-compare C (iq 2 . 1) (iq 3 . 2))        => -1)
       (check (comparator-compare C (iq 2 . 2) (iq 2 . 1))        => +1)
     
       ;; hash
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 2.0 . 1))))
     
       #t)
     
        (let ()
     
          ;; test-group "ilist-comparator"
     
       (define C
         (make-ilist-comparator exact-integer-comparator))
     
       ;; type test
       (test-assert  (comparator-test-type C (iq )))
       (test-assert  (comparator-test-type C (iq 1 2)))
       (test-deny (comparator-test-type C (iq 1 2 . 3)))
       (test-deny (comparator-test-type C (iq 1 2.0)))
       (test-deny (comparator-test-type C "ciao"))
       (test-deny (comparator-test-type C (iq 1+2i)))
     
       ;; type check
       (test-assert  (comparator-check-type C (iq 1 2)))
       (test-assert
        (try
            (comparator-check-type C (void))
          (catch E
            ((&comparator-type-error)
             #t)
            (else E))))
     
       ;; comparison
       (check (comparator-compare C (iq 1 2) (iq 1 2))        => 0)
       (check (comparator-compare C (iq 1 2) (iq 1 3))        => -1)
       (check (comparator-compare C (iq 1 3) (iq 1 2))        => +1)
     
       (check (comparator-compare C (iq )    (iq ))                => 0)
       (check (comparator-compare C (iq )    (iq 1 2))        => -1)
       (check (comparator-compare C (iq 1 2) (iq ))                => +1)
     
       ;; hash
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq ))))
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 1 2))))
     
       #t)
     
        (let ()
     
          ;; test-group "improper-ilist-comparator"
     
       (define C
         (let ()
           (define element-compare
             (let ((compare
                    (comparator-comparison-procedure exact-integer-comparator)))
               (lambda (A B)
                 (if (ipair? A)
                     (begin
                      (assert (ipair? B))
                              (let ((rv (compare (icar A) (icar B))))
                                (if (zero? rv)
                                    (comparator-compare C (icdr A) (icdr B))
                                                        rv)))
                                    (compare A B)))))
     
           (define E
             (make-comparator
              (lambda (x) #t)
              equal?
              element-compare
              (comparator-hash-function (make-default-comparator))))
     
           (define C
             (make-improper-ilist-comparator E))
     
           C))
     
       ;; type test
       (test-assert (comparator-test-type C (iq )))
       (test-assert (comparator-test-type C (iq 1 2)))
       (test-assert (comparator-test-type C (iq 1 2 . 3)))
       (test-assert (comparator-test-type C (iq 1 2.0)))
       (test-assert (comparator-test-type C "ciao"))
       (test-assert (comparator-test-type C (iq 1+2i)))
     
       ;; type check
       (test-assert (comparator-check-type C (iq 1 2)))
       (test-assert (comparator-check-type C (void)))
     
       ;; comparison
       (check (comparator-compare C (iq 1 2) (iq 1 2))        => 0)
       (check (comparator-compare C (iq 1 2) (iq 1 3))        => -1)
       (check (comparator-compare C (iq 1 3) (iq 1 2))        => +1)
     
       (check (comparator-compare C (iq )    (iq ))                => 0)
       (check (comparator-compare C (iq )    (iq 1 2))        => -1)
       (check (comparator-compare C (iq 1 2) (iq ))                => +1)
     
       (check (comparator-compare C (iq 1 2 . 3) (iq 1 2 . 3))  => 0)
       (check (comparator-compare C (iq 1 2 . 3) (iq 1 2 . 4))  => -1)
       (check (comparator-compare C (iq 1 2 . 4) (iq 1 2 . 3))  => +1)
     
       (check (comparator-compare C (iq 1 2 9 . 3) (iq 1 2 9 . 3)) => 0)
       (check (comparator-compare C (iq 1 2 9 . 3) (iq 1 2 9 . 4)) => -1)
       (check (comparator-compare C (iq 1 2 9 . 4) (iq 1 2 9 . 3)) => +1)
     
       ;; hash
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq ))))
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 1 2))))
       (test-assert
        (non-negative-exact-integer? (comparator-hash C (iq 1 2 . 3))))
       (test-assert
        (non-negative-exact-integer? (comparator-hash C "ciao")))
     
       #t)
     
       )))
