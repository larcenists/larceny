;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme list) procedures:
;;;
;;;     cons
;;;     list
;;;     xcons
;;;     cons*
;;;     make-list
;;;     list-tabulate
;;;     list-copy
;;;     circular-list
;;;     iota
;;;    
;;;     pair?
;;;     null?
;;;     proper-list?
;;;     circular-list?
;;;     dotted-list?
;;;     not-pair?
;;;     null-list?
;;;     list=
;;;    
;;;     car
;;;     cdr
;;;     caar
;;;     cadr
;;;     cdar
;;;     cddr
;;;     caaar
;;;     caadr
;;;     cadar
;;;     caddr
;;;     cdaar
;;;     cdadr
;;;     cddar
;;;     cdddr
;;;     caaaar
;;;     caaadr
;;;     caadar
;;;     caaddr
;;;     cadaar
;;;     cadadr
;;;     caddar
;;;     cadddr
;;;     cdaaar
;;;     cdaadr
;;;     cdadar
;;;     cdaddr
;;;     cddaar
;;;     cddadr
;;;     cdddar
;;;     cddddr
;;;     list-ref
;;;     first
;;;     second
;;;     third
;;;     fourth
;;;     fifth
;;;     sixth
;;;     seventh
;;;     eighth
;;;     ninth
;;;     tenth
;;;     car+cdr
;;;     take
;;;     drop
;;;     take-right
;;;     drop-right
;;;     take!
;;;     drop-right!
;;;     split-at
;;;     split-at!
;;;     last
;;;     last-pair
;;;    
;;;     length
;;;     length+
;;;     append
;;;     concatenate
;;;     reverse
;;;     append!
;;;     concatenate!
;;;     reverse!
;;;     append-reverse
;;;     append-reverse!
;;;     zip
;;;     unzip1
;;;     unzip2
;;;     unzip3
;;;     unzip4
;;;     unzip5
;;;     count
;;;    
;;;     map
;;;     for-each
;;;     fold
;;;     unfold
;;;     pair-fold
;;;     reduce
;;;     fold-right
;;;     unfold-right
;;;     pair-fold-right
;;;     reduce-right
;;;     append-map
;;;     append-map!
;;;     map!
;;;     pair-for-each
;;;     filter-map
;;;     map-in-order
;;;    
;;;     filter
;;;     partition
;;;     remove
;;;     filter!
;;;     partition!
;;;     remove!
;;;    
;;;     member
;;;     memq
;;;     memv
;;;     find
;;;     find-tail
;;;     any
;;;     every
;;;     list-index
;;;     take-while
;;;     drop-while
;;;     take-while!
;;;     span
;;;     break
;;;     span!
;;;     break!
;;;    
;;;     delete
;;;     delete-duplicates
;;;     delete!
;;;     delete-duplicates!
;;;    
;;;     assoc
;;;     assq
;;;     assv
;;;     alist-cons
;;;     alist-copy
;;;     alist-delete
;;;     alist-delete!
;;;    
;;;     lset<=
;;;     lset=
;;;     lset-adjoin
;;;     lset-union
;;;     lset-union!
;;;     lset-intersection
;;;     lset-intersection!
;;;     lset-difference
;;;     lset-difference!
;;;     lset-xor
;;;     lset-xor!
;;;     lset-diff+intersection
;;;     lset-diff+intersection!
;;;    
;;;     set-car!
;;;     set-cdr!

;;; Conflicts with (rnrs base):
;;;     map, for-each: allow list arguments of different lengths
;;; Conflicts with (rnrs lists):
;;;     member, assoc: accept an optional third argument
;;;     fold-right: different arguments, different semantics

;;; FIXME: Some of these tests are not portable because they require
;;; the procedures designated as "linear update" variants in the spec
;;; (eg append!) to side-effect their arguments.


(define-library (tests scheme list)
  (export run-list-tests)
  (import (scheme base)
          (scheme cxr)
          (scheme list)
          (tests scheme test))

  ;; Adapted from srfi-1-test.sps

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

   (define (run-list-tests)

     ;; Test cases are ordered as in the spec.  R5RS procedures are left out.
     
     (test (xcons 1 2) '(2 . 1))
     
     (test (cons* 1) 1)
     (test (cons* 1 2 3 4 5) '(1 2 3 4 . 5))
     
     (test (make-list 5 #t) '(#t #t #t #t #t))
     (test (make-list 0 #f) '())
     (test (length (make-list 3)) 3)
     
     (test (list-tabulate 5 (lambda (x) x)) '(0 1 2 3 4))
     (test (list-tabulate 0 (lambda (x) (error "FOO!"))) '())
     
     (test-assert (call-with-current-continuation
                   (lambda (abort)
                     (let* ((c  (list 1 2 3 4 5))
                            (cp (list-copy c)))
                       (or (equal? c cp)
                           (abort #f))
                       (let loop ((c c) (cp cp))
                         (if (not (null? c))
                             (begin
                               (or (not (eq? c cp))
                                   (abort #f))
                               (loop (cdr c) (cdr cp)))))
                       #t))))
     
     (test (list-copy '(1 2 3 . 4)) '(1 2 3 . 4))
     
     (test-assert (not (list? (circular-list 1 2 3))))
     (test-assert (let* ((a (list 'a))
                         (b (list 'b))
                         (c (list 'c))
                         (x (circular-list a b c)))
                    (and (eq? a (car x))
                         (eq? b (cadr x))
                         (eq? c (caddr x))
                         (eq? a (cadddr x)))))
     
     (test (iota 0) '())
     (test (iota 5 2 3) '(2 5 8 11 14))
     (test (iota 5 2) '(2 3 4 5 6))
     
     (test-assert (proper-list? '(1 2 3 4 5)))
     (test-assert (proper-list? '()))
     (test-assert (not (proper-list? '(1 2 . 3))))
     (test-assert (not (proper-list? (circular-list 1 2 3))))
     
     (test-assert (not (circular-list? '(1 2 3 4 5))))
     (test-assert (not (circular-list? '())))
     (test-assert (not (circular-list? '(1 2 . 3))))
     (test-assert (circular-list? (circular-list 1 2 3)))
     
     (test-assert (not (dotted-list? '(1 2 3 4 5))))
     (test-assert (not (dotted-list? '())))
     (test-assert (dotted-list? '(1 2 . 3)))
     (test-assert (not (dotted-list? (circular-list 1 2 3))))
     
     (test-assert (null-list? '()))
     (test-assert (not (null-list? '(1 2))))
     (test-assert (not (null-list? (circular-list 1 2))))
     
     (test-assert (not-pair? 1))
     (test-assert (not (not-pair? (cons 1 2))))
     
     (test-assert (list= = '(1 2 3) '(1 2 3) '(1 2 3)))
     (test-assert (not (list= = '(1 2 3) '(1 2 3) '(1 4 3))))
     ;; Checks that l0 is not being used when testing l2, cf spec
     (test-assert (list= (lambda (a b) (not (eq? a b)))
                         '(#f #f #f) '(#t #t #t) '(#f #f #f)))
     (test-assert (list= =))
     
     (test-assert (= (first '(1 2 3 4 5 6 7 8 9 10)) 1))
     (test-assert (= (second '(1 2 3 4 5 6 7 8 9 10)) 2))
     (test-assert (= (third '(1 2 3 4 5 6 7 8 9 10)) 3))
     (test-assert (= (fourth '(1 2 3 4 5 6 7 8 9 10)) 4))
     (test-assert (= (fifth '(1 2 3 4 5 6 7 8 9 10)) 5))
     (test-assert (= (sixth '(1 2 3 4 5 6 7 8 9 10)) 6))
     (test-assert (= (seventh '(1 2 3 4 5 6 7 8 9 10)) 7))
     (test-assert (= (eighth '(1 2 3 4 5 6 7 8 9 10)) 8))
     (test-assert (= (ninth '(1 2 3 4 5 6 7 8 9 10)) 9))
     (test-assert (= (tenth '(1 2 3 4 5 6 7 8 9 10)) 10))
     
     (let-values (((a b) (car+cdr '(1 . 2))))
       (test-assert (and (= a 1) (= b 2))))
     
     (test (take '(1 2 3 4 5 6) 3) '(1 2 3))
     (test (take '(1) 1) '(1))
     
     (test-assert (let ((x (list 1 2 3 4 5 6)))
                    (eq? (cdddr x) (drop x 3))))
     (test-assert (let ((x (list 1 2 3)))
                    (eq? x (drop x 0))))
     
     (test (take-right '(1 2 3 4 5 6) 3) '(4 5 6))
     (test-assert (null? (take-right '(1 2 3 4 5 6) 0)))
     (test (take-right '(1 2 3 . 4) 2) '(2 3 . 4))
     (test (take-right '(1 2 3 . 4) 0) 4)
     
     (test (drop-right '(1 2 3 4 5 6) 3) '(1 2 3))
     (test (drop-right '(1 2 3) 0) '(1 2 3))
     (test (drop-right '(1 2 3 . 4) 0) '(1 2 3))
     
     (test-assert (let ((x (list 1 2 3 4 5 6)))
                    (let ((y (take! x 3)))
                      (and (eq? x y)
                           (eq? (cdr x) (cdr y))
                           (eq? (cddr x) (cddr y))
                           (equal? y '(1 2 3))))))
     
     (test-assert (let ((x (list 1 2 3 4 5 6)))
                    (let ((y (drop-right! x 3)))
                      (and (eq? x y)
                           (eq? (cdr x) (cdr y))
                           (eq? (cddr x) (cddr y))
                           (equal? y '(1 2 3))))))
     
     (test-assert (let-values (((a b) (split-at '(1 2 3 4 5 6) 2)))
                    (and (equal? a '(1 2))
                         (equal? b '(3 4 5 6)))))
     
     (test-assert (let* ((x (list 1 2 3 4 5 6))
                         (y (cddr x)))
                    (let-values (((a b) (split-at! x 2)))
                      (and (equal? a '(1 2))
                           (eq? a x)
                           (equal? b '(3 4 5 6))
                           (eq? b y)))))
     
     (test-assert (eq? 37 (last '(1 2 3 37))))
     
     (test-assert (not (length+ (circular-list 1 2 3))))
     (test (length+ '(1 2 3 4)) 4)
     
     (test-assert (let ((x (list 1 2))
                        (y (list 3 4))
                        (z (list 5 6)))
                    (let ((r (append! x y '() z)))
                      (and (equal? r '(1 2 3 4 5 6))
                           (eq? r x)
                           (eq? (cdr r) (cdr x))
                           (eq? (cddr r) y)
                           (eq? (cdddr r) (cdr y))
                           (eq? (cddddr r) z)
                           (eq? (cdr (cddddr r)) (cdr z))))))
     
     (test (concatenate '((1 2 3) (4 5 6) () (7 8 9))) '(1 2 3 4 5 6 7 8 9))
     
     (test (concatenate! `(,(list 1 2 3) ,(list 4 5 6) () ,(list 7 8 9)))
           '(1 2 3 4 5 6 7 8 9))
     
     (test (append-reverse '(3 2 1) '(4 5 6)) '(1 2 3 4 5 6))
     
     (test (append-reverse! (list 3 2 1) (list 4 5 6)) '(1 2 3 4 5 6))
     
     (test (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
     (test (zip '() '() '() '()) '())
     (test (zip '(1) (circular-list 1 2)) '((1 1)))
     
     (test (unzip1 '((1) (2) (3) (4) (5))) '(1 2 3 4 5))
     
     (test-assert (let-values (((a b) (unzip2 '((10 11) (20 21) (30 31)))))
                    (and (equal? a '(10 20 30))
                         (equal? b '(11 21 31)))))
     
     (test-assert (let-values (((a b c)
                                (unzip3 '((10 11 12) (20 21 22) (30 31 32)))))
                    (and (equal? a '(10 20 30))
                         (equal? b '(11 21 31))
                         (equal? c '(12 22 32)))))
     
     (test-assert (let-values (((a b c d) (unzip4 '((10 11 12 13)
                                                    (20 21 22 23)
                                                    (30 31 32 33)))))
                    (and (equal? a '(10 20 30))
                         (equal? b '(11 21 31))
                         (equal? c '(12 22 32))
                         (equal? d '(13 23 33)))))
     
     (test-assert (let-values (((a b c d e) (unzip5 '((10 11 12 13 14)
                                                      (20 21 22 23 24)
                                                      (30 31 32 33 34)))))
                    (and (equal? a '(10 20 30))
                         (equal? b '(11 21 31))
                         (equal? c '(12 22 32))
                         (equal? d '(13 23 33))
                         (equal? e '(14 24 34)))))
     
     (test (count even? '(3 1 4 1 5 9 2 5 6)) 3)
     (test (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)) 3)
     (test (count < '(3 1 4 1) (circular-list 1 10)) 2)
     
     (test (fold cons* '() '(a b c) '(1 2 3 4 5)) '(c 3 b 2 a 1))
     
     (test (fold-right cons* '() '(a b c) '(1 2 3 4 5)) '(a 1 b 2 c 3))
     
     (test-assert (let* ((x (list 1 2 3))
                         (r (list x (cdr x) (cddr x)))
                         (y (pair-fold (lambda (pair tail) 
                                         (set-cdr! pair tail) pair) 
                                       '()
                                       x)))
                    (and (equal? y '(3 2 1))
                         (every (lambda (c) (memq c r))
                                (list y (cdr y) (cddr y)))
                         #t)))
     
     (test (pair-fold-right cons '() '(a b c)) '((a b c) (b c) (c)))
     
     (test (reduce max 'illegal '(1 2 3 4 5)) 5)
     (test (reduce max 0 '()) 0)
     
     (test (reduce-right append 'illegal '((1 2) () (3 4 5))) '(1 2 3 4 5))
     
     (test (unfold (lambda (x) (> x 10))
                   (lambda (x) (* x x))
                   (lambda (x) (+ x 1))
                   1)
           '(1 4 9 16 25 36 49 64 81 100))
     
     (test (unfold-right zero? 
                         (lambda (x) (* x x))
                         (lambda (x) (- x 1))
                         10)
           '(1 4 9 16 25 36 49 64 81 100))
     
     (test (map + '(3 1 4 1) (circular-list 1 0)) '(4 1 5 1))
     
     (test (let ((v 1)
                 (l '()))
             (for-each (lambda (x y)
                         (let ((n v))
                           (set! v (+ v 1))
                           (set! l (cons n l))))
                       '(0 0 0 0 0)
                       (circular-list 1 2))
             l)
           '(5 4 3 2 1))
          
     
     (test (append-map (lambda (x) (list x (- x))) '(1 3 8))
           '(1 -1 3 -3 8 -8))
     
     (test (append-map! (lambda (x) (list x (- x))) '(1 3 8))
           '(1 -1 3 -3 8 -8))
     
     (test-assert (let* ((l (list 1 2 3))
                         (m (map! (lambda (x) (* x x)) l)))
                    (and (equal? m '(1 4 9))
                         (equal? l '(1 4 9)))))
     
     (test (let ((v 1))
             (map-in-order (lambda (x)
                             (let ((n v))
                               (set! v (+ v 1))
                               n))
                           '(0 0 0 0 0)))
           '(1 2 3 4 5))
     
     (test (let ((xs (list 1 2 3))
                 (l '()))
             (pair-for-each (lambda (x) (set! l (cons x l))) xs)
             l)
           '((3) (2 3) (1 2 3)))
     
     (test (filter-map (lambda (x y) (and (number? x) (* x x))) 
                       '(a 1 b 3 c 7)
                       (circular-list 1 2))
           '(1 9 49))
     
     (test (filter even? '(0 7 8 8 43 -4)) '(0 8 8 -4))
     
     (test-assert (let-values (((a b)
                                (partition symbol? '(one 2 3 four five 6))))
                    (and (equal? a '(one four five))
                         (equal? b '(2 3 6)))))
     
     (test (remove even? '(0 7 8 8 43 -4)) '(7 43))
     
     (test-assert (let* ((x (list 0 7 8 8 43 -4))
                         (y (pair-fold cons '() x))
                         (r (filter! even? x)))
                    (and (equal? '(0 8 8 -4) r)
                         (every (lambda (c) (memq c y))
                                (pair-fold cons '() r))
                         #t)))
     
     (test-assert (let* ((x (list 'one 2 3 'four 'five 6))
                         (y (pair-fold cons '() x)))
                    (let-values (((a b) (partition! symbol? x)))
                      (and (equal? a '(one four five))
                           (equal? b '(2 3 6))
                           (every (lambda (c) (memq c y))
                                  (pair-fold cons '() a))
                           (every (lambda (c) (memq c y))
                                  (pair-fold cons '() b))
                           #t))))
     
     (test-assert (let* ((x (list 0 7 8 8 43 -4))
                         (y (pair-fold cons '() x))
                         (r (remove! even? x)))
                    (and (equal? '(7 43) r)
                         (every (lambda (c) (memq c y))
                                (pair-fold cons '() r))
                         #t)))
     
     (test (find even? '(3 1 4 1 5 9 8)) 4)
     
     (test (find-tail even? '(3 1 4 1 5 9 8)) '(4 1 5 9 8))
     (test (find-tail even? '(1 3 5 7)) '#f)
     
     (test (take-while even? '(2 18 3 10 22 9)) '(2 18))
     
     (test-assert (let* ((x (list 2 18 3 10 22 9))
                         (r (take-while! even? x)))
                    (and (equal? r '(2 18))
                         (eq? r x)
                         (eq? (cdr r) (cdr x)))))
     
     (test (drop-while even? '(2 18 3 10 22 9)) '(3 10 22 9))
     
     (test-assert (let-values (((a b) (span even? '(2 18 3 10 22 9))))
                    (and (equal? a '(2 18))
                         (equal? b '(3 10 22 9)))))
     
     (test-assert (let-values (((a b) (break even? '(3 1 4 1 5 9))))
                    (and (equal? a '(3 1))
                         (equal? b '(4 1 5 9)))))
     
     (test-assert (let* ((x     (list 2 18 3 10 22 9))
                         (cells (pair-fold cons '() x)))
                    (let-values (((a b) (span! even? x)))
                      (and (equal? a '(2 18))
                           (equal? b '(3 10 22 9))
                           (every (lambda (x) (memq x cells))
                                  (pair-fold cons '() a))
                           (every (lambda (x) (memq x cells))
                                  (pair-fold cons '() b))
                           #t))))
     
     (test-assert (let* ((x     (list 3 1 4 1 5 9))
                         (cells (pair-fold cons '() x)))
                    (let-values (((a b) (break! even? x)))
                      (and (equal? a '(3 1))
                           (equal? b '(4 1 5 9))
                           (every (lambda (x) (memq x cells))
                                  (pair-fold cons '() a))
                           (every (lambda (x) (memq x cells))
                                  (pair-fold cons '() b))
                           #t))))
     
     (test-assert (any integer? '(a 3 b 2.7)))
     (test-assert (not (any integer? '(a 3.1 b 2.7))))
     (test-assert (any < '(3 1 4 1 5) (circular-list 2 7 1 8 2)))
     (test (any (lambda (a b) (if (< a b) 'yes #f))
                  '(1 2 3) '(0 1 4))
           'yes)
     
     (test-assert (every integer? '(1 2 3)))
     (test-assert (not (every integer? '(3 4 5.1))))
     (test-assert (every < '(1 2 3) (circular-list 2 3 4)))
     
     (test (list-index even? '(3 1 4 1 5 9)) 2)
     (test (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) 1)
     (test-assert (not (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))
     
     (test (member 5 '(1 2 5 37 48) <) '(37 48))
     
     (test (delete 5 '(1 48 2 5 37) <) '(1 2 5))
     (test (delete 5 '(1 5 2 5 7)) '(1 2 7))
     
     (test-assert (let* ((x     (list 1 48 2 5 37))
                         (cells (pair-fold cons '() x))
                         (r     (delete! 5 x <)))
                    (and (equal? r '(1 2 5))
                         (every (lambda (x) (memq x cells))
                                (pair-fold cons '() r))
                         #t)))
     
     (test (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                              (lambda (x y) (eq? (car x) (car y))))
           '((a . 3) (b . 7) (c . 1)))
     (test (delete-duplicates '(a b a c a b c z) eq?) '(a b c z))
     
     (test-assert (let* ((x     (list 'a 'b 'a 'c 'a 'b 'c 'z))
                         (cells (pair-fold cons '() x))
                         (r     (delete-duplicates! x)))
                    (and (equal? '(a b c z) r)
                         (every (lambda (x) (memq x cells))
                                (pair-fold cons '() r))
                         #t)))
     
     (test (assoc 6 
                  '((4 . #t) (3 . #t) (5 . #t))
                  (lambda (x y)
                    (zero? (remainder x y))))
           '(3 . #t))
     
     (test (alist-cons 1 #t '((2 . #f))) '((1 . #t) (2 . #f)))
     
     (test-assert (let* ((a (list (cons 1 2) (cons 3 4)))
                         (b (alist-copy a)))
                    (and (equal? a b)
                         (every (lambda (x) (not (memq x b))) a)
                         (every (lambda (x) (not (memq x a))) b))))
     
     (test (alist-delete 5 '((1 . #t) (2 . #t) (37 . #t) (4 . #t) (48 #t)) <)
           '((1 . #t) (2 . #t) (4 . #t)))
     (test (alist-delete 7 '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 #t)))
           '((1 . #t) (2 . #t) (4 . #t)))
     
     (test-assert (let* ((x '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 #t)))
                         (y (list-copy x))
                         (cells (pair-fold cons '() x))
                         (r (alist-delete! 7 x)))
                    (and (equal? r '((1 . #t) (2 . #t) (4 . #t)))
                         (every (lambda (x) (memq x cells))
                                (pair-fold cons '() r))
                         (every (lambda (x) (memq x y)) r)
                         #t)))
     
     (test-assert (lset<= eq? '(a) '(a b a) '(a b c c)))
     (test-assert (not (lset<= eq? '(a) '(a b a) '(a))))
     (test-assert (lset<= eq?))
     (test-assert (lset<= eq? '(a)))
     
     (test-assert (lset= eq? '(b e a) '(a e b) '(e e b a)))
     (test-assert (not (lset= eq? '(b e a) '(a e b) '(e e b a c))))
     (test-assert (lset= eq?))
     (test-assert (lset= eq? '(a)))
     
     (test (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)
           '(u o i a b c d c e))
     
     (test (lset-union eq? '(a b c d e) '(a e i o u))
           '(u o i a b c d e))
     (test (lset-union eq? '(a a c) '(x a x)) '(x a a c))
     (test-assert (null? (lset-union eq?)))
     (test (lset-union eq? '(a b c)) '(a b c))
     
     (test (lset-intersection eq? '(a b c d e) '(a e i o u)) '(a e))
     (test (lset-intersection eq? '(a x y a) '(x a x z)) '(a x a))
     (test (lset-intersection eq? '(a b c)) '(a b c))
     
     (test (lset-difference eq? '(a b c d e) '(a e i o u)) '(b c d))
     (test (lset-difference eq? '(a b c)) '(a b c))
     
     (test-assert (lset= eq?
                         '(d c b i o u)
                         (lset-xor eq? '(a b c d e) '(a e i o u))))
     (test-assert (lset= eq? '() (lset-xor eq?)))
     (test-assert (lset= eq? '(a b c d e) (lset-xor eq? '(a b c d e))))
     
     (test-assert (let-values (((d i)
                                (lset-diff+intersection eq?
                                                        '(a b c d e)
                                                        '(c d f))))
                    (and (equal? d '(a b e))
                         (equal? i '(c d)))))
     
     ;; FIXME: For the following five procedures, need to check that cells
     ;; returned are from the arguments.
     
     (test (lset-union! eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))
           '(u o i a b c d e))
     (test (lset-union! eq? (list 'a 'a 'c) (list 'x 'a 'x)) '(x a a c))
     (test-assert (null? (lset-union! eq?)))
     (test (lset-union! eq? (list 'a 'b 'c)) '(a b c))
     
     (test (lset-intersection! eq?
                               (list 'a 'b 'c 'd 'e) 
                               (list 'a 'e 'i 'o 'u))
           '(a e))
     (test (lset-intersection! eq?
                               (list 'a 'x 'y 'a) 
                               (list 'x 'a 'x 'z))
           '(a x a))
     (test (lset-intersection! eq? (list 'a 'b 'c)) '(a b c))
     
     (test (lset-difference! eq?
                             (list 'a 'b 'c 'd 'e)
                             (list 'a 'e 'i 'o 'u))
           '(b c d))
     (test (lset-difference! eq? (list 'a 'b 'c)) '(a b c))
     
     (test-assert (lset= eq?
                         '(d c b i o u)
                         (lset-xor! eq?
                                    (list 'a 'b 'c 'd 'e)
                                    (list 'a 'e 'i 'o 'u))))
     (test-assert (lset= eq? '() (lset-xor! eq?)))
     (test-assert (lset= eq?
                         '(a b c d e)
                         (lset-xor! eq? (list 'a 'b 'c 'd 'e))))
     
     (test-assert (let-values (((d i)
                                (lset-diff+intersection! eq?
                                                         (list 'a 'b 'c 'd 'e)
                                                         (list 'c 'd 'f))))
                    (and (equal? d '(a b e))
                         (equal? i '(c d)))))

     )))
