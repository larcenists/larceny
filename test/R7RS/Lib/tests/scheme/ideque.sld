;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme ideque) procedures:
;;;
;;;     ideque
;;;     ideque-tabulate
;;;     ideque-unfold
;;;     ideque-unfold-right 
;;;     ideque?
;;;     ideque-empty?
;;;     ideque=
;;;     ideque-any
;;;     ideque-every
;;;
;;;     ideque-front
;;;     ideque-add-front
;;;     ideque-remove-front
;;;     ideque-back
;;;     ideque-add-back
;;;     ideque-remove-back
;;;
;;;     ideque-ref
;;;     ideque-take
;;;     ideque-take-right
;;;     ideque-drop
;;;     ideque-drop-right
;;;     ideque-split-at
;;;
;;;     ideque-length
;;;     ideque-append
;;;     ideque-reverse
;;;     ideque-count
;;;     ideque-zip
;;;
;;;     ideque-map
;;;     ideque-filter-map
;;;     ideque-for-each
;;;     ideque-for-each-right
;;;     ideque-fold
;;;     ideque-fold-right
;;;     ideque-append-map
;;;
;;;     ideque-filter
;;;     ideque-remove
;;;     ideque-partition
;;;
;;;     ideque-find
;;;     ideque-find-right
;;;     ideque-take-while
;;;     ideque-take-while-right
;;;     ideque-drop-while
;;;     ideque-drop-while-right
;;;     ideque-span ideque-break
;;;
;;;     list->ideque
;;;     ideque->list
;;;     generator->ideque
;;;     ideque->generator

(define-library (tests scheme ideque)
  (export run-ideque-tests)
  (import (scheme base)
          (scheme ideque)
          (tests scheme test)
          (srfi 8)                ; receive
          (scheme char)
          (scheme generator)
          (scheme list))

  ;; Adapted from srfi-134-test.sps

  ;; Requires srfi-1 and srfi-121 (generators)

  (begin

   (define (run-ideque-tests)

     (define-syntax test-assert
       (syntax-rules ()
        ((test-assert expr)
         (test expr #t))))

     ;; test-group "ideque"

     ;; test-group "ideque/constructors"

     (test (ideque->list (ideque)) '())
     (test (ideque->list (list->ideque '())) '())
     (test (ideque->list (ideque 1 2 3)) '(1 2 3))
     (test (ideque->list (list->ideque '(4 5 6 7))) '(4 5 6 7))
     (test (ideque->list (ideque-unfold zero? values (lambda (n) (- n 1)) 10))
           '(10 9 8 7 6 5 4 3 2 1))
     (test (ideque->list
            (ideque-unfold-right zero? values (lambda (n) (- n 1)) 10))
           '(1 2 3 4 5 6 7 8 9 10))
     (test (ideque->list (ideque-tabulate 6 (lambda (n) (* n 2))))
           '(0 2 4 6 8 10))

     ;; corner cases

     (test (ideque->list
            (ideque-unfold (lambda (n) #t) values (lambda (n) (+ n 1)) 0))
           '())
     (test (ideque->list
            (ideque-unfold-right (lambda (n) #t) values (lambda (n) (+ n 1)) 0))
           '())
     (test (ideque->list (ideque-tabulate 0 values)) '())

     ;; test-group "ideque/predicates"

     (test-assert (ideque? (ideque)))
     (test-assert (not (ideque? 1)))
     (test-assert (ideque-empty? (ideque)))
     (test-assert (not (ideque-empty? (ideque 1))))
     (test-assert (ideque= eq?))
     (test-assert (ideque= eq? (ideque 1)))
     (test-assert (ideque= char-ci=? (ideque #\a #\b) (ideque #\A #\B)))
     (test-assert (ideque= char-ci=? (ideque) (ideque)))
     (test-assert (not (ideque= char-ci=? (ideque #\a #\b)
                                          (ideque #\A #\B #\c))))
     (test-assert (not (ideque= char-ci=? (ideque #\a #\b) (ideque #\A))))
     (test-assert (ideque= char-ci=? (ideque) (ideque) (ideque)))
     (test-assert (ideque= char-ci=?
                           (ideque #\a #\b)
                           (ideque #\A #\B)
                           (ideque #\a #\B)))
     (test-assert (not (ideque= char-ci=?
                                (ideque #\a #\b)
                                (ideque #\A)
                                (ideque #\a #\B))))
     (test-assert (not (ideque= char-ci=?
                                (ideque #\a #\b)
                                (ideque #\A #\B)
                                (ideque #\A #\B #\c))))


     ;; test-group "ideque/queue-operations"

     (test/unspec-or-exn (ideque-front (ideque)) &error)
     (test/unspec-or-exn (ideque-back (ideque)) &error)
     (test (ideque-front (ideque 1 2 3)) 1)
     (test (ideque-back (ideque 1 2 3)) 3)
     (test (ideque-front (ideque-remove-front (ideque 1 2 3))) 2)
     (test (ideque-back (ideque-remove-back (ideque 1 2 3))) 2)
     (test (ideque-front (ideque-remove-back (ideque 1 2 3))) 1)
     (test (ideque-back (ideque-remove-front (ideque 1 2 3))) 3)
     (test-assert (ideque-empty? (ideque-remove-front (ideque 1))))
     (test-assert (ideque-empty? (ideque-remove-back (ideque 1))))
     (test (ideque-front (ideque-add-front (ideque 1 2 3) 0)) 0)
     (test (ideque-back (ideque-add-back (ideque 1 2 3) 0)) 0)

     ;; test-group "ideque/other-accessors"

     (let ()
       (define (check name ideque-op list-op n)
         (let* ((lis (iota n))
                (dq (list->ideque lis)))
           (for-each (lambda (i)
                       (test (receive xs
                                      (ideque-op dq i)
                                      (map ideque->list xs))
                             (receive xs (list-op lis i) xs)))
                     lis)))
       (check 'ideque-take ideque-take take 7)
       (check 'ideque-drop ideque-drop drop 6)
       (check 'ideque-split-at ideque-split-at split-at 8))

     ;; out-of-range conditions

     (test/unspec-or-exn (ideque->list
                          (ideque-take (ideque 1 2 3 4 5 6 7) 10))
                         &error)
     (test/unspec-or-exn (ideque->list
                          (ideque-take-right (ideque 1 2 3 4 5 6 7) 10))
                         &error)
     (test/unspec-or-exn (ideque-split-at (ideque 1 2 3 4 5 6 7) 10)
                         &error)

     (test (map (lambda (n) (ideque-ref (ideque 3 2 1) n)) '(0 1 2))
           '(3 2 1))
     (test/unspec-or-exn (ideque-ref (ideque 3 2 1) -1) &error)
     (test/unspec-or-exn (ideque-ref (ideque 3 2 1) 3) &error)

     ;; test-group "ideque/whole-ideque"

     (test (ideque-length (ideque 1 2 3 4 5 6 7)) 7)
     (test (ideque-length (ideque)) 0)
     (test (ideque->list (ideque-append)) '())
     (test (ideque->list (ideque-append (ideque) (ideque))) '())
     (test (ideque->list (ideque-append (ideque 1 2 3)
                                        (ideque 'a 'b 'c 'd)
                                        (ideque)
                                        (ideque 5 6 7 8 9)))
          '(1 2 3 a b c d 5 6 7 8 9))
     (test (ideque->list (ideque-reverse (ideque))) '())
     (test (ideque->list (ideque-reverse (ideque 1 2 3 4 5))) '(5 4 3 2 1))
     (test (ideque-count odd? (ideque)) 0)
     (test (ideque-count odd? (ideque 1 2 3 4 5)) 3)
     (test (ideque->list (ideque-zip (ideque 1 2 3) (ideque 'a 'b 'c 'd 'e)))
          '((1 a) (2 b) (3 c)))
     (test (ideque->list (ideque-zip (ideque 1 2 3 4 5)
                                     (ideque 'a 'b 'c 'd 'e)
                                     (ideque 'x 'y 'z)))
          '((1 a x) (2 b y) (3 c z)))
     (test (ideque->list (ideque-zip (ideque 1 2 3)))
          '((1) (2) (3)))
     (test (ideque->list (ideque-zip (ideque 1 2 3) (ideque))) '())

     ;; test-group "ideque/mapping"

     (test-assert (ideque-empty? (ideque-map list (ideque))))
     (test (ideque->list (ideque-map - (ideque 1 2 3 4 5)))
           '(-1 -2 -3 -4 -5))
     (test (ideque->list (ideque-filter-map (lambda (x) (and (number? x) (- x)))
                                            (ideque 1 3 'a -5 8)))
           '(-1 -3 5 -8))
     (test (let ((r '()))
             (ideque-for-each (lambda (n) (set! r (cons n r)))
                              (ideque 1 2 3 4 5))
             r)
           '(5 4 3 2 1))
     (test (let ((r '()))
             (ideque-for-each-right (lambda (n) (set! r (cons n r)))
                                    (ideque 1 2 3 4 5))
             r)
           '(1 2 3 4 5))
     (test (ideque-fold cons 'z (ideque 1 2 3 4 5))
           '(5 4 3 2 1 . z))
     (test (ideque-fold-right cons 'z (ideque 1 2 3 4 5))
           '(1 2 3 4 5 . z))
     (test (ideque->list (ideque-append-map (lambda (x) (list x x))
                                            (ideque 'a 'b 'c)))
           '(a a b b c c))

     ;; test-group "ideque/filtering"

     (test (ideque->list (ideque-filter odd? (ideque 1 2 3 4 5)))
           '(1 3 5))
     (test (ideque->list (ideque-remove odd? (ideque 1 2 3 4 5)))
           '(2 4))
     (test (receive xs
                    (ideque-partition odd? (ideque 1 2 3 4 5))
                    (map ideque->list xs))
           '((1 3 5) (2 4)))

     ;; test-group "ideque/searching"

     (test (ideque-find number? (ideque 'a 3 'b 'c 4 'd) (lambda () 'boo))
           3)
     (test (ideque-find number? (ideque 'a 'b 'c 'd) (lambda () 'boo))
           'boo)
     (test (ideque-find number? (ideque 'a 'b 'c 'd))
           #f)
     (test (ideque-find-right number?
                              (ideque 'a 3 'b 'c 4 'd)
                              (lambda () 'boo))
           4)
     (test (ideque-find-right number? (ideque 'a 'b 'c 'd) (lambda () 'boo))
           'boo)
     (test (ideque-find-right number? (ideque 'a 'b 'c 'd)) #f)
     (test (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                            (ideque 1 3 2 5 8 4 6 3 4 2)))
           '(1 3 2))
     (test (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                            (ideque 1 3 2 5 8 4 6 3 4 2)))
           '(5 8 4 6 3 4 2))
     (test (ideque->list (ideque-take-while-right
                          (lambda (n) (< n 5))
                          (ideque 1 3 2 5 8 4 6 3 4 2)))
           '(3 4 2))
     (test (ideque->list (ideque-drop-while-right
                          (lambda (n) (< n 5))
                          (ideque 1 3 2 5 8 4 6 3 4 2)))
           '(1 3 2 5 8 4 6))
     (test (ideque->list (ideque-take-while (lambda (n) (< n 5))
                                            (ideque 5 8 4 6 3 4 2 9)))
           '())
     (test (ideque->list (ideque-drop-while (lambda (n) (< n 5))
                                            (ideque 1 4 3 2 3 4 2 1)))
           '())
     (test (ideque->list (ideque-take-while-right (lambda (n) (< n 5))
                                                  (ideque 5 8 4 6 3 4 2 9)))
           '())
     (test (ideque->list (ideque-drop-while-right (lambda (n) (< n 5))
                                                  (ideque 1 3 2 4 3 2 3 2)))
           '())
     (test (receive xs
                    (ideque-span (lambda (n) (< n 5))
                                 (ideque 1 3 2 5 8 4 6 3 4 2))
                    (map ideque->list xs))
           '((1 3 2) (5 8 4 6 3 4 2)))
     (test (receive xs
                    (ideque-break (lambda (n) (< n 5))
                                    (ideque 5 8 4 6 3 4 2 9))
                    (map ideque->list xs))
           '((5 8) (4 6 3 4 2 9)))
     (test (ideque-any (lambda (x) (and (number? x) x))
                       (ideque 'a 3 'b 'c 4 'd 'e))
           3)
     (test (ideque-any (lambda (x) (and (number? x) x))
                       (ideque 'a 'b 'c 'd 'e 5))
           5)
     (test (ideque-any (lambda (x) (and (number? x) x))
                        (ideque 'a 'b 'c 'd 'e))
           #f)
     (test (ideque-every (lambda (x) (and (number? x) x))
                         (ideque 1 5 3 2 9))
           9)
     (test (ideque-every (lambda (x) (and (number? x) x))
                         (ideque 1 5 'a 2 9))
           #f)

     ;; check if we won't see further once we found the result

     (test (ideque-any (lambda (x) (and (odd? x) x))
                       (ideque 2 1 'a 'b 'c 'd))
           1)
     (test (ideque-every (lambda (x) (and (odd? x) x))
                         (ideque 1 2 'a 'b 'c 'd))
           #f)

     (test (generator->list (ideque->generator (ideque 1 2 3))) '(1 2 3))
     (test (generator->list (ideque->generator (ideque))) '())
     (test (ideque->list (generator->ideque (generator 1 2 3))) '(1 2 3))
     (test (ideque->list (generator->ideque (generator))) '()))))
