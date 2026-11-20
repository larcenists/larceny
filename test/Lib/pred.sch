; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Predicates.
; Number predicates are tested by number.sch, but we still need to test 
; many other things in this file:
;  - pair?
;  - null?
;  - boolean?
;  - symbol?
;  - structure?
;  - port?, input-port?, output-port?
;  - list?
;  - vector?
;  - char?
;  - number?
;  - procedure?

(define (run-predicate-tests)
  (display "Predicate") (newline)
  (test-equality-primitives-helper 0 1 1 2 'a 'b)
  (predicate-test-0)
  (equal-test)
  (symbol-predicate-test))

; equal? on circular, shared, long, and deep structures, where it
; switches between plain recursion and a union-find algorithm.

(define (equal-test)
  (define (circular n x)
    (let ((l (vector->list (make-vector n x))))
      (set-cdr! (list-tail l (- n 1)) l)
      l))
  (define (iota n)
    (do ((i (- n 1) (- i 1))
         (l '() (cons i l)))
        ((< i 0) l)))
  (define (shared-tree n)
    (if (= n 0)
        '()
        (let ((sub (shared-tree (- n 1))))
          (vector->list (make-vector n sub)))))
  (define (deep n)
    (do ((n n (- n 1))
         (t '() (cons t 'a)))
        ((= n 0) t)))
  (allof "equal?"
   (test "circular" (equal? (circular 1000 'a) (circular 1000 'a)) #t)
   (test "circular, shifted" (let ((c (circular 1000 'a))) (equal? c (cdr c)))
         #t)
   (test "circular, other length" (equal? (circular 3 'a) (circular 5 'a)) #t)
   (test "circular, different"
         (let ((c (circular 1000 'a))
               (d (circular 1000 'a)))
           (set-car! (list-tail d 777) 'b)
           (equal? c d))
         #f)
   (test "cycles of 2 and 3"
         (let ((c (list 1 2)) (d (list 1 2 1)))
           (set-cdr! (cdr c) c)
           (set-cdr! (cddr d) d)
           (equal? c d))
         #f)
   (test "cycle through car"
         (let ((x (list 1)) (y (list 1)))
           (set-car! x x)
           (set-car! y y)
           (equal? x y))
         #t)
   (test "cycle through a vector"
         (let ((v (vector 1 #f)) (w (vector 1 #f)))
           (vector-set! v 1 v)
           (vector-set! w 1 w)
           (equal? v w))
         #t)
   (test "cycles through vectors, different"
         (let ((v (vector 1 #f)) (w (vector 2 #f)))
           (vector-set! v 1 v)
           (vector-set! w 1 w)
           (equal? v w))
         #f)
   (test "shared tree" (equal? (shared-tree 60) (shared-tree 60)) #t)
   (test "shared tree, different" (equal? (shared-tree 60) (shared-tree 59))
         #f)
   (test "long list" (equal? (iota 300000) (iota 300000)) #t)
   (test "long list, different at the end"
         (let ((l (iota 300000)))
           (set-car! (list-tail l 299999) 'x)
           (equal? (iota 300000) l))
         #f)
   (test "deep tree" (equal? (deep 100000) (deep 100000)) #t)
   (test "deep tree, different" (equal? (deep 100000) (deep 99999)) #f)
   (test "strings and bytevectors"
         (equal? (list "abc" (vector "x" (make-bytevector 2 7)))
                 (list "abc" (vector "x" (make-bytevector 2 7))))
         #t)
   (test "numbers" (equal? (list 2 2.0 1/2) (list 2 2.0 1/2)) #t)
   (test "exactness" (equal? 2 2.0) #f)))

(define (test-equality-primitives-helper zero one xone two a b)
  (allof
   (test "(eq? a 'a)" (eq? a 'a) #t)
   (test "(eq? a b)" (eq? a b) #f)
   (test "(eq? one xone)" (eq? one xone) #t)
   (test "(eq? zero one)" (eq? zero one) #f)
   (test "(eqv? a 'a)" (eqv? a 'a) #t)
   (test "(eqv? a b)" (eqv? a b) #f)
   (test "(eqv? one xone)" (eqv? one xone) #t)
   (test "(eqv? zero one)" (eqv? zero one) #f)
   (test "(equal? a 'a)" (equal? a 'a) #t)
   (test "(equal? a b)" (equal? a b) #f)
   (test "(equal? xone one)" (equal? xone one) #t)
   (test "(equal? zero one)" (equal? zero one) #f)
   ))

; Tests eq? and eqv?, first time around (basic stuff, no numbers.)

(define (predicate-test-0)

  (define (e a b)
    (list (eq? a b) (eqv? a b)))

  (let ((a "string1")
	(b (lambda (x) x)))
    (allof
     (test "(e '() '())" (e '() '()) '(#t #t))
     (test "(e (vector) (vector))" (e (vector) (vector)) '(#f #t))
     (test "(e (string) (string))" (e (string) (string)) '(#f #t))
     (test "(e #t #f)" (e #t #f) '(#f #f))
     (test "(e a a)" (e a a) '(#t #t))
     (test "(e b b)" (e b b) '(#t #t))
     (test "(e 'foo 'foo)" (e 'foo 'foo) '(#t #t))
     (test "(e (lambda () 1) (lambda () 1))"
           (e (lambda () 1) (lambda () 1))
           '(#f #f))
     (test "(e (closure 0) (closure 0))"
           (let ((closure (lambda (n) (lambda () n))))
             (e (closure 0) (closure 0)))
           '(#f #f))
     (test "(e (closure 1) (closure 1))"
           (let* ((n (- (expt 3 3) 27))
                  (closure (lambda (ignored) (lambda () n))))
             (e (closure 1) (closure 1)))
           '(#f #f))
     )))

; Tests symbol=?

(define (symbol-predicate-test)

  (allof
   (test "(symbol=? 'a 'a)" (symbol=? 'a 'a) #t)
   (test "(symbol=? 'a 'a 'a)" (symbol=? 'a 'a 'a) #t)
   (test "(symbol=? 'a 'a 'b)" (symbol=? 'a 'a 'b) #f)
   (test "(symbol=? 'a 'b)" (symbol=? 'a 'b) #f)
   (test "(symbol=? 'a 'b 'a)" (symbol=? 'a 'b 'a) #f)
   (test "(symbol=? 'a 'b 'b)" (symbol=? 'a 'b 'b) #f)
   ))

; eof
