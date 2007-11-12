; Copyright 2007 William D Clinger
;
; $Id: hashtable.sch 5038 2007-10-30 20:30:38Z will $
;
; Stress tests for R6RS hashtables.

(define (run-hashtable-tests)
  (display "Hashtables") (newline)
  (hashtable-basic-tests)
  (hashtable-eq-tests 100 (lambda () (make-r6rs-hashtable object-hash eq?)))
  (hashtable-eq-tests 10000 make-eq-hashtable)
  (hashtable-eq-tests 10000 make-eqv-hashtable))

; This just calls every R6RS hashtable procedure
; (except for make-eq-hashtable and make-eqv-hashtable)
; at least once.

(define (hashtable-basic-tests)
  (let ((ht (make-r6rs-hashtable equal-hash equal?)))

    (allof "basic hashtable tests"

     (test "hashtable-hash-function"
           (hashtable-hash-function ht) equal-hash)
     (test "hashtable-equivalence-function"
           ((hashtable-equivalence-function ht) '(a b) (reverse '(b a)))
           #t)
     (test "hashtable-mutable? ht"
           (hashtable-mutable? ht) #t)

     (test "hashtable? (empty)" (hashtable? ht) #t)
     (test "hashtable-size (empty)" (hashtable-size ht) 0)
     (test "hashtable-ref (empty)" (hashtable-ref ht 'foo 16) 16)
     (test "hashtable-contains? (empty)" (hashtable-contains? ht 'foo) #f)
     (test "hashtable-delete! (empty)"
           (begin (hashtable-delete! ht 'foo) #t)
           #t)
     (test "hashtable-copy (empty)" (hashtable-size (hashtable-copy ht)) 0)
     (test "hashtable-clear! (empty)"
           (begin (hashtable-clear! ht)
                  (hashtable-size ht))
           0)
     (test "hashtable-keys (empty)" (hashtable-keys ht) '#())
     (test "hashtable-entries (empty)"
      (call-with-values
       (lambda () (hashtable-entries ht))
       (lambda (keys vals)
         (list (vector-length keys) (vector-length vals))))
      '(0 0))

     (test "dummy"
           (begin (hashtable-set! ht 'foo 21)
                  (hashtable-set! ht 'bar 22)
                  (hashtable-set! ht 'baz 3)
                  (hashtable-set! ht '(a) #\a)
                  (hashtable-set! ht '(b) #\b)
                  (hashtable-set! ht '(c) #\c)
                  (hashtable-update! ht 'baz (lambda (n) (+ n 20)) 1000)
                  #t)
           #t)

     (test "hashtable?" (hashtable? ht) #t)
     (test "hashtable-size" (hashtable-size ht) 6)
     (test "hashtable-ref" (hashtable-ref ht 'foo 16) 21)
     (test "hashtable-contains?" (hashtable-contains? ht 'baz) #t)
     (test "hashtable-contains?" (hashtable-contains? ht 'bzz) #f)
     (test "hashtable-delete! (empty)"
           (begin (hashtable-delete! ht '(a)) #t)
           #t)
     (test "hashtable-copy" (hashtable-ref (hashtable-copy ht) '(b) 55) #\b)
     (test "hashtable-keys"
           (let ((keys (vector->list (hashtable-keys ht))))
             (and (memq 'foo keys)
                  (memq 'bar keys)
                  (memq 'baz keys)
                  (not (member '(a) keys))
                  (member '(b) keys)
                  (member '(c) keys)
                  12345))
           12345)
     (test "hashtable-entries"
      (call-with-values
       (lambda () (hashtable-entries ht))
       (lambda (keys vals)
         (and (= 5 (vector-length keys))
              (= 5 (vector-length vals)))))
      #t)
     (test "hashtable-clear!"
           (begin (hashtable-clear! ht 100)
                  (hashtable-size ht))
           0))))

(define tested-table (make-eq-hashtable))

; The parameter is the number of items to be added to the table
; during the stress phase.

(define (hashtable-eq-tests n2 . rest)
  (call-with-current-continuation
   (lambda (exit)
     (let ((maker (if (null? rest) make-eq-hashtable (car rest)))
           (test (lambda (n passed?)
                   (if passed?
                       (test "hashtable-eq-test" passed? #t)
                       (test (string-append "hashtable-eq-test "
                                            (number->string n))
                             passed?
                             #t)))))

       (let ((t (maker))
             (not-found (list 'not-found))
             (x1 3.14159)
             (sym1 'sym1)
             (vec1 (vector 'vec1))
             (pair1 (list -1))
             (n1 1000)             ; population added in first phase
            ;(n2 10000)            ; population added in second phase
             (n3 1000))            ; population added in third phase

         (set! tested-table t)

         (test 1 (eq? not-found (hashtable-ref t x1 not-found)))
         (hashtable-set! t x1 'a)
         (test 2 (eq? 'a (hashtable-get t x1)))
         (hashtable-set! t sym1 'b)
         (test 3 (eq? 'a (hashtable-get t x1)))
         (test 4 (eq? 'b (hashtable-get t sym1)))
         (hashtable-set! t vec1 'c)
         (test 5 (eq? 'a (hashtable-get t x1)))
         (test 6 (eq? 'b (hashtable-get t sym1)))
         (test 7 (eq? 'c (hashtable-get t vec1)))
         (hashtable-set! t n2 'd)
         (test 8 (eq? 'a (hashtable-get t x1)))
         (test 9 (eq? 'b (hashtable-get t sym1)))
         (test 10 (eq? 'c (hashtable-get t vec1)))
         (test 11 (eq? 'd (hashtable-get t n2)))

         (hashtable-set! t pair1 'e)

         (do ((i 0 (+ i 1)))
             ((= i n1))
           (hashtable-set! t (list i) i))
         (test 12 (eq? 'e (hashtable-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n2))
           (if (and #f (zero? (mod i 1000))) (display "."))
           (hashtable-set! t (list i) i))
         (test 13 (eq? 'e (hashtable-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n3))
           (test 14 (eq? 'e (hashtable-get t pair1)))
           (hashtable-set! t (list i) i))
         (test 15 (eq? 'a (hashtable-get t x1)))
         (test 16 (eq? 'b (hashtable-get t sym1)))
         (test 17 (eq? 'c (hashtable-get t vec1)))
         (test 18 (eq? 'd (hashtable-get t n2)))
         (test 19 (eq? 'e (hashtable-get t pair1))))))))
