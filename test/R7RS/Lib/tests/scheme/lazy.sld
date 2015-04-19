;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme lazy) procedures and syntaxes:
;;;
;;;     delay
;;;     delay-force  (called "lazy" in SRFI 45)
;;;     force
;;;     make-promise
;;;     promise?


(define-library (tests scheme lazy)
  (export run-lazy-tests)
  (import (scheme base)
          (scheme lazy)
          (tests scheme test))

  (begin

   ;; The SRFI 45 tests use infinite loops to test for space leakage.
   ;; For automated testing, finite loops that run long enough to
   ;; reveal any O(n) space leaks should be good enough.  The following
   ;; loop count should be large enough to reveal space leaks but small
   ;; enough for the run time to be tolerable.

   (define alot 1000000)

   ;; Definitions used by the SRFI 45 tests.

   ;=========================================================================
   ; Convenient list deconstructor used below.
     
   (define-syntax match
     (syntax-rules ()
       ((match exp 
          (()      exp1)
          ((h . t) exp2))
        (let ((lst exp))
          (cond ((null? lst) exp1)
                ((pair? lst) (let ((h (car lst))
                                   (t (cdr lst)))
                               exp2))
                (else 'match-error))))))
     
   ;========================================================================
   ; Leak test 5: Naive stream-filter should run in bounded space.
   ;              Simplest case.
     
   (define (stream-filter p? s)
     (delay-force (match (force s)
                   (()      (delay '())) 
                   ((h . t) (if (p? h)
                                (delay (cons h (stream-filter p? t)))
                                (stream-filter p? t))))))
     
   ;========================================================================
   ; Leak test 6: Another long traversal should run in bounded space.
     
   ; The stream-ref procedure below does not strictly need to be lazy.  
   ; It is defined lazy for the purpose of testing safe compostion of 
   ; lazy procedures in the times3 benchmark below (previous 
   ; candidate solutions had failed this).  
     
   (define (stream-ref s index)
     (delay-force
      (match (force s)
        (()      'error)
        ((h . t) (if (zero? index)
                     (delay h)
                     (stream-ref t (- index 1)))))))
     
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define (run-lazy-tests)

     ;; From Racket R6RS tests/r6rs/r5rs.sls

     ;; ----------------------------------------

     (define a-stream
       (letrec ((next
                 (lambda (n)
                   (cons n (delay (next (+ n 1)))))))
         (next 0)))
     (define head car)
     (define tail
       (lambda (stream) (force (cdr stream))))

     (define count 0)
     (define p
       (delay (begin (set! count (+ count 1))
                     (if (> count x)
                         count
                         (force p)))))
     (define x 5)

     ;; From the R7RS (small) document.

     (test (force (delay (+ 1 2))) 3)

     (test (let ((p (delay (+ 1 2))))
             (list (force p) (force p)))
           '(3 3))

     (test (let ()
             (define integers
               (letrec ((next
                         (lambda (n)
                           (delay (cons n (next (+ n 1)))))))
                 (next 0)))
             (define head
               (lambda (stream) (car (force stream))))
             (define tail
               (lambda (stream) (cdr (force stream))))
             (head (tail (tail integers))))
           2)

     (test (let ()
             (define integers
               (letrec ((next
                         (lambda (n)
                           (delay (cons n (next (+ n 1)))))))
                 (next 0)))
             (define head
               (lambda (stream) (car (force stream))))
             (define tail
               (lambda (stream) (cdr (force stream))))
             (define (stream-filter p? s)
               (delay-force
                (if (null? (force s))
                    (delay '())
                    (let ((h (car (force s)))
                          (t (cdr (force s))))
                      (if (p? h)
                          (delay (cons h (stream-filter p? t)))
                          (stream-filter p? t))))))
             (head (tail (tail (stream-filter odd? integers)))))
           5)

     (test (let ()
             (define count 0)
             (define p
               (delay (begin (set! count (+ count 1))
                             (if (> count x)
                                 count
                                 (force p)))))
             (define x 5)
             (let* ((result1 (promise? p))
                    (result2 (force p))
                    (result3 (promise? p))
                    (result4 (begin (set! x 10) (force p))))
               (list result1 result2 result3 result4)))
           '(#t 6 #t 6))

     ;; The R7RS fails to say the expression given to delay-force
     ;; is supposed to evaluate to a promise.  I learned that from
     ;; reading Kawa's documentation. -- Will

     (test (promise? (make-promise 11)) #t)
     (test (promise? (delay 12)) #t)
     (test (promise? (delay-force (make-promise 13))) #t)

     (test (force (make-promise 14)) 14)
     (test (force (delay 15)) 15)
     (test (force (delay-force (make-promise 16))) 16)

     ;; ----------------------------------------

     (test (force (delay (+ 1 2)))    3)
    
     (test (let ((p (delay (+ 1 2))))
             (list (force p) (force p)))  
           '(3 3))
    
     (test (head (tail (tail a-stream))) 2)

     (test/unspec p)
     (test (force p) 6)
     (test/unspec p)
     (test (begin (set! x 10)
                  (force p))      
           6)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;
     ;; These tests are adapted from the tests in SRFI 45.
     ;;
     ;; I assume SRFI 45's lazy is the same as R7RS delay-force.
     ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;=========================================================================
     ; Memoization test 1:

     (test (let ((n 100))
             (define s (delay (begin (set! n (+ n 1)) 1)))
     
             (force s)
             (let ((result (force s)))
               (list result n)))
                    ;===> Should display 'hello once
                    ; or, in this modified version, should increment n once
           '(1 101))
     
     ;=========================================================================
     ; Memoization test 2:

     (test (let ((n 100))
     
             (let* ((s (delay (begin (set! n (+ n 1)) 2)))
                    (result (+ (force s) (force s))))
               (list result n)))
           '(4 101))
     
     ;=========================================================================
     ; Memoization test 3: (pointed out by Alejandro Forero Cuervo) 
     
     (test (let ((n 100))
             (define r (delay (begin (set! n (+ n 1)) 1)))
             (define s (delay-force r))
             (define t (delay-force s))
     
             (let ((result1 (force t))
                   (result2 (force r)))
               (list result1 result2 n)))
           '(1 1 101))
     
     ;=========================================================================
     ; Memoization test 4: Stream memoization 
     
     (test (let ((n 100))
             (define (stream-drop s index)
               (delay-force
                (if (zero? index)
                    s
                    (stream-drop (cdr (force s)) (- index 1)))))
     
             (define (ones)
               (delay (begin
                       (set! n (+ n 1))
                       (cons 1 (ones)))))
     
             (define s (ones))
     
             (let ((result1 (car (force (stream-drop s 4))))
                   (result2 (car (force (stream-drop s 4)))))
               (list result1 result2 n)))
           '(1 1 105))
     
     ;=========================================================================
     ; Reentrancy test 1: from R5RS
     
     (test (let ()
             (define count 0)
             (define p
               (delay (begin (set! count (+ count 1))
                             (if (> count x)
                                 count
                                 (force p)))))
             (define x 5)
             (let ((result1 (force p)))                      ;===>  6
               (set! x 10)
               (list result1 (force p))))                    ;===>  6
           '(6 6))            
     
     ;=========================================================================
     ; Reentrancy test 2: from SRFI 40
     
     (test (let ()
             (define f
               (let ((first? #t))
                 (delay
                   (if first?
                       (begin
                         (set! first? #f)
                         (force f))
                       'second))))
     
             (force f))                     ;===> 'second 
           'second)
     
     ;=========================================================================
     ; Reentrancy test 3: due to John Shutt
     
     (test (let ()
             (define q
               (let ((count 5))
                 (define (get-count) count)
                 (define p (delay (if (<= count 0)
                                      count
                                      (begin (set! count (- count 1))
                                             (force p)
                                             (set! count (+ count 2))
                                             count))))
                 (list get-count p)))
             (define get-count (car q))
             (define p (cadr q))
     
             (let* ((result1 (get-count))  ; =>   5
                    (result2 (force p))    ; =>   0
                    (result3 (get-count))) ; =>   10
               (list result1 result2 result3)))
           '(5 0 10))
     
     ;=========================================================================
     ; Test leaks:  All the leak tests should run in bounded space.
     
     ;=========================================================================
     ; Leak test 1: Infinite loop in bounded space.

     (test (let ()
             (define (loop n)
               (delay-force (if (> n 0)
                                (loop (- n 1))
                                (delay 'done))))
             (force (loop alot)))                  ;==> bounded space
           'done)
     
     ;=========================================================================
     ; Leak test 2: Pending memos should not accumulate 
     ;              in shared structures.
     
     (test (let ()
             (define (loop n)
               (if (> n 0)
                   (delay-force (loop (- n 1)))
                   (delay 'done)))
             (define s (loop alot))
             (force s))                            ;==> bounded space
           'done)
     
     ;=========================================================================
     ; Leak test 3: Safely traversing infinite stream.
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
     
             (define (traverse s k)
               (if (> k 0)
                   (delay-force (traverse (cdr (force s)) (- k 1)))
                   (delay 'done)))
     
             (force (traverse (from 0) alot)))     ;==> bounded space
           'done)
     
     ;=========================================================================
     ; Leak test 4: Safely traversing infinite stream 
     ;              while pointer to head of result exists.
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
     
             (define (traverse s k)
               (if (> k 0)
                   (delay-force (traverse (cdr (force s)) (- k 1)))
                   (make-promise 'done)))
     
             (define s (traverse (from 0) alot))  
             (force s))                            ;==> bounded space
           'done)
     
     ;=========================================================================
     ; Convenient list deconstructor used below.
#;     
     (define-syntax match
       (syntax-rules ()
         ((match exp 
            (()      exp1)
            ((h . t) exp2))
          (let ((lst exp))
            (cond ((null? lst) exp1)
                  ((pair? lst) (let ((h (car lst))
                                     (t (cdr lst)))
                                 exp2))
                  (else 'match-error))))))
     
     ;========================================================================
     ; Leak test 5: Naive stream-filter should run in bounded space.
     ;              Simplest case.
#;
     (define (stream-filter p? s)
       (delay-force (match (force s)
                     (()      (delay '())) 
                     ((h . t) (if (p? h)
                                  (delay (cons h (stream-filter p? t)))
                                  (stream-filter p? t))))))
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
             (car (force (stream-filter (lambda (n) (= n alot))
                                        (from 0)))))
                                                  ;==> bounded space
           alot)
     
     ;========================================================================
     ; Leak test 6: Another long traversal should run in bounded space.
     
     ; The stream-ref procedure below does not strictly need to be lazy.  
     ; It is defined lazy for the purpose of testing safe compostion of 
     ; lazy procedures in the times3 benchmark below (previous 
     ; candidate solutions had failed this).  
#;     
     (define (stream-ref s index)
       (delay-force
        (match (force s)
          (()      'error)
          ((h . t) (if (zero? index)
                       (delay h)
                       (stream-ref t (- index 1)))))))
     
     ; Check that evenness is correctly implemented - should terminate:
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
             (force (stream-ref (stream-filter zero? (from 0))
                                0)))                            ;==> 0
           0)           
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
             (define s (stream-ref (from 0) alot))
             (force s)                                  ;==> bounded space
             'done)
           'done)
     
     ;======================================================================
     ; Leak test 7: Infamous example from SRFI 40. 
     
     (test (let ()
             (define (from n)
               (delay (cons n (from (+ n 1)))))
             (define (times3 n)
               (stream-ref (stream-filter
                            (lambda (x) (zero? (modulo x n)))
                            (from 0))
                           3))
             (let ((result1 (force (times3 7)))
                   (result2 (force (times3 alot))))     ;==> bounded space
     
               (list result1 result2)))
           (list 21 (* 3 alot)))

     )))

