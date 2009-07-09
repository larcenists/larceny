; Test suite for SRFI 45
;
; $Id$

(cond-expand (srfi-45))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

;=========================================================================
; TESTS AND BENCHMARKS:
;=========================================================================

;=========================================================================
; Memoization test 1:

(define out (open-output-string))

(define s (delay (begin (display 'hello out) 1)))

(force s)
(force s)
               ;===> Should display 'hello once

(or (equal? "hello" (get-output-string out))
    (fail 'memo:1))

;=========================================================================
; Memoization test 2:

(define out (open-output-string))

(let ((s (delay (begin (display 'bonjour out) 2))))
  (+ (force s) (force s)))

               ;===> Should display 'bonjour once

(or (equal? "bonjour" (get-output-string out))
    (fail 'memo:2))

;=========================================================================
; Memoization test 3: (pointed out by Alejandro Forero Cuervo) 

(define out (open-output-string))

(define r (delay (begin (display 'hi out) 1)))
(define s (lazy r))
(define t (lazy s))

(force t)
(force r)
               ;===> Should display 'hi once

(or (equal? "hi" (get-output-string out))
    (fail 'memo:3))

;=========================================================================
; Memoization test 4: Stream memoization 

(define out (open-output-string))

(define (stream-drop s index)
  (lazy
   (if (zero? index)
       s
       (stream-drop (cdr (force s)) (- index 1)))))

(define (ones)
  (delay (begin
           (display 'ho out)
           (cons 1 (ones)))))

(define s (ones))

(car (force (stream-drop s 4)))
(car (force (stream-drop s 4)))

               ;===> Should display 'ho five times

(or (equal? "hohohohoho" (get-output-string out))
    (fail 'memo:4))

;=========================================================================
; Reentrancy test 1: from R5RS

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
(or (equal? 6 (force p))      ;===>  6
    (fail 'reentrancy:1a))
(set! x 10)
(or (equal? 6 (force p))      ;===>  6
    (fail 'reentrancy:1b))       

;=========================================================================
; Reentrancy test 2: from SRFI 40

(define f
  (let ((first? #t))
    (delay
      (if first?
          (begin
            (set! first? #f)
            (force f))
          'second))))

(or (equal? 'second (force f)) ;===> 'second 
    (fail 'reentrancy:2))

;=========================================================================
; Reentrancy test 3: due to John Shutt

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

(or (equal? 5 (get-count))  ; =>   5
    (fail 'reentrancy:3))
(or (equal? 0 (force p))    ; =>   0
    (fail 'reentrancy:3b))
(or (equal? 10 (get-count)) ; =>   10
    (fail 'reentrancy:3c))

;=========================================================================
; Test leaks:  All the leak tests should run in bounded space.

;=========================================================================
; Leak test 1: Infinite loop in bounded space.

(define (loop) (lazy (loop)))
;(force (loop))                               ;==> bounded space

;=========================================================================
; Leak test 2: Pending memos should not accumulate 
;              in shared structures.

(define s (loop))
;(force s)                                    ;==> bounded space 

;=========================================================================
; Leak test 3: Safely traversing infinite stream.

(define (from n)
  (delay (cons n (from (+ n 1)))))

(define (traverse s)
  (lazy (traverse (cdr (force s)))))

;(force (traverse (from 0)))                  ;==> bounded space

;=========================================================================
; Leak test 4: Safely traversing infinite stream 
;              while pointer to head of result exists.

(define s (traverse (from 0)))  
;(force s)                                    ;==> bounded space

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
  (lazy (match (force s)
          (()      (delay '())) 
          ((h . t) (if (p? h)
                       (delay (cons h (stream-filter p? t)))
                       (stream-filter p? t))))))

;(force (stream-filter (lambda (n) (= n 10000000000))
;                      (from 0)))
                                             ;==> bounded space

;========================================================================
; Leak test 6: Another long traversal should run in bounded space.

; The stream-ref procedure below does not strictly need to be lazy.  
; It is defined lazy for the purpose of testing safe compostion of 
; lazy procedures in the times3 benchmark below (previous 
; candidate solutions had failed this).  

(define (stream-ref s index)
  (lazy
   (match (force s)
     (()      'error)
     ((h . t) (if (zero? index)
                  (delay h)
                  (stream-ref t (- index 1)))))))

; Check that evenness is correctly implemented - should terminate:

(or (equal? 0 (force (stream-ref (stream-filter zero? (from 0))
                                 0)))                          ;==> 0
    (fail 'leak:6))

(define s (stream-ref (from 0) 100000000))
;(force s)                                          ;==> bounded space

;======================================================================
; Leak test 7: Infamous example from SRFI 40. 

(define (times3 n)
  (stream-ref (stream-filter
               (lambda (x) (zero? (modulo x n)))
               (from 0))
              3))

(force (times3 7))
;(force (times3 100000000))                        ;==> bounded space

(writeln "Done.")
