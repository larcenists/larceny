;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         permsort.sch
; Description:  destructive sort of a generated list of permutations
; Author:       Lars Hansen, Will Clinger, and Gene Luks
; Created:      18-Mar-94
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 940720 / lth Added some more benchmarks for the thesis paper.

; This benchmark is in four parts.  Each tests a different aspect of
; the memory system.
;
;    perm8            storage allocation
;    10perm8          storage allocation and garbage collection
;    sumperms         sequential traversal
;    mergesort        side effects
;
; The perm8 benchmark generates a list of all 40320 permutations of
; the first 8 integers, allocating 149912 pairs (typically 1199296 bytes),
; all of which goes into the generated list.  (That is, the perm8
; benchmark generates absolutely no garbage.)  The generated permutations
; are in order of a grey code that bears no obvious relationship to a
; lexicographic order.
;
; The sumperms benchmark computes the sum of the permuted integers
; over all permutations.
;
; The 10perm8 benchmark repeats the perm8 benchmark 10 times, so it
; allocates and reclaims 1499120 pairs (typically 11,992,960 bytes).
;
; The mergesort! benchmark destructively sorts the generated permutations
; into lexicographic order, allocating no storage whatsoever.
 
; Date: Thu, 17 Mar 94 19:43:32 -0800
; From: luks@sisters.cs.uoregon.edu
; To: will
; Subject: Pancake flips
; 
; Procedure P_n generates a grey code of all perms of n elements
; on top of stack ending with reversal of starting sequence
; 
; F_n is flip of top n elements.
; 
; 
; procedure P_n
; 
;   if n>1 then
;     begin
;        repeat   P_{n-1},F_n   n-1 times;
;        P_{n-1}
;     end
; 

(define (permutations x)
  (let ((x x)
        (perms (list x)))
    (define (P n)
      (if (> n 1)
          (do ((j (- n 1) (- j 1)))
              ((zero? j)
               (P (- n 1)))
              (P (- n 1))
              (F n))))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (cons x perms)))
    (define (revloop x n y)
      (if (zero? n)
          y
          (revloop (cdr x)
                   (- n 1)
                   (cons (car x) y))))
    (define (list-tail x n)
      (if (zero? n)
          x
          (list-tail (cdr x) (- n 1))))
    (P (length x))
    perms))

; Given a list of lists of numbers, returns the sum of the sums
; of those lists.
;
; for (; x != NULL; x = x->rest)
;     for (y = x->first; y != NULL; y = y->rest)
;         sum = sum + y->first;

(define (sumlists x)
  (do ((x x (cdr x))
       (sum 0 (do ((y (car x) (cdr y))
                   (sum sum (+ sum (car y))))
                  ((null? y) sum))))
      ((null? x) sum)))

; Destructive merge of two sorted lists.
; From Hansen's MS thesis.

(define (merge!! a b less?)

  (define (loop r a b)
    (if (less? (car b) (car a))
        (begin (set-cdr! r b)
               (if (null? (cdr b))
                   (set-cdr! b a)
                   (loop b a (cdr b)) ))
        ;; (car a) <= (car b)
        (begin (set-cdr! r a)
               (if (null? (cdr a))
                   (set-cdr! a b)
                   (loop a (cdr a) b)) )) )

  (cond ((null? a) b)
        ((null? b) a)
        ((less? (car b) (car a))
         (if (null? (cdr b))
             (set-cdr! b a)
             (loop b a (cdr b)))
         b)
        (else                           ; (car a) <= (car b)
         (if (null? (cdr a))
             (set-cdr! a b)
             (loop a (cdr a) b))
         a)))


;; Sort procedure which copies the input list and then sorts the
;; new list imperatively. Due to Richard O'Keefe; algorithm
;; attributed to D.H.D. Warren

(define (sort!! seq less?)
  
  (define (step n)
    (cond ((> n 2)
           (let* ((j (quotient n 2))
                  (a (step j))
                  (k (- n j))
                  (b (step k)))
             (merge!! a b less?)))
          ((= n 2)
           (let ((x (car seq))
                 (y (cadr seq))
                 (p seq))
             (set! seq (cddr seq))
             (if (less? y x)
                 (begin
                  (set-car! p y)
                  (set-car! (cdr p) x)))
             (set-cdr! (cdr p) '())
             p))
          ((= n 1)
           (let ((p seq))
             (set! seq (cdr seq))
             (set-cdr! p '())
             p))
          (else
           '())))
  
  (step (length seq)))

(define lexicographically-less?
  (lambda (x y)
    (define (lexicographically-less? x y)
      (cond ((null? x) (not (null? y)))
            ((null? y) #f)
            ((< (car x) (car y)) #t)
            ((= (car x) (car y))
             (lexicographically-less? (cdr x) (cdr y)))
            (else #f)))
    (lexicographically-less? x y)))

; this is 'sort!'
(define (sort-1 list less?)
  (sort!! list less?))

; this is 'sort'
(define (sort-2 list less?)
  (sort!! (list-copy list) less?))

; this is 'safe-sort'

(define (sort-3 list less?)
  (list-copy (sort!! (list-copy list) less?)))

;; This is pretty optimal for Larceny.

(define (list-copy l)
  (define (loop l prev)
    (if (null? l)
	#t
	(let ((q (cons (car l) '())))
	  (set-cdr! prev q)
	  (loop (cdr l) q))))
  (if (null? l)
      l
      (let ((first (cons (car l) '())))
	(loop (cdr l) first)
	first)))
   
(define (rgen n m)
  (let loop ((n n) (l '()))
    (if (zero? n) 
	l
	(loop (- n 1) (cons (random m) l)))))

(define (perm8-benchmark)
  (run-benchmark "Perm8"
                 (lambda ()
                   (set! *perms* (permutations '(1 2 3 4 5 6 7 8)))
                   #t)))

(define (sumperms-benchmark)
  (run-benchmark "Sumperms"
                 (lambda ()
                   (sumlists *perms*))))

(define (10perm8-benchmark)
  (run-benchmark "10perm8"
                 (lambda ()
                   (set! *perms* (permutations '(1 2 3 4 5 6 7 8)))
                   #t)
                 10))

(define (mergesort-benchmark)
  (run-benchmark "Mergesort!"
                 (lambda ()
                   (sort!! *perms* lexicographically-less?)
                   #t)))

(define (sort-benchmark1 sorter)
  (let ((l (rgen 30000 10000)))
    (run-benchmark "Sorting 30000 random integers"
		   (lambda ()
		     (sorter l <)
		     #t))))

(define (sort-benchmark2 sorter)
  (run-benchmark "Making and then sorting 40320 permutations"
		 (lambda ()
		   (sorter (permutations '(1 2 3 4 5 6 7 8))
			   lexicographically-less?))))
