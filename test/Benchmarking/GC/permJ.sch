;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         permJ.sch
; Description:  memory system benchmark using Zaks's permutation generator
; Author:       Lars Hansen, Will Clinger, and Gene Luks
; Created:      18-Mar-94 (perm9.sch) but rewritten 04-Jun-2011 (see below)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 940720 / lth Added some more benchmarks for the thesis paper.
; 970215 / wdc Increased problem size from 8 to 9; improved tenperm9-benchmark.
; 970531 / wdc Cleaned up for public release.
; 000820 / wdc Added the MpermNKL benchmark; revised for new run-benchmark.
; 110604 / wdc permJ.sch uses vectors instead of pairs (to compare with Java)
;
; In 32-bit Larceny, a pair occupies only 8 bytes, but a two-element
; vector occupies 16 bytes.  When comparing with Java, 16-byte pairs
; make Java look better.
;
; The original comment follows, but the sumperms and mergesort!
; benchmarks have been removed from this file.

; This benchmark is in four parts.  Each tests a different aspect of
; the memory system.
;
;    perm            storage allocation
;    10perm          storage allocation and garbage collection
;    sumperms        traversal of a large, linked, self-sharing structure
;    mergesort!      side effects and write barrier
;
; The perm9 benchmark generates a list of all 362880 permutations of
; the first 9 integers, allocating 1349288 pairs (typically 10,794,304
; bytes), all of which goes into the generated list.  (That is, the
; perm9 benchmark generates absolutely no garbage.)  This represents
; a savings of about 63% over the storage that would be required by
; an unshared list of permutations.  The generated permutations are
; in order of a grey code that bears no obvious relationship to a
; lexicographic order.
;
; The 10perm9 benchmark repeats the perm9 benchmark 10 times, so it
; allocates and reclaims 13492880 pairs (typically 107,943,040 bytes).
; The live storage peaks at twice the storage that is allocated by the
; perm9 benchmark.  At the end of each iteration, the oldest half of
; the live storage becomes garbage.  Object lifetimes are distributed
; uniformly between 10.3 and 20.6 megabytes.
;
; The 10perm9 benchmark is the 10perm9:2:1 special case of the
; MpermNKL benchmark, which allocates a queue of size K and then
; performs M iterations of the following operation:  Fill the queue
; with individually computed copies of all permutations of a list of
; size N, and then remove the oldest L copies from the queue.  At the
; end of each iteration, the oldest L/K of the live storage becomes
; garbage, and object lifetimes are distributed uniformly between two
; volumes that depend upon N, K, and L.
;
; The sumperms benchmark computes the sum of the permuted integers
; over all permutations.
;
; The mergesort! benchmark destructively sorts the generated permutations
; into lexicographic order, allocating no storage whatsoever.
;
; The benchmarks are run by calling the following procedures:
;
;    (perm-benchmark n)
;    (tenperm-benchmark n)
;    (sumperms-benchmark n)
;    (mergesort-benchmark n)
;
; The argument n may be omitted, in which case it defaults to 9.
;
; These benchmarks assume that
;
;    (RUN-BENCHMARK <string> <thunk> <count>)
;    (RUN-BENCHMARK <string> <count> <thunk> <predicate>)
;
; reports the time required to call <thunk> the number of times
; specified by <count>, and uses <predicate> to test whether the
; result returned by <thunk> is correct.
 
; kons, kar, kdr, klist are the vector versions of cons, car, cdr, list

(define-syntax kons
  (syntax-rules ()
   ((kons x y)
    (vector x y))))

(define-syntax kar
  (syntax-rules ()
   ((kar x)
    (vector-ref x 0))))

(define-syntax kdr
  (syntax-rules ()
   ((kar x)
    (vector-ref x 1))))

(define-syntax klist
  (syntax-rules ()
   ((klist x)
    (vector x '()))))

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
        (perms (klist x)))
    (define (P n)
      (if (> n 1)
          (do ((j (- n 1) (- j 1)))
              ((zero? j)
               (P (- n 1)))
              (P (- n 1))
              (F n))))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (kons x perms)))
    (define (revloop x n y)
      (if (zero? n)
          y
          (revloop (kdr x)
                   (- n 1)
                   (kons (kar x) y))))
    (define (list-tail x n)
      (if (zero? n)
          x
          (list-tail (kdr x) (- n 1))))
    (define (length x)
      (if (null? x)
          0
          (+ 1 (length (kdr x)))))
    (P (length x))
    perms))


(define *perms* '#())

(define (one..n n)
  (do ((n n (- n 1))
       (p '() (kons n p)))
      ((zero? n) p)))
   
(define (perm-benchmark . rest)
  (let ((n (if (null? rest) 9 (car rest))))
    (set! *perms* '())
    (run-benchmark (string-append "Perm" ":" (number->string n))
                   1
                   (lambda ()
                     (set! *perms* (permutations (one..n n)))
                     #t)
                   (lambda (x) #t))))

(define (tenperm-benchmark . rest)
  (let ((n (if (null? rest) 9 (car rest))))
    (set! *perms* '())
    (MpermNKL-benchmark 10 n 2 1)))

(define (MpermNKL-benchmark m n k ell)
  (if (and (<= 0 m)
           (positive? n)
           (positive? k)
           (<= 0 ell k))
      (let ((id (string-append (number->string m)
                               "perm"
                               (number->string n)
                               ":"
                               (number->string k)
                               ":"
                               (number->string ell)))
            (queue (make-vector k '())))

        ; Fills queue positions [i, j).

        (define (fill-queue i j)
          (if (< i j)
              (begin (vector-set! queue i (permutations (one..n n)))
                     (fill-queue (+ i 1) j))))

        ; Removes ell elements from queue.

        (define (flush-queue)
          (let loop ((i 0))
            (if (< i k)
                (begin (vector-set! queue
                                    i
                                    (let ((j (+ i ell)))
                                      (if (< j k)
                                          (vector-ref queue j)
                                          '())))
                       (loop (+ i 1))))))

        (fill-queue 0 (- k ell))
        (run-benchmark id
                       m
                       (lambda ()
                         (fill-queue (- k ell) k)
                         (flush-queue)
                         queue)
                       (lambda (q)
                         (let ((q0 (vector-ref q 0))
                               (qi (vector-ref q (max 0 (- k ell 1)))))
                           (equal? q0 qi)))))
      (begin (display "Incorrect arguments to MpermNKL-benchmark")
             (newline))))
