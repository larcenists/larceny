;;; Implementation of the Sieve of Eratosthenes, using a bitset.
;;; The bitset is immutable and thus copied on each modification,
;;; which ruins the efficiency of the algorithm but makes it a
;;; good benchmark for bitwise operations.

(import (rnrs base)
        (rnrs arithmetic bitwise)
        (rnrs io simple))

;;; A bitset is a non-negative exact integer.

;; Returns a list of the indexes at which the bitset has a one.
(define (bitset->list bs)
  (define (loop bs l)
    (if (zero? bs)
        l
        (let ((k (bitwise-first-bit-set bs)))
          (loop (bitwise-copy-bit bs k 0)
                (cons k l)))))
  (reverse (loop bs '())))

;; Returns a list of the prime numbers within the given bound.
(define (primes<= n)
  (define (filter-loop k m comps)
    (if (> m n)
      comps
      (filter-loop k (+ k m) (bitwise-copy-bit comps m 1))))
  (define (base-loop i comps)
    (cond ((> i n)
           comps)
          ((bitwise-bit-set? comps i)
           (base-loop (+ i 1) comps))
          (else
           (base-loop (+ i 1) (filter-loop i (+ i i) comps)))))
  (bitset->list
    (bitwise-not
      (bitwise-ior (base-loop 2 3)
                   (bitwise-arithmetic-shift-left -1 (+ n 1))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "primes"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (primes<= (hide count input1)))
     (lambda (result) (equal? result output)))))
