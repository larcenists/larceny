;;; Larceny-specific
;;; Benchmark to compare the quality and performance of hash functions
;;; exported by
;;;
;;;     (rnrs hashtable)            R6RS hashtables
;;;     (srfi 126)                  R6RS-based hashtables
;;;     (srfi 128)                  comparators (reduced)
;;;     (srfi 128 reference)        comparators (reduced), reference impl
;;;     (srfi 125)                  intermediate hash tables
;;;
;;; The reference implementation of SRFI 128 is still being benchmarked
;;; because it serves as a stress test for Larceny's garbage collectors
;;; and is being used to track down a possible bug in Larceny's regional
;;; collector.

(import (scheme base)
        (scheme time)
        (scheme write)
        (scheme inexact)
        (primitives sro typetag object-is-circular? filter mod)  ; FIXME
        (rnrs hashtables)
        (rename (only (srfi 126) equal-hash)
                (equal-hash srfi-126-equal-hash))
        (only (srfi 128) default-hash)
        (rename (only (srfi 128 reference) default-hash)
                (default-hash srfi-128-reference-default-hash))
        (only (srfi 125) hash-by-identity))

;;; Given a hash function and a vector of objects to hash,
;;; returns two values:
;;;     a vector of the hash values
;;;     how many seconds it took to compute those hash values

(define (hash-objects hf objects)
  (let* ((t0 (current-jiffy))
         (v (vector-map hf objects))
         (t1 (current-jiffy)))
    (values v
            (inexact (/ (- t1 t0) (jiffies-per-second))))))

;;; Given a hash function, a pointer tag and type tag as accepted
;;; by Larceny's sro procedure, and a unary predicate, uses the
;;; hash function to compute the hash value for every object that
;;; has the given pointer tag and type tag and satisfies the predicate,
;;; and returns four values:
;;;     the number of objects hashed
;;;     the number of hash collisions per hash
;;;     the RMS number of objects per bucket (with as many buckets as objects)
;;;     how many seconds it took to compute the hash values

(define (hash-test hf pointer-tag type-tag okay?)
  (define (square x) (* x x))
  (let* ((v0 (sro pointer-tag type-tag -1))
         (v (list->vector
             (filter okay?
                     (vector->list v0)))))
    (call-with-values
     (lambda () (hash-objects hf v))
     (lambda (hashes dt)
       (let ((ht (make-hashtable abs =)))
         (vector-for-each
          (lambda (hash)
            (hashtable-set! ht hash (+ 1 (hashtable-ref ht hash 0))))
          hashes)
         (let* ((unique-hashes (hashtable-keys ht))
                (m (vector-length hashes))
                (n (vector-length unique-hashes))
                (pseudo-buckets (make-vector m 0)))
           (vector-for-each
            (lambda (hash)
              (let ((i (mod (abs hash) m)))
                (vector-set! pseudo-buckets
                             i
                             (+ 1 (vector-ref pseudo-buckets i)))))
            hashes)
           (do ((i 0 (+ i 1))
                (sum 0 (+ sum (square (vector-ref pseudo-buckets i)))))
               ((= i m)
                (let ((m (max m 1)))
                  (values m
                          (inexact (/ (- m n) m))
                          (inexact (sqrt (/ sum m)))
                          (inexact (/ dt m))))))))))))

;;; Given
;;;     the name of a benchmark
;;;     pointer-tag, type-tag, and unary predicate
;;; Calls hash-test for each of the hash functions to be tested
;;; and prints the results.

(define (run-hash-benchmark name pointer-tag type-tag okay?)
  (define (round-to x n)
    (let ((a (expt 10.0 n)))
      (/ (round (* a x)) a)))
  (define (show-percentage x)
    (right-justify (string-append (number->string (round-to (* 100.0 x) 1))
                                  "%")
                   12))
  (define (show-rms x)
    (right-justify (round-to x 2) 8))
  (define (show-nanoseconds x)
    (right-justify x 12))
  (define (right-justify x n)
    (cond ((string? x)
           (display (string-append (make-string (max 0 (- n (string-length x)))
                                                #\space)
                                   (substring x 0 (min n (string-length x))))))
          ((number? x)
           (right-justify (number->string x) n))
          (else
           (write x))))
  (define (run-hash-function hf first?)
    (call-with-values
     (lambda () (hash-test hf pointer-tag type-tag okay?))
     (lambda (m collisions/hash rms/bucket time/hash)
       (if first?
           (begin (newline)
                  (display name)
                  (display " (")
                  (write m)
                  (display ")")
                  (newline)))
       (show-percentage collisions/hash)
       (display "    ")
       (show-rms rms/bucket)
       (display "    ")
       (show-nanoseconds (exact (round (* 1e9 time/hash))))
       (newline))))
  (for-each run-hash-function
            (list equal-hash
                  srfi-126-equal-hash
                  default-hash
                  srfi-128-reference-default-hash
                  hash-by-identity)
            '(#t #f #f #f #f)))

(define (non-circular? x)
  (not (object-is-circular? x)))

(display "collisions (%)   rms/bucket     nanoseconds/hash\n")

(run-hash-benchmark "empty strings"
                    5
                    (typetag "")
                    (lambda (x) (= 0 (string-length x))))
(run-hash-benchmark "strings" 5 (typetag "") string?)
(run-hash-benchmark "symbols" 3 (typetag 'a) symbol?)
(run-hash-benchmark "numbers" -1 -1 number?)
(run-hash-benchmark "vectors" 3 (typetag '#()) non-circular?)
(run-hash-benchmark "vector-like" 3 -1 non-circular?)
(run-hash-benchmark "bytevectors" 5 -1 bytevector?)
(run-hash-benchmark "bytevector-like" 5 -1 (lambda (x) #t))
(run-hash-benchmark "procedures" 7 -1 non-circular?)

;;; SRFI-128 default-hash runs out of memory while hashing
;;; one of the non-circular lists.

(run-hash-benchmark "lists" 1 -1 non-circular?)

(run-hash-benchmark "all non-circular" -1 -1 non-circular?)
