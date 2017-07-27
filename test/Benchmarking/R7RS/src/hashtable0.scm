;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Hashtable benchmark.
;
; Tests only eq? and eqv? hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (except (scheme hash-table)
                string-hash string-ci-hash)
        (scheme comparator))
  
; Crude test rig, just for benchmarking.

(define failures '())

(define (report-failure! n)
  (set! failures (cons n failures))
  (display "******** TEST FAILED ******** ")
  (write n)
  (newline))

(define (make-eq-hash-table)
  (make-hash-table (make-eq-comparator)))

(define (make-eqv-hash-table)
  (make-hash-table (make-eqv-comparator)))

; The parameter n2 is the number of items to be added to the table
; during the stress phase.

(define (hash-table-eq-tests n2 . rest)
  (call-with-current-continuation
   (lambda (exit)
     (let ((maker (if (null? rest)
                      make-eq-hash-table
                      (car rest)))
           (test (lambda (n passed?)
                   (if (not passed?)
                       (report-failure! n)))))

       (let ((t (maker))
             (not-found (list 'not-found))
             (x1 (string #\a #\b #\c))
             (sym1 'sym1)
             (vec1 (vector 'vec1))
             (pair1 (list -1))
             (n1 1000)             ; population added in first phase
            ;(n2 10000)            ; population added in second phase
             (n3 1000))            ; population added in third phase

         (define (hash-table-get t key)
           (hash-table-ref/default t key #f))

         (test 1 (eq? not-found (hash-table-ref/default t x1 not-found)))
         (hash-table-set! t x1 'a)
         (test 2 (eq? 'a (hash-table-get t x1)))
         (hash-table-set! t sym1 'b)
         (test 3 (eq? 'a (hash-table-get t x1)))
         (test 4 (eq? 'b (hash-table-get t sym1)))
         (hash-table-set! t vec1 'c)
         (test 5 (eq? 'a (hash-table-get t x1)))
         (test 6 (eq? 'b (hash-table-get t sym1)))
         (test 7 (eq? 'c (hash-table-get t vec1)))
         (hash-table-set! t n2 'd)
         (test 8 (eq? 'a (hash-table-get t x1)))
         (test 9 (eq? 'b (hash-table-get t sym1)))
         (test 10 (eq? 'c (hash-table-get t vec1)))
         (test 11 (eq? 'd (hash-table-get t n2)))

         (hash-table-set! t pair1 'e)

         (do ((i 0 (+ i 1)))
             ((= i n1))
           (hash-table-set! t (list i) i))
         (test 12 (eq? 'e (hash-table-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n2))
           (if (and #f (zero? (remainder i 1000))) (display "."))
           (hash-table-set! t (list i) i))
         (test 13 (eq? 'e (hash-table-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n3))
           (test 14 (eq? 'e (hash-table-get t pair1)))
           (hash-table-set! t (list i) i))
         (test 15 (eq? 'a (hash-table-get t x1)))
         (test 16 (eq? 'b (hash-table-get t sym1)))
         (test 17 (eq? 'c (hash-table-get t vec1)))
         (test 18 (eq? 'd (hash-table-get t n2)))
         (test 19 (eq? 'e (hash-table-get t pair1)))

         (hash-table-size t))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "hashtable0"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (hash-table-eq-tests (hide count input1) make-eq-hash-table)
       (hash-table-eq-tests (hide count input2) make-eqv-hash-table))
     (lambda (result) (and (null? failures) (equal? result output))))))
