;;; PRIMES -- Compute primes less than 100, written by Eric Mohr.

(define  (interval-list m n)
  (if (> m n)
    '()
    (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                '()
                (if (= (modulo (car l) n) 0)
                  (remove-multiples n (cdr l))
                  (cons (car l)
                        (remove-multiples n (cdr l))))))))
    (if (null? l)
      '()
      (cons (car l)
            (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(run-benchmark
  "primes"
  1000
  (lambda () (primes<= 100))
  (lambda (result)
    (equal? result
            '(2 3 5 7 11 13 17 19 23 29 31 37 41
              43 47 53 59 61 67 71 73 79 83 89 97))))
