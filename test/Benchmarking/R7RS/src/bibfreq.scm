;;; find the most frequently referenced word in the bible.
;;; aziz ghuloum (Nov 2007)
;;; modified (slightly) by Will Clinger (Nov 2007)
;;; and again for R7RS (July 2017)

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme file)
        (scheme list)
        (scheme char)
        (scheme sort)
        (except (scheme hash-table)
                string-hash string-ci-hash)
        (scheme comparator))
  
(define (fill input-file h)
  (let ((p (open-input-file input-file))
        (updater (lambda (x) (+ x 1)))
        (if-missing (lambda () 0)))
    (define (put ls) 
      (hash-table-update! h 
        (string->symbol
          (list->string
            (reverse ls)))
        updater
        if-missing))
    (define (alpha ls) 
      (let ((c (read-char p)))
        (cond
          ((eof-object? c) 
           (put ls))
          ((char-alphabetic? c) 
           (alpha (cons (char-downcase c) ls)))
          (else (put ls) (non-alpha)))))
    (define (non-alpha) 
      (let ((c (read-char p)))
        (cond
          ((eof-object? c) (values))
          ((char-alphabetic? c) 
           (alpha (list (char-downcase c))))
          (else (non-alpha)))))
    (non-alpha)
    (close-input-port p)))

(define (go input-file)
  (let ((h (make-hash-table (make-eq-comparator))))
    (fill input-file h)
    (let-values (((keys vals) (hash-table-entries h)))
       (let ((ls (map cons keys vals)))
         (take 
           (list-sort (lambda (a b) (> (cdr a) (cdr b))) ls)
           10)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "bibfreq"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
