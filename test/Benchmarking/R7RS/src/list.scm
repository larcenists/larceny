;;; List benchmark for (scheme list).

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme list))

(define (symbols n)
  (map string->symbol
       (map (lambda (s) (string-append "x" s))
            (map number->string
                 (iota n)))))

(define (powerset universe)
  (if (null? universe)
      (list '())
      (let* ((x (car universe))
             (u2 (cdr universe))
             (pu2 (powerset u2)))
        (lset-union eq?
                    pu2
                    (map (lambda (y)
                           (lset-adjoin eq? y x))
                         pu2)))))

(define (permutations universe)
  (if (null? universe)
      (list '())
      (let* ((x (car universe))
             (u2 (cdr universe))
             (perms2 (permutations u2)))
        (concatenate
         (map (lambda (perm)
                (map (lambda (i)
                       (append (take perm i)
                               (cons x (drop perm i))))
                     (iota (+ 1 (length perm)))))
              perms2)))))

(define (go n)
  (let* ((universe (symbols n))
         (subsets (powerset universe))
         (perms (permutations universe)))
    (filter (lambda (perm)
              (member perm subsets))
            perms)))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "list"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
