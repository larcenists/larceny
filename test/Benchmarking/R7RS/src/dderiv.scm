;;; DDERIV -- Table-driven symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme cxr)
        (except (scheme hash-table)
                string-hash string-ci-hash)
        (scheme comparator))
  
(define (lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define symbol-comparator
  (make-comparator symbol?
                   eq?
                   (lambda (x y)
                     (string<? (symbol->string x)
                               (symbol->string y)))
                   symbol-hash))

(define properties (make-hash-table symbol-comparator))

(define (get key1 key2)
  (let ((x (hash-table-ref/default properties key1 #f)))
    (if x
      (let ((y (lookup key2 x)))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (hash-table-ref/default properties key1 #f)))
    (if x
      (let ((y (lookup key2 x)))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (hash-table-set! properties key1 (list (cons key2 val))))))

(define (my+dderiv a)
  (cons '+
        (map dderiv (cdr a))))

(define (my-dderiv a)
  (cons '-
        (map dderiv (cdr a))))

(define (*dderiv a)
  (list '*
         a
         (cons '+
               (map (lambda (a) (list '/ (dderiv a) a)) (cdr a)))))

(define (/dderiv a)
  (list '-
        (list '/
              (dderiv (cadr a))
              (caddr a))
        (list '/
              (cadr a)
              (list '*
                    (caddr a)
                    (caddr a)
                    (dderiv (caddr a))))))

(put '+ 'dderiv my+dderiv)
(put '- 'dderiv my-dderiv)
(put '* 'dderiv *dderiv)
(put '/ 'dderiv /dderiv)

(define (dderiv a)
  (if (not (pair? a))
    (if (eq? a 'x) 1 0)
    (let ((f (get (car a) 'dderiv)))
      (if f
        (f a)
        (error #f "No derivation method available")))))

(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (name "dderiv"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (dderiv (hide count input)))
     (lambda (result) (equal? result output)))))
