; Test suite for sorting: vector-sort, vector-sort!, list-sort, sort,
; and sort!, on random, sorted, reversed, and constant input of every
; length up to 100, checked against a simple stable insertion sort
; (the vector sorts are merge sorts, with insertion sort for short runs).

(define (run-sort-tests)
  (display "Sort") (newline)
  (sort-stability-tests))

(define (sort-stability-tests)

  (define seed 12345)

  (define (rand n)
    (set! seed (remainder (+ (* seed 1103515245) 12345) 2147483648))
    (remainder (quotient seed 65536) n))

  (define (by-car<? x y) (< (car x) (car y)))

  (define (reference-sort l)             ; stable insertion sort
    (define (insert x l)
      (if (or (null? l) (by-car<? x (car l)))
          (cons x l)
          (cons (car l) (insert x (cdr l)))))
    (let loop ((l l) (acc '()))
      (if (null? l) acc (loop (cdr l) (insert (car l) acc)))))

  (define (input kind n)
    (do ((i 0 (+ i 1))
         (l '() (cons (cons (case kind
                              ((random) (rand 7))
                              ((sorted) i)
                              ((reversed) (- n i))
                              (else 3))
                            i)
                      l)))
        ((= i n) (reverse l))))

  (allof "sorting"
   (allof-map "kinds and lengths"
    (lambda (kind)
      (do ((n 0 (+ n 1)))
          ((> n 100))
        (let* ((l (input kind n))
               (expected (reference-sort l))
               (name (string-append (symbol->string kind) " "
                                    (number->string n))))
          (test (string-append "vector-sort " name)
                (vector->list (vector-sort by-car<? (list->vector l)))
                expected)
          (test (string-append "vector-sort! " name)
                (let ((v (list->vector l)))
                  (vector-sort! by-car<? v)
                  (vector->list v))
                expected)
          (test (string-append "list-sort " name)
                (list-sort by-car<? l)
                expected)
          (test (string-append "sort of a vector " name)
                (vector->list (sort (list->vector l) by-car<?))
                expected)
          (test (string-append "sort! of a vector " name)
                (let ((v (list->vector l)))
                  (sort! v by-car<?)
                  (vector->list v))
                expected))))
    '(random sorted reversed constant))
   (test "vector-sort! of a range"
         (let ((v (vector 9 8 7 6 5 4 3 2 1 0)))
           (vector-sort! < v 2 8)
           v)
         '#(9 8 2 3 4 5 6 7 1 0))
   (test "vector-sort of a range"
         (vector-sort < (vector 9 8 7 6 5 4 3 2 1 0) 2 8)
         '#(2 3 4 5 6 7))))

; eof
