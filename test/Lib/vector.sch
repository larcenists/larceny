; Test cases for vector copies and fills.
;
; Copies and fills of at least sys$vector-like-move-threshold elements
; use memmove (src/Rts/Sys/primitive.c), doing the write barriers' work
; for each slot, so these tests use sizes on both sides of it, overlaps
; in both directions, and collections after storing young objects in
; old vectors.

(define (run-vector-tests)
  (display "Vectors") (newline)
  (vector-copy-tests)
  (vector-barrier-tests))

(define (vector-copy-tests)

  (define sizes '(0 1 2 5 7 8 9 15 16 17 100 1000))

  (define (iota-vector n)
    (let ((v (make-vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) v)
        (vector-set! v i (list i)))))

  ; The reference: copies one element at a time, through a list, so
  ; that overlap does not matter.

  (define (reference-copy! dst at src start end)
    (let ((xs (do ((i (- end 1) (- i 1))
                   (xs '() (cons (vector-ref src i) xs)))
                  ((< i start) xs))))
      (do ((i at (+ i 1))
           (xs xs (cdr xs)))
          ((null? xs) dst)
        (vector-set! dst i (car xs)))))

  (define (clone v)
    (reference-copy! (make-vector (vector-length v)) 0 v 0 (vector-length v)))

  (define (same? v w)
    (and (= (vector-length v) (vector-length w))
         (let loop ((i 0))
           (or (= i (vector-length v))
               (and (eq? (vector-ref v i) (vector-ref w i))
                    (loop (+ i 1)))))))

  (define (every? p l)
    (or (null? l) (and (p (car l)) (every? p (cdr l)))))

  (allof "vector copy tests"

   (test "vector-copy"
         (every? (lambda (n)
                   (let ((v (iota-vector n)))
                     (and (same? (vector-copy v) v)
                          (or (< n 2)
                              (same? (vector-copy v 1 (- n 1))
                                     (reference-copy! (make-vector (- n 2))
                                                      0 v 1 (- n 1)))))))
                 sizes)
         #t)

   (test "vector-append"
         (every? (lambda (n)
                   (let* ((v (iota-vector n))
                          (w (iota-vector (quotient n 2)))
                          (a (vector-append v w v)))
                     (same? a
                            (let ((r (make-vector (+ n (quotient n 2) n))))
                              (reference-copy! r 0 v 0 n)
                              (reference-copy! r n w 0 (quotient n 2))
                              (reference-copy! r (+ n (quotient n 2)) v 0 n)
                              r))))
                 sizes)
         #t)

   (test "vector-copy! between vectors"
         (every? (lambda (n)
                   (let ((src (iota-vector n)))
                     (every? (lambda (at)
                               (let ((d1 (make-vector (+ n 3) 'x))
                                     (d2 (make-vector (+ n 3) 'x)))
                                 (vector-copy! d1 at src)
                                 (reference-copy! d2 at src 0 n)
                                 (same? d1 d2)))
                             '(0 1 3))))
                 sizes)
         #t)

   (test "vector-copy! overlapping, both directions"
         (every? (lambda (n)
                   (every? (lambda (shift)
                             (let* ((v1 (iota-vector (+ n 4)))
                                    (v2 (clone v1)))
                               (if (< shift 0)
                                   (begin
                                    (vector-copy! v1 0 v1 (- shift) (+ n 4))
                                    (reference-copy! v2 0 v2 (- shift) (+ n 4)))
                                   (begin
                                    (vector-copy! v1 shift v1 0 n)
                                    (reference-copy! v2 shift v2 0 n)))
                               ;; reference-copy! of v2 into itself read
                               ;; the source before storing.
                               (same? v1 v2)))
                           '(-3 -1 0 1 3 4)))
                 sizes)
         #t)

   (test "vector-fill!"
         (every? (lambda (n)
                   (let* ((v (iota-vector (+ n 4)))
                          (w (clone v))
                          (x (list 'fill)))
                     (vector-fill! v x 2 (+ n 2))
                     (do ((i 2 (+ i 1)))
                         ((= i (+ n 2)))
                       (vector-set! w i x))
                     (vector-fill! w (vector-ref w 0) 0 0)
                     (same? v w)))
                 sizes)
         #t)

   (test "vector-fill!, whole vector"
         (let ((v (make-vector 100 0)))
           (vector-fill! v 'a)
           (vector->list v 95))
         '(a a a a a))))

; Stores young objects into old vectors with vector-copy!, vector-fill!
; and vector-copy, and checks them after collections: a missing barrier
; entry lets a minor collection lose or not update the young objects.
; The large vector is a large object in some collectors.

(define (vector-barrier-tests)

  (define (churn)                       ; allocate enough for collections
    (do ((i 0 (+ i 1))
         (l '() (if (= 0 (remainder i 1000)) '() (cons i l))))
        ((= i 200000))))

  (define (fresh n k)
    (let ((v (make-vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) v)
        (vector-set! v i (list k i)))))

  (define (check v k start end)
    (let loop ((i start))
      (or (= i end)
          (let ((x (vector-ref v i)))
            (and (pair? x)
                 (eqv? (car x) k)
                 (eqv? (cadr x) (- i start))
                 (loop (+ i 1)))))))

  (define (check-fill v x start end)
    (let loop ((i start))
      (or (= i end)
          (and (eq? (vector-ref v i) x)
               (equal? (vector-ref v i) '(filled))
               (loop (+ i 1))))))

  (define (run n rounds)
    (let ((old (make-vector n #f)))
      (collect)                         ; old is old
      (let loop ((k 0) (ok #t))
        (if (or (= k rounds) (not ok))
            ok
            (let ((src (fresh n k))
                  (x (list 'filled))
                  (half (quotient n 2)))
              (vector-copy! old 0 src 0 half)
              (vector-fill! old x half n)
              (let ((copy (vector-copy old 0 half)))
                (churn)
                (loop (+ k 1)
                      (and (check old k 0 half)
                           (check-fill old x half n)
                           (check copy k 0 half)))))))))

  (allof "vector barrier tests"
   (test "copies and fills into an old vector" (run 1000 20) #t)
   (test "copies and fills into an old large vector" (run 200000 5) #t)))
