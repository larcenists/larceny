; Objects larger than 2GB and 4GB (64-bit systems only).
;
; Needs about 12GB of memory; run it under each collector:
;
;     LARCENY_ARENA_GB=64 larceny [-stopcopy | -rrof] < big-objects.sch
;
; Each test prints "ok" or "FAIL".  The stress test stores young objects
; in the high slots (beyond 4GB) of an old vector while the collector
; runs; under -rrof, that exercises the remembered sets and summaries.

(define (report name ok)
  (display name)
  (display (if ok ": ok" ": FAIL"))
  (newline))

(define (test-bytevector n)
  (let ((bv (make-bytevector n 7)))
    (bytevector-u8-set! bv (- n 1) 42)
    (bytevector-u8-set! bv 0 17)
    (collect)
    (collect)
    (report (string-append "bytevector " (number->string n))
            (and (= (bytevector-length bv) n)
                 (= (bytevector-u8-ref bv (- n 1)) 42)
                 (= (bytevector-u8-ref bv 0) 17)
                 (= (bytevector-u8-ref bv (quotient n 2)) 7)
                 (= (bytevector-u8-ref bv (- n 2)) 7)))))

(define (test-vector n)
  (let ((v (make-vector n 0)))
    (vector-set! v (- n 1) (list 'last))
    (vector-set! v (quotient n 2) (list 'middle))
    (collect)
    (vector-set! v (- n 2) (list 'young))
    (collect)
    (collect)
    (report (string-append "vector " (number->string n))
            (and (= (vector-length v) n)
                 (equal? (vector-ref v (- n 1)) '(last))
                 (equal? (vector-ref v (- n 2)) '(young))
                 (equal? (vector-ref v (quotient n 2)) '(middle))
                 (eqv? (vector-ref v 0) 0)))))

(define (test-string n)
  (let ((s (make-string n #\a)))
    (string-set! s (- n 1) #\z)
    (collect)
    (report (string-append "string " (number->string n))
            (and (= (string-length s) n)
                 (char=? (string-ref s (- n 1)) #\z)
                 (char=? (string-ref s (quotient n 2)) #\a)))))

(define (test-vector-copy n)
  (let* ((v (make-vector n 1))
         (w (vector-copy v)))
    (vector-set! w (- n 1) 2)
    (collect)
    (report (string-append "vector-copy " (number->string n))
            (and (= (vector-ref v (- n 1)) 1)
                 (= (vector-ref w (- n 1)) 2)))))

(define (test-stress n rounds)
  (define (index j round)
    (- n 1 (* 997 (+ j (* 1000 round)))))
  (let ((v (make-vector n #f)))
    (collect)
    (do ((round 0 (+ round 1)))
        ((= round rounds))
      (do ((j 0 (+ j 1)))
          ((= j 1000))
        (let ((i (index j round)))
          (vector-set! v i (list i (make-vector 1000 i)))))
      (do ((j 0 (+ j 1)))
          ((= j 200000))
        (make-vector 20 j)))
    (collect)
    (report (string-append "stress " (number->string n))
            (let loop ((round 0))
              (or (= round rounds)
                  (and (let check ((j 0))
                         (or (= j 1000)
                             (let* ((i (index j round))
                                    (x (vector-ref v i)))
                               (and (pair? x)
                                    (eqv? (car x) i)
                                    (= (vector-length (cadr x)) 1000)
                                    (eqv? (vector-ref (cadr x) 0) i)
                                    (eqv? (vector-ref (cadr x) 999) i)
                                    (check (+ j 1))))))
                       (loop (+ round 1))))))))

(test-bytevector 3000000000)
(test-bytevector 10000000000)
(test-vector 400000000)                 ; 3.2GB
(test-string 700000000)                 ; 2.8GB
(test-vector-copy 300000000)            ; 2.4GB
(test-stress 600000000 40)              ; 4.8GB
(exit)
