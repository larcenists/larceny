;;; This is a stress test and benchmark for span-contains and friends.

(import (scheme base)
        (scheme char)
        (scheme write)
        (scheme time)
        (srfi 27)
        (primitives random)
        (in-progress spans))

(define (writeln . xs)
  (for-each write xs)
  (newline))

(define (displayln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (displayln "Error: test failed: ")
  (writeln token)
  (if (not (null? more))
      (for-each writeln more))
  (newline)
  #f)

;;; FIXME

(define-syntax test
  (syntax-rules ()
   ((_ expr expected)
    (let ((actual expr))
      (or (equal? actual expected)
          (fail 'expr actual expected))))))

(define-syntax test-assert
  (syntax-rules ()
   ((_ expr)
    (or expr (fail 'expr)))))

(define-syntax test-deny
  (syntax-rules ()
   ((_ expr)
    (or (not expr) (fail 'expr)))))

(define-syntax timed-test
  (syntax-rules ()
   ((_ name expr expected)
    (time-named-thunk name (lambda () (test expr expected))))))

(define (time-named-thunk name thunk)
  (define (rounded jiffies)
    (let* ((usec (round (/ (* 1000000.0 jiffies) (jiffies-per-second)))))
      (/ usec 1000000.0)))
  (let ((j0 (current-jiffy)))
    (thunk)
    (let ((j1 (current-jiffy)))
      (display name)
      (display "  ")
      (display (rounded (- j1 j0)))
      (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-random-string n)
  (make-random-ascii-string n))

;; Never uses tilde

(define (make-random-ascii-string n)
  (do ((s (make-string n))
       (i 0 (+ i 1)))
      ((= i n) s)
    (string-set! s i (integer->char (random 126)))))

(define (make-random-unicode-string n)
  (do ((s (make-string n))
       (i 0 (+ i 1)))
      ((= i n) s)
    (string-set! s i (integer->char (random 32768)))))

(define (random-ascii-search-successful m n)
  (let* ((n1 (random n))
         (n2 (max 1 (- n n1 1)))
         (m1 (random (max 1 (- m n))))
         (m2 (max 1 (- m m1 n)))
         (P (string-append (make-random-ascii-string n1)
                           "~"
                           (make-random-ascii-string n2)))
         (T (string-append (make-random-ascii-string m1)
                           P
                           (make-random-ascii-string m2)))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (expected (span-index->cursor sp1 m1))
         (name (string-append "success: "
                              (number->string m)
                              " "
                              (number->string n))))
    (lambda ()
      (timed-test (string-append name "      ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2)))
                  expected)
#;
      (timed-test (string-append name " (RK) ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2 'rabin-karp)))
                  expected)
#;
      (timed-test (string-append name " (BM) ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2 'boyer-moore)))
                  expected))))

(define (random-ascii-search-failing m n)
  (let* ((n1 (random n))
         (n2 (max 1 (- n n1 1)))
         (P (string-append (make-random-ascii-string n1)
                           "~"
                           (make-random-ascii-string n2)))
         (T (string-append (make-random-ascii-string m)))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (name (string-append "failure: "
                              (number->string m)
                              " "
                              (number->string n))))
    (lambda ()
      (timed-test (string-append name "      ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2)))
                  #f)
#;
      (timed-test (string-append name " (RK) ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2 'rabin-karp)))
                  #f)
#;
      (timed-test (string-append name " (BM) ")
                  (dotimes (quotient total-work m)
                           (lambda () (span-contains sp1 sp2 'boyer-moore)))
                  #f))))

(define (hard-case-naive-ascii-search-successful m n)
  (let* ((P (string-append (make-string (- n 1) #\a) "b"))
         (T (string-append (make-string (- m n) #\a) P))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (expected (span-index->cursor sp1 (- m n)))
         (name (string-append "hard(R): "
                              (number->string m)
                              " "
                              (number->string n))))
    (lambda ()
      (timed-test (string-append name "      ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2)))
                  expected)
#;
      (timed-test (string-append name " (RK) ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2 'rabin-karp)))
                  expected)
#;
      (timed-test (string-append name " (BM) ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2 'boyer-moore)))
                  expected))))

(define (hard-case-BM-ascii-search-successful m n)
  (let* ((P (string-append "b" (make-string (- n 1) #\a)))
         (T (string-append (make-string (- m n) #\a) P))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (expected (span-index->cursor sp1 (- m n)))
         (name (string-append "hard(L): "
                              (number->string m)
                              " "
                              (number->string n))))
    (lambda ()
      (timed-test (string-append name "      ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2)))
                  expected)
#;
      (timed-test (string-append name " (RK) ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2 'rabin-karp)))
                  expected)
#;
      (timed-test (string-append name " (BM) ")
                  (dotimes (quotient total-work (* m n))
                           (lambda () (span-contains sp1 sp2 'boyer-moore)))
                  expected))))

(define (dotimes n thunk)
  (if (> n 1)
      (begin (thunk) (dotimes (- n 1) thunk))
      (thunk)))

(define total-work 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lengths1
  '(1 10 100 1000 10000))

(define lengths2
  '(1 2 3 4 5 10 20 50 100 1000))

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            ((hard-case-naive-ascii-search-successful m n))))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            ((hard-case-BM-ascii-search-successful m n))))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            ((random-ascii-search-successful m n))))
                      lengths2))
          lengths1)

; eof
