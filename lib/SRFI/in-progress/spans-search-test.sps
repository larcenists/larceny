;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

(define (timed-test name thunk expected)
  (define name
    (if (string? name)
        (string->symbol name)
        name))
  (define (rounded jiffies)
    (let* ((usec (round (/ (* 1000000.0 jiffies) (jiffies-per-second)))))
      (/ usec 1000000.0)))
  (let* ((j0 (current-jiffy))
         (result (thunk))
         (j1 (current-jiffy)))
    (if (not (equal? result expected))
        (fail name result expected))
    (rounded (- j1 j0))))

(define (seconds->milliseconds t)
  (exact (round (* 1000.0 t))))

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
    (let loop ((bits (random #x10ffff)))
      (if (or (<= #xd800 bits #xdfff)
              (= bits (char->integer #\~)))
          (loop (random 32768))
          (string-set! s i (integer->char bits))))))

(define *sp1* (span))
(define *sp2* (span))

(define name:length 22) ; FIXME

(define (random-search testname make-random-string m n)
  (let* ((n1 (random n))
         (n2 (max 1 (- n n1 1)))
         (m1 (random (max 1 (- m n))))
         (m2 (max 1 (- m m1 n)))
         (P (string-append (make-random-string n1)
                           "~"
                           (make-random-string n2)))
         (T (string-append (make-random-string m1)
                           P
                           (make-random-string m2)))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (ignored (begin (set! *sp1* sp1)
                         (set! *sp2* sp2)))
         (expected (span-index->cursor sp1 m1))
         (name (string-append testname
                              (number->string m)
                              " "
                              (number->string n)))
         (t1 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:naive sp1 sp2))))
                         expected))
         (t2 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:rabin-karp sp1 sp2))))
                         expected))
         (t3 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:boyer-moore sp1 sp2))))
                         expected))
         (t4 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (span-contains sp1 sp2))))
                         expected))
         (t0 (min t1 t2 t3)))
    (display name)
    (display (make-string (- name:length (string-length name)) #\space))
    (display (cond ((= t0 t1)
                    "naive        ")
                   ((= t0 t2)
                    "Rabin-Karp   ")
                   ((= t0 t3)
                    "Boyer-Moore  ")))
    (write (cons (/ (round (* 10.0 (/ t4 t0))) 10.0)
                 (map seconds->milliseconds (list t1 t2 t3 t4))))
    (newline)))

(define (random-search-failing testname make-random-string m n)
  (let* ((n1 (random n))
         (n2 (max 1 (- n n1 1)))
         (P (string-append (make-random-string n1)
                           "~"
                           (make-random-string n2)))
         (T (string-append (make-random-string m)))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (name (string-append testname
                              (number->string m)
                              " "
                              (number->string n)))
         (t1 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:naive sp1 sp2))))
                         #f))
         (t2 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:rabin-karp sp1 sp2))))
                         #f))
         (t3 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work m)
                                    (lambda ()
                                      (%span-contains:boyer-moore sp1 sp2))))
                         #f))
         (t4 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (span-contains sp1 sp2))))
                         #f))
         (t0 (min t1 t2 t3)))
    (display name)
    (display (make-string (- name:length (string-length name)) #\space))
    (display (cond ((= t0 t1)
                    "naive        ")
                   ((= t0 t2)
                    "Rabin-Karp   ")
                   ((= t0 t3)
                    "Boyer-Moore  ")))
    (write (cons (/ (round (* 10.0 (/ t4 t0))) 10.0)
                 (map seconds->milliseconds (list t1 t2 t3 t4))))
    (newline)))

(define (hard-case left/right make-random-string m n)
  (let* ((P (make-string (- n 1) #\a))
         (P (case left/right
             ((R) (string-append P "b"))
             ((L) (string-append "b" P))))
         (T (string-append (make-string (- m n) #\a) P))
         (sp1 (string->span T))
         (sp2 (string->span P))
         (expected (span-index->cursor sp1 (- m n)))
         (name (string-append "hard("
                              (symbol->string left/right)
                              "): "
                              (number->string m)
                              " "
                              (number->string n)))
         (t1 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (%span-contains:naive sp1 sp2))))
                         expected))
         (t2 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (%span-contains:rabin-karp sp1 sp2))))
                         expected))
         (t3 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (%span-contains:boyer-moore sp1 sp2))))
                         expected))
         (t4 (timed-test name
                         (lambda ()
                           (dotimes (quotient total-work (* m n))
                                    (lambda ()
                                      (span-contains sp1 sp2))))
                         expected))
         (t0 (min t1 t2 t3)))
    (display name)
    (display (make-string (- name:length (string-length name)) #\space))
    (display (cond ((= t0 t1)
                    "naive        ")
                   ((= t0 t2)
                    "Rabin-Karp   ")
                   ((= t0 t3)
                    "Boyer-Moore  ")))
    (write (cons (/ (round (* 10.0 (/ t4 t0))) 10.0)
                 (map seconds->milliseconds (list t1 t2 t3 t4))))
    (newline)))

(define (random-ascii-search-successful m n)
  (random-search "success: " make-random-ascii-string m n))

(define (random-ascii-search-failing m n)
  (random-search-failing "failure: " make-random-ascii-string m n))

(define (hard-case-naive-ascii-search-successful m n)
  (hard-case 'R make-random-ascii-string m n))

(define (hard-case-BM-ascii-search-successful m n)
  (hard-case 'L make-random-ascii-string m n))

(define (random-unicode-search-successful m n)
  (random-search "success: " make-random-unicode-string m n))

(define (hard-case-naive-unicode-search-successful m n)
  (hard-case 'R make-random-unicode-string m n))

(define (hard-case-BM-unicode-search-successful m n)
  (hard-case 'L make-random-unicode-string m n))

(define (dotimes n thunk)
  (if (> n 1)
      (begin (thunk) (dotimes (- n 1) thunk))
      (thunk)))

(define total-work 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define heading
  "test                  fastest  (def/best naive Rabin-Karp Boyer-Moore def)")

(display heading)
(newline)
(newline)

(define lengths1
  '(1 10 100 500 1000 5000 10000 20000))

(define lengths2
  '(1 2 3 4 5 10 20 50 100 1000))

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (hard-case-naive-ascii-search-successful m n)))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (hard-case-BM-ascii-search-successful m n)))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (random-ascii-search-successful m n)))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (hard-case-naive-unicode-search-successful m n)))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (hard-case-BM-unicode-search-successful m n)))
                      lengths2))
          lengths1)

(for-each (lambda (m)
            (for-each (lambda (n)
                        (if (<= n m)
                            (random-unicode-search-successful m n)))
                      lengths2))
          lengths1)

; eof
