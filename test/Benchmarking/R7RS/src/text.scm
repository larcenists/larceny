;;; Micro-benchmarks for SRFI 135 and (scheme text).
;;;
;;; The main purpose of this program is to provide objective
;;; measurements that can be used to decide which of the sample
;;; implementations' three kernels would perform best in any
;;; particular system.
;;;
;;; The kernels implement a small core:
;;;
;;; text?  text-tabulate  text-length  text-ref  subtext  text-concatenate
;;;
;;; The performance of that core determines the performance of
;;; the sample implementations, so a few micro-benchmarks should
;;; suffice:
;;;
;;;     traversal
;;;         English, texts of length 10, 100, 1000
;;;         English, Thucydides Book I
;;;         Greek, Thucydides Book I
;;;     split
;;;         English, Thucydides Book I
;;;         Greek, Thucydides Book I
;;;     join
;;;         English, reversing the split
;;;         Greek, reversing the split
;;;
;;; Benchmark results are reported as nanoseconds per character.
;;;
;;; English and Greek texts for Thucydides' History of the
;;; Peloponnesian War, in Unicode, were downloaded from these
;;; sites at Project Gutenberg:
;;;
;;; http://onlinebooks.library.upenn.edu/webbin/gutbook/lookup?num=7142
;;; http://onlinebooks.library.upenn.edu/webbin/gutbook/lookup?num=29833
;;;
;;; The Greek text contains Book I only.  The Crawley translation
;;; is complete, but the end of Book I can be detected by scanning
;;; for a line saying "BOOK II" following a famous sentence about
;;; Pericles.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme time)
        (scheme text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read test data from files.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-file-as-string filename)
  (display "Reading ")
  (display filename)
  (display " as string...\n")
  (call-with-port
   (open-input-file filename)
   (lambda (p)
     (let ((q (open-output-string)))
       (let loop ()
         (let ((s (read-line p)))
           (if (string? s)
               (begin (write-string s q)
                      (newline q)
                      (loop))
               (begin (flush-output-port q)
                      (get-output-string q)))))))))

(define (read-file-as-text filename)
  (display "Reading ")
  (display filename)
  (display " as text...\n")
  (call-with-port
   (open-input-file filename)
   (lambda (p)
     (let loop ((texts '()))
       (let ((s (read-line p)))
         (if (string? s)
             (loop (cons (string->text (string-append s "\n")) texts))
             (textual-concatenate-reverse texts)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-traversal-benchmark s count)
  (define (traverse s)
    (let ((n (string-length s)))
      (let loop ((i 0)
                 (j 0))
        (cond ((= i n) j)
              ((char=? #\space (string-ref s i))
               (loop (+ i 1) (+ j 1)))
              (else (loop (+ i 1) j))))))
  (run-character-benchmark 'string-traversal
                           count
                           (lambda () (traverse s))
                           exact-integer?
                           (string-length s)))

(define (text-traversal-benchmark s count)
  (define (traverse s)
    (let ((n (text-length s)))
      (let loop ((i 0)
                 (j 0))
        (cond ((= i n) j)
              ((char=? #\space (text-ref s i))
               (loop (+ i 1) (+ j 1)))
              (else (loop (+ i 1) j))))))
  (run-character-benchmark 'text-traversal
                           count
                           (lambda () (traverse s))
                           exact-integer?
                           (text-length s)))

(define (text-split-benchmark txt count)
  (run-character-benchmark 'textual-split
                           count
                           (lambda () (textual-split txt " "))
                           list?
                           (text-length txt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations
;;; and returns the execution time in seconds.

(define (run-benchmark name count thunk ok?)
  (let* ((j/s (jiffies-per-second))
         (t0 (current-second))
         (j0 (current-jiffy)))
    (let loop ((i 0)
               (result (if #f #f)))
      (cond ((< i count)
             (loop (+ i 1) (thunk)))
            ((ok? result)
             (let* ((j1 (current-jiffy))
                    (jifs (- j1 j0))
                    (secs (inexact (/ jifs j/s))))
               secs))
            (else
             (display "ERROR: ")
             (display name)
             (display " returned incorrect result: ")
             (write result)
             (newline)
             -1)))))

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; a unary predicate that is true of the correct results of that thunk,
;;; and the number of characters processed by calling the thunk once,
;;; runs the benchmark the specified number of times
;;; and returns the number of seconds per character.

(define (run-character-benchmark name count thunk ok? k)
  (let ((t (run-benchmark name count thunk ok?)))
    (/ t count k)))

;;; Given the name of a benchmark,
;;; a unary procedure that runs the benchmark on its input,
;;; an input for that procedure,
;;; and the number of times to run the benchmark,
;;; runs the benchmark that many times and reports its timing.

(define (report-benchmark name proc input count)
  (display name)
  (display " : ")
  (let* ((t (proc input count))
         (t (exact (round (* 1e9 t)))))
    (write t))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (go2 thucydides1-file crawley-file)

  (define thucydides1-as-string (read-file-as-string thucydides1-file))
  (define thucydides1-as-text   (read-file-as-text   thucydides1-file))
  (define crawley-as-string     (read-file-as-string crawley-file))
  (define crawley-as-text       (read-file-as-text   crawley-file))

;;; FIXME

  (define crawley1-as-text
    (subtext crawley-as-text
	     0
	     (textual-contains crawley-as-text
			       "BOOK II"
			       (textual-contains
				crawley-as-text
				"Such were the words of Pericles."))))

  (define crawley1-as-string
    (substring crawley-as-string 0 (text-length crawley1-as-text)))

  (define str10   (substring crawley1-as-string 0 10))
  (define str100  (substring crawley1-as-string 0 100))
  (define str1000 (substring crawley1-as-string 0 1000))

  (define text10   (string->text str10))
  (define text100  (string->text str100))
  (define text1000 (string->text str1000))

  (write (text-length crawley1-as-text))
  (display " characters in English input\n")

  (write (text-length thucydides1-as-text))
  (display " characters in Greek input\n\n")

  (display "Nanoseconds per character:\n\n")

  (report-benchmark "string traversal (10)     "
		    string-traversal-benchmark
		    str10
                    100000)

  (report-benchmark "string traversal (100)    "
		    string-traversal-benchmark
		    str100
                    10000)

  (report-benchmark "string traversal (1000)   "
		    string-traversal-benchmark
		    str1000
                    1000)

  (report-benchmark "string traversal (English)"
		    string-traversal-benchmark
		    crawley1-as-string
                    50)

  (report-benchmark "string traversal (Greek)  "
		    string-traversal-benchmark
		    thucydides1-as-string
                    25)

  (report-benchmark "text   traversal (10)     "
		    text-traversal-benchmark
		    text10
                    100000)

  (report-benchmark "text   traversal (100)    "
		    text-traversal-benchmark
		    text100
                    10000)

  (report-benchmark "text   traversal (1000)   "
		    text-traversal-benchmark
		    text1000
                    1000)

  (report-benchmark "text   traversal (English)"
		    text-traversal-benchmark
		    crawley1-as-text
                    50)

  (report-benchmark "text   traversal (Greek)  "
		    text-traversal-benchmark
		    thucydides1-as-text
                    25)

  (report-benchmark "text   split     (100)    "
		    text-split-benchmark
		    (subtext crawley1-as-text 1000 1100)
                    2000)

  (report-benchmark "text   split     (1000)   "
		    text-split-benchmark
		    (subtext crawley1-as-text 1000 2000)
                    200)

  (report-benchmark "text   split     (10000)  "
		    text-split-benchmark
		    (subtext crawley1-as-text 1000 11000)
                    20)

  (report-benchmark "text   split     (English)"
		    text-split-benchmark
		    crawley1-as-text
                    10)

  (length (textual-split crawley1-as-text " ")))

;(define thucydides1-file "29833-0.txt")
;(define crawley-file "7142.txt")

(define (go thucydides1-file crawley-file)
  (go2 thucydides1-file crawley-file))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "text"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (go (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))
