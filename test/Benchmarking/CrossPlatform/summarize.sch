; Extraction of benchmark results.

(define (summarize-usual-suspects)
  ((summarize bigloo-results) "results.Bigloo" "summary.Bigloo")
  ((summarize chez-results) "results.Chez-Scheme" "summary.Chez")
  ((summarize chicken-results) "results.Chicken" "summary.Chicken")
  ((summarize gambit-results) "results.Gambit-C" "summary.Gambit")
  ((summarize larceny-results) "results.Larceny" "summary.Larceny")
  ((summarize mzscheme-results) "results.MzScheme" "summary.MzScheme"))

(define (decode-usual-suspects)
  (map decode-summary
       '("summary.Larceny"
         "summary.Bigloo"
         "summary.Chez"
         "summary.Chicken"
         "summary.Gambit"
         "summary.MzScheme")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (readline port)
  (do ((c (read-char port) (read-char port))
       (chars '() (cons c chars)))
      ((or (eof-object? c)
           (char=? c #\newline))
       (list->string (reverse chars)))))

(define (readlines port)
  (do ((c (peek-char port) (peek-char port))
       (lines '() (cons (readline port) lines)))
      ((eof-object? c)
       (reverse lines))))

; If s1 is a substring of s2, then returns the least integer m
; such that (string=? s1 (substring s2 m (+ m (string-length s1)))).
; Otherwise returns #f.

(define (substring? s1 s2)
  (let ((n1 (string-length s1))
        (n2 (string-length s2)))
    (let ((n (- n2 n1)))
      (let loop ((m 0))
        (if (<= m n)
            (if (substring=? s1 s2 m (+ m n1))
                m
                (loop (+ m 1)))
            #f)))))

(define (substring=? s1 s2 m n)
  (and (<= (string-length s1) (- n m))
       (<= n (string-length s2))
       (do ((i 0 (+ i 1))
            (m m (+ m 1)))
           ((or (= m n)
                (not (char=? (string-ref s1 i)
                             (string-ref s2 m))))
            (= m n)))))

(define (right-justify x n . port)
  (let ((p (open-output-string))
        (port (if (null? port) (current-output-port) (car port))))
    (display x p)
    (let* ((s (get-output-string p))
           (m (string-length s)))
      (if (< m n)
          (display (string-append (make-string (- n m) #\space) s) port)
          (display (substring s 0 n) port)))))

(define (left-justify x n . port)
  (let ((p (open-output-string))
        (port (if (null? port) (current-output-port) (car port))))
    (display x p)
    (let* ((s (get-output-string p))
           (m (string-length s)))
      (if (< m n)
          (display (string-append s (make-string (- n m) #\space)) port)
          (display (substring s 0 n) port)))))

; Given a string that contains a timing in hours:minutes:seconds,
; returns the timing in milliseconds.

(define (string->msec s)
  (let* ((s0 (list->string
              (filter (lambda (c) (not (char=? c #\space)))
                        (string->list s))))
         (n1 (substring? ":" s0))
         (s1 (if n1 (substring s0 0 n1) "0"))
         (s0 (if n1 (substring s0 (+ n1 1) (string-length s0)) s0))
         (n2 (substring? ":" s0))
         (s2 (if n2 (substring s0 0 n2) "0"))
         (s0 (if n2 (substring s0 (+ n2 1) (string-length s0)) s0))
         (hours   (if (and n1 n2) (string->number s1) 0))
         (minutes (cond ((and n1 n2) (string->number s2))
                        (n1 (string->number s1))
                        (else 0)))
         (seconds (string->number s0))
         (seconds (+ (* 3600 hours) (* 60 minutes) seconds)))
    (inexact->exact (round (* 1000.0 seconds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Summarizing the results.* files that are created by the bench
; script.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (summarize f)
  (define (summarize in . rest)
    (define (bad-arguments)
      (error "Bad arguments to summarize-results"
             (cons in rest)))
    (cond ((string? in)
           (call-with-input-file
            in
            (lambda (in) (apply summarize in rest))))
          ((input-port? in)
           (cond ((null? rest)
                  (summarize in (current-output-port)))
                 ((string? (car rest))
                  (call-with-output-file
                   (car rest)
                   (lambda (out)
                     (summarize in out))))
                 ((output-port? (car rest))
                  (f (readlines in) (car rest)))
                 (else
                  (bad-arguments))))
          (else
           (bad-arguments))))
  summarize)

; Gambit-C

(define (gambit-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under Gambit-C")
        (cpu-key " ms cpu time")
        (real-key " ms real time")
        (gc-key " collections accounting for ")
        (ms-key " ms")
        (space-key "    ")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-gc-key (string-length gc-key))
          (n-ms-key (string-length ms-key))
          (n-space-key (string-length space-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines)
                 (real ""))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display "benchmark                  cpu      real        gc"
                              out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring? gc-key line)
                     (let ((x (substring line
                                         (+ n-gc-key (substring? gc-key line))
                                         (substring? ms-key line))))
                       (right-justify x timing-width out)
                       (newline out)))
                    ((substring? cpu-key line)
                     (let ((x (substring line
                                         n-space-key
                                         (substring? cpu-key line))))
                       (right-justify x timing-width out)
                       (right-justify real timing-width out)))
                    ((substring? real-key line)
                     (let ((x (substring line
                                         n-space-key
                                         (substring? real-key line))))
                       (set! real x)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display "    " out)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines) real)))))))

; Chez Scheme.

(define (chez-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under Chez-Scheme")
        (cpu-key " ms elapsed cpu time")
        (real-key " ms elapsed real time")
        (space-key "    ")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-space-key (string-length space-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display "benchmark                  cpu      real" out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring? cpu-key line)
                     (let ((x (substring line
                                         n-space-key
                                         (substring? cpu-key line))))
                       (right-justify x timing-width out)))
                    ((substring? real-key line)
                     (let ((x (substring line
                                         n-space-key
                                         (substring? real-key line))))
                       (right-justify x timing-width out)
                       (newline out)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display "    " out)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines))))))))

; Larceny

(define (larceny-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under Larceny")
        (cpu-key "User: ")
        (real-key "Elapsed time...: ")
        (gc-key "Elapsed GC time: ")
        (ms-key " ms")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-gc-key (string-length gc-key))
          (n-ms-key (string-length ms-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display
                      "benchmark                  cpu      real        gc"
                      out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring=? real-key line 0 n-real-key)
                     (let* ((n1 (substring? ms-key line))
                            (n2 (substring? cpu-key line))
                            (s  (substring line n2 (string-length line)))
                            (n3 (substring? ms-key s))
                            (x (substring line n-real-key n1))
                            (y (substring s n-cpu-key n3)))
                       (right-justify y timing-width out)
                       (right-justify x timing-width out)))
                    ((substring=? gc-key line 0 n-gc-key)
                     (let* ((n1 (substring? ms-key line))
                            (x (substring line n-gc-key n1)))
                       (right-justify x timing-width out)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines))))))))

; Chicken

(define (chicken-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under Chicken")
        (cpu-key "user ")
        (real-key " seconds elapsed")
        (gc-key " seconds in (major) GC")
        (ms-key " ms")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-gc-key (string-length gc-key))
          (n-ms-key (string-length ms-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display
                      "benchmark                  cpu      real        gc"
                      out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring? real-key line)
                     (let* ((n1 (substring? real-key line))
                            (s  (substring line 0 n1))
                            (s1 (list->string
                                 (filter (lambda (c) (not (char=? c #\space)))
                                         (string->list s))))
                            (x  (string->msec s1)))
                       (right-justify x timing-width out)
                       (right-justify x timing-width out)))
                    ((substring? gc-key line)
                     (let* ((n1 (substring? gc-key line))
                            (s  (substring line 0 n1))
                            (s1 (list->string
                                 (filter (lambda (c) (not (char=? c #\space)))
                                         (string->list s))))
                            (x  (string->msec s1)))
                       (right-justify x timing-width out)
                       (newline out)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines))))))))

; MzScheme

(define (mzscheme-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under MzScheme")
        (cpu-key "cpu time: ")
        (real-key "real time: ")
        (gc-key "gc time: ")
        (ms-key " ms")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-gc-key (string-length gc-key))
          (n-ms-key (string-length ms-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display
                      "benchmark                  cpu      real        gc"
                      out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring=? cpu-key line 0 n-cpu-key)
                     (let* ((n1 (substring? real-key line))
                            (n2 (substring? gc-key line))
                            (s  (substring line n2 (string-length line)))
                            (x (substring line n-cpu-key (- n1 1)))
                            (y (substring line
                                          (+ n1 n-real-key)
                                          (- n2 1)))
                            (z (substring line
                                          (+ n2 n-gc-key)
                                          (string-length line))))
                       (right-justify x timing-width out)
                       (right-justify y timing-width out)
                       (right-justify z timing-width out)
                       (newline out)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines))))))))

; Bigloo

(define (bigloo-results lines out)
  (let ((system-key "Benchmarking ")
        (test-key "Testing ")
        (test-key-tail " under Bigloo")
        (run-key "Running...")
        (cpu-key "user ")
        (real-key "real ")
        (error-key "Error: ")
        (wrong-key "*** wrong result ***"))
    (let ((n-system-key (string-length system-key))
          (n-test-key (string-length test-key))
          (n-run-key (string-length run-key))
          (n-cpu-key (string-length cpu-key))
          (n-real-key (string-length real-key))
          (n-error-key (string-length error-key))
          (n-wrong-key (string-length wrong-key))
          (name-width 20)
          (timing-width 10))
      (let loop ((lines lines))
        (if (null? lines)
            (newline out)
            (let ((line (car lines)))
              (cond ((substring=? system-key line 0 n-system-key)
                     (display line out)
                     (newline out)
                     (newline out)
                     (display
                      "benchmark                  cpu      real        gc"
                      out)
                     (newline out))
                    ((substring=? test-key line 0 n-test-key)
                     (newline out)
                     (let ((name (substring line
                                            n-test-key
                                            (substring? test-key-tail line))))
                       (left-justify name name-width out)))
                    ((substring=? run-key line 0 n-run-key)
                     (let* ((lines
                             (do ((lines (cdr lines) (cdr lines)))
                                 ((substring=? real-key
                                               (car lines) 0 n-real-key)
                                  lines)))
                            (line1 (car lines))
                            (line2 (cadr lines))
                            (y (string->msec
                                (substring line1
                                           n-real-key
                                           (string-length line1))))
                            (x (string->msec
                                (substring line2
                                           n-cpu-key
                                           (string-length line2)))))
                       (right-justify x timing-width out)
                       (right-justify y timing-width out)
                       (newline out)))
                    ((substring=? error-key line 0 n-error-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out))
                    ((substring=? wrong-key line 0 n-wrong-key)
                     (display line out)
                     (newline out)
                     (display (make-string name-width #\space) out)))
              (loop (cdr lines))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversion of the summaries into Scheme-readable data.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a file name or input port containing a summary
; produced by the summarize procedure above,
; returns a decoded summary of the form
;
; (<system>                   ; a string, e.g. "Larceny"
;  (<hostname> <date> ...)    ; strings
;  ((<benchmark>              ; a symbol, e.g. fib
;    <cputime>                ; a number, in milliseconds
;    <realtime>               ; a number, in milliseconds
;    <gctime>)                ; a number, in milliseconds
;   ...))

(define (decode-summary in)
  (define (bad-arguments)
    (error "Bad arguments to summarize-results" in))
  (cond ((string? in)
         (call-with-input-file
          in
          (lambda (in) (decode-summary in))))
        ((input-port? in)
         (decode-lines (readlines in)))
        (else
         (bad-arguments))))

; Given the summary as a list of lines,
; returns the decoded summary as for decode-summary.

(define (decode-lines lines)
  (let ((system-key "Benchmarking ")
        (date-key " on ")
        (header-key "benchmark"))
    (let ((n-system-key (string-length system-key))
          (n-date-key (string-length date-key))
          (n-header-key (string-length header-key)))
      (and (not (null? lines))
           (substring=? system-key (car lines) 0 n-system-key))
           (let* ((line0 (car lines))
                  (n0 (string-length line0))
                  (n1 (substring? date-key line0))
                  (system (substring line0 n-system-key n1))
                  (hostname "unknown")
                  (date (substring line0 (+ n1 n-date-key) n0))
                  (benchmarks
                   (map (lambda (line)
                          (let* ((padding " #f #f #f #f")
                                 (in (open-input-string
                                      (string-append line padding)))
                                 (name (read in))
                                 (cpu (read in))
                                 (real (read in))
                                 (gc (read in)))
                            (list name cpu real gc)))
                        (cdr lines)))
                  (benchmarks
                   (filter (lambda (x)
                             (and (car x)
                                  (symbol? (car x))
                                  (not (eq? (car x) 'benchmark))
                                  (number? (cadr x))
                                  (number? (caddr x))
                                  (positive? (caddr x))))
                           benchmarks)))
             (list system
                   (list hostname date)
                   benchmarks)))))
