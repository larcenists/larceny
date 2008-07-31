; Copyright 2008 William D Clinger
;
; $Id$
;
; Synthetic benchmark for timing gross behavior of
; Twobit passes.
;
; (time-twobit n)
;     Benchmarks Twobit file-compiling a procedure with n calls.
;     Creates files named temporary.sch and temporary.fasl.
;
; (time-twobit-usual)
;     Calls time-twobit for powers of 2 up to 65536.

(require 'Experimental/twobit-timer)

(define tempfile "temporary.sch")

(define (time-twobit n)
  (newline)
  (newline)
  (display "Benchmark of size ")
  (write n)
  (newline)
  (delete-file tempfile)
  (call-with-output-file
   tempfile
   (lambda (out)
     (write-benchmark n out)))
  (time (begin (reset-twobit-timing!)
               (start-twobit-timing!)
               (compile-file tempfile)
               (stop-twobit-timing!)
               (pretty-print (report-twobit-timing!))))
  (pretty-print (assq 'heap-area-info (system-features)))
  (newline))

(define (write-benchmark n out)
  (define (println s)
    (display s out)
    (newline out))
  (println "(define (f x y z)")
  (do ((i 0 (+ i 1)))
      ((= i n))
    (println "  (g y z x)"))
  (println "  )"))

(define (time-twobit-usual)
  (define usual
    '(2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536))
  (for-each time-twobit usual))
