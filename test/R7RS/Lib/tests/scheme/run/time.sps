(import (scheme base)
        (scheme write)
        (tests scheme time)
        (tests scheme test))

(cond-expand
 ((and kawa
       (library (scheme process-context)))
  (import (scheme process-context))
  (begin (define (write-result x)
           (if (get-environment-variable "VERBOSE")
               (write-megaloops x)))))
 (else
  (begin (define (write-result x)
           (write-megaloops x)))))

(define (write-megaloops loops/s)
  (write (round (/ loops/s 1e6)))
  (display " megaloops/s\n"))

(display "Running tests for (scheme time)\n")
(write-result (run-time-tests))
(report-test-results)
