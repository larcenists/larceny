;;; Jiffies have one-second resolution or better, and are reckoned with
;;; zero jiffies corresponding to the approximate time this library was
;;; loaded.

(define arbitrary-baseline
  (call-with-values
   current-utc-time
   (lambda (secs usecs)
     secs)))

(define current-jiffy
  (case (jiffies-per-second)
   ((1)
    (lambda () (- (current-seconds) arbitrary-baseline)))
   ((1000000)
    (lambda ()
      (call-with-values
       current-utc-time
       current-utc-time-continuation-microseconds)))
   (else
    (lambda ()
      (call-with-values
       current-utc-time
       (lambda (secs usecs)
         (let ((j/s (jiffies-per-second)))
           (+ (* j/s (- secs arbitrary-baseline))
              (quotient (* j/s usecs) 1000000)))))))))

;;; The microsecond case is bummed because Will thought it might matter.
;;;
;;; It didn't matter.  On our fastest machine (3.6 GHz Intel i7-4790):
;;;
;;;     (current-second)    takes about  6 usec (high variance; see below)
;;;     (current-jiffy)     takes about 16 usec (low variance)
;;;                                  or 20 usec (after overflow into bignums)
;;;     (current-seconds)   takes about 50 usec
;;;
;;; (current-second) usually averages about 6 usec, but sometimes
;;; averages about 1.3 usec.  That's an average over 10 calls.
;;; The variance for (current-seconds) is the same as for
;;; (current-second), which makes sense because (current-seconds)
;;; just calls (current-second) and converts the result to an
;;; exact integer.
;;;
;;; Curious fact:  After overflowing into bignums, the first call
;;; to (current-jiffy) in a sequence of 10 takes about 50 usec,
;;; but the next nine calls take 20 usec or slightly less.  Perhaps
;;; a continuation is being captured somewhere.
;;;
;;; Bottom line:  We *might* be able to make (current-jiffy) faster
;;; and more useful for timing small intervals by making it a syscall
;;; instead of going through the FFI.  Not worth the trouble.

(define (current-utc-time-continuation-microseconds secs usecs)
  (+ (* 1000000 (- secs arbitrary-baseline))
     usecs))
