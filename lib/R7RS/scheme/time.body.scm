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
       (lambda (secs usecs)
         (+ (* 1000000 (- secs arbitrary-baseline))
            usecs)))))
   (else
    (lambda ()
      (call-with-values
       current-utc-time
       (lambda (secs usecs)
         (let ((j/s (jiffies-per-second)))
           (+ (* j/s (- secs arbitrary-baseline))
              (quotient (* j/s usecs) 1000000)))))))))
