;;; Jiffies have one-second resolution, and are reckoned with
;;; zero jiffies corresponding to the time this library was loaded.

(define (jiffies-per-second) 1)

(define jiffies-per-second-as-float (inexact (jiffies-per-second)))

(define arbitrary-basetime (current-second))

(define (current-jiffy)
  (let* ((t (current-second))
         (j (- t arbitrary-basetime))
         (j (* jiffies-per-second-as-float j)))
    (exact (round j))))

