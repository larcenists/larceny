;;; FIXME:  Although this is R7RS-conformant, it isn't very good.
;;; We could provide millisecond resolution via memstats, but
;;; memstats does an awful lot of allocation, and the whole point
;;; of using jiffies is to avoid excessive allocation.

(define (current-second)
  (inexact (current-seconds)))

(define (current-jiffy)
  (current-seconds))

(define (jiffies-per-second) 1)
