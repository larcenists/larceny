; Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; Larceny library -- timer interrupts.

($$trace "time")

;;; From the old timer.sch, where they seemed out of place.

(define (current-second)
  (flonum:time))

(define (current-seconds)
  (inexact->exact (round (current-second))))

;;; The monotonic clock in nanoseconds, a fixnum, or #f where there is
;;; none that fits (32-bit systems).  See (scheme time).

(define (larceny:monotonic-nanoseconds)
  (%syscall syscall:monotonic-ns))

