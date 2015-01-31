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

