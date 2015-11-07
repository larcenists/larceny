; Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; Larceny library -- timer interrupts.

($$trace "timer")

; FIXME:  This seems out of place.

(define twobit-timer-hook
  (make-parameter "twobit-timer-hook" #f))

; This is well-behaved but somewhat expensive w.r.t. allocation.

(define (call-without-interrupts thunk)
  (let ((old #f))
    (dynamic-wind 
     (lambda () (set! old (disable-interrupts)))
     thunk
     (lambda () (if old (enable-interrupts old))))))

; FIXME: this won't work with true multiprocessing,
; and might not work even with green threads.
; It works if and only if the critical section is an
; uninterruptible sequence of straight-line code.

(define (vector-like-cas! v i old new)
; (call-without-interrupts
;  (lambda ()

     ; begin critical section

     (let ((x (vector-like-ref v i)))
       (if (eq? x old)
           (vector-like-set! v i new))

     ; end critical section

       x));))

; A timeslice of 50,000 is a compromise between overhead and response time.
; On atlas.ccs.neu.edu (a SPARC 10 (?)), 50,000 is really too much for
; longer sections of straight-line code yet too little for very branch-
; or call-intensive programs.  See Util/timeslice.sch for details.
;
; The _right_ solution is probably the following:
;  * the scheduler uses priorities
;  * higher-priority jobs get longer time slices
;  * interactive input tasks (mouse, keyboard) get to interrupt
;    whatever task is running.
;  * run-benchmark gets to ask for a very long time slice.
;
; Since the scheduler will run at a different level from this low-level
; RTS code, the 50,000 is an OK compromise for this level.
;
; (Alternatively, the timer has some arbitrary value but that value is
; chunked into pieces of 10,000, say, maintained by millicode.  Each 10K
; decrements a quick trip is made into millicode to check for interrupts
; and get the next timer chunk.)

(define *standard-timeslice* 50000)       ; Empirical.

(define standard-timeslice
  (make-parameter "standard-timeslice"
                  *standard-timeslice* 
                  (lambda (x)
                    (and (fixnum? x) (> x 0)))))

; eof
