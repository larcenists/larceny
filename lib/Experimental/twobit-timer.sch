; Copyright 2008 William D Clinger
;
; $Id$
;
; Cumulative timing for the phases of Twobit.
;
; (start-twobit-timing!)
;     Resumes timing of Twobit phases.
;
; (stop-twobit-timing!)
;     Halts timing of Twobit phases.
;
; (reset-twobit-timing!)
;     Resets counters to 0.
;
; (report-twobit-timing!)
;     Displays a report to (current-output-port).

; Uses twobit-timer-hook, which is defined in
; Compiler/driver-twobit.sch and Lib/Common/timer.sch
; and called from Compiler/pass4.aux.sch and
; Asm/Shared/pass5p1.sch

(define (start-twobit-timing!)
  (twobit-timer-hook the-twobit-timer))

(define (stop-twobit-timing!)
  (set! *twobit-timing-stack* '())
  (twobit-timer-hook #f))

(define (reset-twobit-timing!)
  (set! *twobit-timing-counters* '())
  (set! *twobit-timing-stack* '())
  (unspecified))

(define (report-twobit-timing!)
  (pretty-print (reverse *twobit-timing-counters*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *twobit-timing-counters* '())

(define *twobit-timing-stack* '())

(define (usual-twobit-timer phase event . rest)
  (let ((counter (assq phase *twobit-timing-counters*)))
    (if (not counter)
        (begin (set! *twobit-timing-counters*
                     (cons (cons phase 0) *twobit-timing-counters*))
               (usual-twobit-timer phase event))
        (case event
         ((begin)
          (set! *twobit-timing-stack*
                (cons (cons phase (current-cpu-time))
                      *twobit-timing-stack*)))
         ((end)
          (let* ((t (current-cpu-time))
                 (entry (if (pair? *twobit-timing-stack*)
                            (car *twobit-timing-stack*)
                            #f)))
            
            (if entry
                (begin (set! *twobit-timing-stack*
                             (cdr *twobit-timing-stack*))
                       (set-cdr! counter
                                 (+ (cdr counter)
                                    (- t (cdr entry)))))
                (set! *twobit-timing-stack* '()))))))))

(define (current-cpu-time)
  (memstats-user-time (memstats)))

(define the-twobit-timer usual-twobit-timer)

(start-twobit-timing!)
