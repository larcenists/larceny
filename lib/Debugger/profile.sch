; Copyright 2008 Felix S Klock II
;
; $Id$
;
; Extremely rough but slightly useful prototype of procedure
; profiling.
;
; Usage:
; (run-with-profiling <thunk>)
; (profile <expr>)
;
; The idea is to generate an interrupt at reasonably regular
; intervals and record:
;     the named procedure that is currently awaiting a value
;     all named procedures that are currently awaiting values

(require 'inspect-cont)

(define (run-with-profiling thunk)
  (if (not (procedure? thunk))
      (assertion-violation 'run-with-profiling
                           "argument should be thunk"
                           thunk))
  (reset-profiler!)
  (start-profiler!)
  (let ((r (thunk)))
    (stop-profiler!)
    (report-profiler!)
    r))

(define-syntax profile
  (syntax-rules ()
   ((_ expr)
    (run-with-profiling (lambda () expr)))))

(define original-tih (timer-interrupt-handler))

(define (restore-original-tih!)
  (timer-interrupt-handler original-tih))

(define *profiler-top-procs* '())  ; alist of (<symbol> . <count>) pairs
(define *profiler-all-procs* '())  ; alist of (<symbol> . <count>) pairs
(define *profiler-interrupts* 0)

(define (reset-profiler!)
  (restore-original-tih!)
  (set! *profiler-top-procs* '())
  (set! *profiler-all-procs* '())
  (set! *profiler-interrupts* 0)
  (unspecified))

(define (stop-profiler!)
  (restore-original-tih!))

(define (start-profiler!)
  (install-profiler! 100000 100))

(define (report-profiler!)
  (let* ((more? (lambda (x y) (> (cdr x) (cdr y))))
         (top (list-sort more? *profiler-top-procs*))
         (all (list-sort more? *profiler-all-procs*))
         (n (+ 0.0 (max 1 *profiler-interrupts*))))
    (define (percent j)
      (inexact->exact (round (/ (* 100.0 j) n))))
    (define (show name j)
      (write (percent j))
      (display "  ")
      (write name)
      (newline))
    (display " %  topmost named procedure")
    (newline)
    (for-each show (map car top) (map cdr top))
    (newline)
    (display " %  active procedures")
    (newline)
    (for-each show (map car all) (map cdr all))
    (newline)))

(define (process-cont! el rendered-cont)
  (define (remove-duplicates stk)
    (do ((rstk (reverse stk) (cdr rstk))
         (stk '()
              (if (memq (car rstk) (cdr rstk))
                  stk
                  (cons (car rstk) stk))))
        ((null? rstk) stk)))
  (set! xxx rendered-cont)
  (set! *profiler-interrupts*
        (+ 1 *profiler-interrupts*))
  (let* ((stk rendered-cont)
         (stk (map (lambda (x) (if (pair? x) (car x) x))
                   stk))
         (stk (filter symbol? stk))
         (stk (remove-duplicates stk)))
    (if (pair? stk)
        (let* ((top (car stk))
               (probe (assq top *profiler-top-procs*)))
          (if probe
              (set-cdr! probe (+ 1 (cdr probe)))
              (set! *profiler-top-procs*
                    (cons (cons top 1)
                          *profiler-top-procs*)))))
    (for-each (lambda (sym)
                (let ((probe (assq sym *profiler-all-procs*)))
                  (if probe
                      (set-cdr! probe (+ 1 (cdr probe)))
                      (set! *profiler-all-procs*
                            (cons (cons sym 1)
                                  *profiler-all-procs*)))))
              stk)))

(define (extend-interrupt-handler/profiling old-tih min-delta)
  (let ((last-el 0))
    (lambda l 
      (let ((el (memstats-elapsed-time (memstats))))
        ;; Compenstate for irregular interrupt measurement;
        ;; if we haven't seen the system clock tick, then 
        ;; we should let the system make more progress.
        (cond ((>= (- el last-el) min-delta)
               (let* ((cc (current-continuation-structure))
                      (i (make-continuation-inspector cc))
                      (rendered-cont (render-cont i))
                      (el (memstats-elapsed-time (memstats))))
                 (process-cont! el rendered-cont)
                 (set! last-el el))))
        (apply old-tih l)))))

; Usage:
; (install-profiler! <granularity> <delta>)
;
; On a 1.5 GHz SPARC,   1000000      90
; will give roughly 10 interrupts per second.
  
(define (install-profiler! . args)
  (if (not (null? args))
      (set-timer-granularity! (car args)))
  (timer-interrupt-handler 
   (let ((old-tih (timer-interrupt-handler))
         (min-delta (if (and (not (null? args))
                             (not (null? (cdr args))))
                        (cadr args)
                        1)))
     (extend-interrupt-handler/profiling old-tih min-delta))))

;; The standard timer-interrupt-handler just turns interrupts back on
;; with (standard-timeslice) as the argument; this procedure allows
;; the user to set their own grain.
(define (set-timer-granularity! grain)
  (timer-interrupt-handler
   (lambda () (enable-interrupts grain)))
  (enable-interrupts grain))

(define (render-cont i)
  (define (show val cnt)
    (cond ((= cnt 1)
           val)
          (else
           (list val '* cnt))))
  (let loop ((prev (unspecified))
             (pcnt 0))
    (cond ((not (i 'probe-down)) 
           (list (show prev pcnt)))
          (else (i 'down)
                (let* ((info (((i 'get) 'code) 'procedure))
                       (name (cond ((procedure? info) (procedure-name info))
                                   (else info))))
                  (cond ((eq? name prev)
                         (loop name (+ pcnt 1)))
                        ((> pcnt 0)
                         (cons (show prev pcnt) (loop name 1)))
                        (else
                         (loop name 1))))))))

'
(define (process-cont! el rendered-cont)
  (display `(time: ,el cont: ,rendered-cont))
  (newline))
