; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Tasking system extensions for Unix, supporting nonblocking I/O and 
; a nonblocking console.
;
; You must load tasking.sch first because this file captures and 
; overrides some procedures from that file.

(define-syntax tasks/without-interrupts    ; Unchanged
  (syntax-rules ()
    ((tasks/without-interrupts E0 E1 ...)
     (let ((x (disable-interrupts)))
       (let ((r (begin E0 E1 ...)))
         (if x (enable-interrupts x))
         r)))))

(define *end-tasking-continuation* #f)
(define *poll-interval* 10)
(define *poll-counter* *poll-interval*)
(define *poll-input* '())               ; ((fd . task) ...)
(define *poll-output* '())              ; ((fd . task) ...)

(define begin-tasking
  (let ((begin-tasking begin-tasking)
        (end-tasking end-tasking))
    (lambda ()
      (if *tasking-on* (error "Tasking is already on."))
      (fluid-let ((console-input-port nonblocking-console-input-port)
                  (console-output-port nonblocking-console-output-port))
        (call-with-current-continuation
         (lambda (k)
           (set! *end-tasking-continuation* k)
           (begin-tasking)
           (repl)))
        (end-tasking)))))

(define (end-tasking)
  (if (not *tasking-on*) (error "Tasking is not on."))
  (tasks/without-interrupts
   (*end-tasking-continuation* #t)))

; Called in critical section.

(define tasks/scheduler
  (let ((tasks/scheduler tasks/scheduler))
    (lambda ()
      (set! *poll-counter* (- *poll-counter* 1))
      (if (or (zero? *poll-counter*) (run-queue.empty? *run-queue*))
          (begin
            (set! *poll-counter* *poll-interval*)
            (tasks/poll-for-io (run-queue.empty? *run-queue*))))
      (tasks/scheduler))))

(define (tasks/poll-for-io block-system?)
  (let ((ready (poll-descriptors (map car *poll-input*)
                                 (map car *poll-output*)
                                 (if block-system? -1 0))))
    (do ((ready ready (cdr ready)))
        ((null? ready))
      (cond ((assq (car ready) *poll-input*) 
             => (lambda (x)
                  (set! *poll-input* (remq! x *poll-input*))
                  (tasks/schedule (cdr x))))
            ((assq (car ready) *poll-output*)
             => (lambda (x)
                  (set! *poll-output* (remq! x *poll-output*))
                  (tasks/schedule (cdr x))))
            (else
             (error "Internal error in tasks/poll-for-io: " (car ready)))))))

; May be called outside critical section.

(define (unix-tasks/block-for-input fd)
  (tasks/without-interrupts
   (set! *poll-input* (cons (cons fd (current-task)) *poll-input*))
   (block (current-task))))

(define (unix-tasks/block-for-output fd)
  (tasks/without-interrupts
   (set! *poll-output* (cons (cons fd (current-task)) *poll-output*))
   (block (current-task))))

; eof
