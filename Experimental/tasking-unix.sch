; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Tasking system extensions for Unix, supporting nonblocking I/O and 
; a nonblocking console.

(require 'list)
(require 'experimental/poll)
(require 'experimental/nonblocking-console)
(require 'experimental/tasking)

(define-syntax tasks/without-interrupts    ; Unchanged
  (syntax-rules ()
    ((tasks/without-interrupts E0 E1 ...)
     (let ((x (disable-interrupts)))
       (let ((r (begin E0 E1 ...)))
         (if x (enable-interrupts x))
         r)))))

(define *old-console-input-port-factory* #f)
(define *old-console-output-port-factory* #f)
(define *poll-interval* 10)
(define *poll-counter* *poll-interval*)
(define *poll-input* '())               ; ((fd . task) ...)
(define *poll-output* '())              ; ((fd . task) ...)

(define begin-tasking
  (let ((begin-tasking begin-tasking))
    (lambda ()
      (if *tasking-on* (error "Tasking is already on."))
      (disable-interrupts)
      (set! *old-console-input-port-factory* (console-input-port-factory))
      (set! *old-console-output-port-factory* (console-output-port-factory))
      (console-input-port-factory nonblocking-console-input-port)
      (console-output-port-factory nonblocking-console-output-port)
      (begin-tasking))))

(define end-tasking
  (let ((end-tasking end-tasking))
    (lambda ()
      (if (not *tasking-on*) (error "Tasking is not on."))
      (disable-interrupts)
      (console-input-port-factory *old-console-input-port-factory*)
      (console-output-port-factory *old-console-output-port-factory*)
      (end-tasking))))

(define kill
  (let ((kill kill))
    (lambda (task)
      (if (not *tasking-on*) (error "Tasking is not on."))
      (if (not (task? task)) (error "KILL: " task " is not a task."))
      (tasking/remove-from-ioblock-if-blocked task)
      (kill task))))

(define unblock
  (let ((unblock unblock))
    (lambda (task)
      (if (not *tasking-on*) (error "Tasking is not on."))
      (if (not (task? task)) (error "UNBLOCK: " task " is not a task."))
      (tasking/remove-from-ioblock-if-blocked task)
      (unblock task))))

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
    ; Tasks may have been killed while waiting
    (do ((ready ready (cdr ready)))
        ((null? ready))
      (let ((probe (assq (car ready) *poll-input*)))
	(if (and probe (task-alive (cdr probe)))
	    (begin
	      (set! *poll-input* (remq! probe *poll-input*))
	      (tasks/schedule (cdr probe)))
	    (let ((probe (assq (car ready) *poll-output*)))
	      (if (and probe (task-alive (cdr probe)))
		  (begin
		    (set! *poll-output* (remq! probe *poll-output*))
		    (tasks/schedule (cdr probe))))))))))

; May be called outside critical section.

(define (tasking/remove-from-ioblock-if-blocked task)
  (tasks/without-interrupts
   (cond ((reverse-assq task *poll-input*)
          => (lambda (x)
               (set! *poll-input* (remq! x *poll-input*))))
         ((reverse-assq task *poll-output*)
          (lambda (x)
            (set! *poll-output* (remq! x *poll-output*)))))))

(define (unix-tasks/block-for-input fd)
  (tasks/without-interrupts
   (set! *poll-input* (cons (cons fd (current-task)) *poll-input*))
   (block (current-task))))

(define (unix-tasks/block-for-output fd)
  (tasks/without-interrupts
   (set! *poll-output* (cons (cons fd (current-task)) *poll-output*))
   (block (current-task))))

; eof
