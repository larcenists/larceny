; Copyright 1999 Lars T Hansen.
;
; $Id$
;
; Simple multitasking for Larceny.
;
; WITH-TASKING thunk => object
;   Starts tasking and runs thunk in the initial thread.  The value
;   returned is the value passed to END-TASKING, if any.
;
; END-TASKING [v]
;   Returns from WITH-TASKING, returning the value v.
;
; SPAWN thunk [name] => task
;   Create and schedule a new task, and return that task.  If name is
;   specified, use this to name the task (otherwise it is just given a
;   numeric task ID).  When the thunk returns, the task is killed
;
; KILL task
;   Kill the task immediately
;
; YIELD
;   Surrender the timeslice; the current task is placed on the run queue
;
; BLOCK task
;   Remove task from the run queue
;
; UNBLOCK task
;   Place task on the run queue
;
; CURRENT-TASK  =>  task
;   Return the active task
;
; ALL-TASKS  =>  (task ...)
;   Return a list of all non-dead tasks
;
; TASK? obj  =>  boolean
;   Tests an object for taskness
;
; WITHOUT-INTERRUPTS e1 e2 ...  (Syntax)
;   Evaluate the expressions in a critical section
;
;
; Observe the following restrictions.
;
; If code in the dynamic scope of WITH-TASKING throws to a
; continuation captured outside that scope, then tasking is
; terminated, and an attempt to throw from outside the dynamic scope
; of WITH-TASKING into the scope will cause an error to be signalled.
;
; I/O and other system calls in Larceny are not tasking-aware, so if a
; task calls eg READ and that call blocks, then the entire tasking
; system blocks.  (A tasking system without these problems can be
; built on this package along with the experimental nonblocking I/O
; system.)

(require 'assert)
(require 'queue)
(require 'define-record)

; Interface

(define-syntax without-interrupts       ; Critical section
  (syntax-rules ()
    ((without-interrupts E0 E1 ...)
     (call-without-interrupts
       (lambda () E0 E1 ...)))))

(define with-tasking)                   ; (with-tasking thunk) => v
(define end-tasking)                    ; (end-tasking [v])
(define spawn)                          ; (spawn thunk [name]) => task
(define kill)                           ; (kill task)
(define yield)                          ; (yield)
(define block)                          ; (block task)
(define unblock)                        ; (unblock task)
(define current-task)                   ; (current-task) => task
(define all-tasks)                      ; (all-tasks) => (task ...)
(define task?)                          ; (task? obj) => boolean

; Overridable callbacks (for more sophisticated thread systems)

(define idle-handler)			; (idle-handler)
(define poll-handler)			; (poll-handler)
(define task-state-changed-handler)     ; (task-state-changed-handler t)

; Implementation

(define *tasking-on* #f)		; #t in dynamic scope of with-tasking
(define *tasking-exit-k*)		; To leave tasking
(define *tasking-schedule-k*)    	; To enter scheduler
(define *run-queue*)                    ; Queue of scheduled tasks
(define *current-task*)                 ; Current task
(define *all-tasks*)			; List of all non-dead tasks
(define *saved-interrupt-handler*)	; To restore state 
(define *saved-timeslice*)		;   when done tasking
(define *poll-counter*)			; Countdown for polling

; Constants

(define *timeslice* 5000)               ; Perhaps low on modern HW.
(define *poll-frequency* 10)            ; Poll every this many switches

; System-level critical section does not use dynamic-wind.

(define-syntax tasks/without-interrupts
  (syntax-rules ()
    ((tasks/without-interrupts E0 E1 ...)
     (let ((x (disable-interrupts)))
       (let ((r (begin E0 E1 ...)))
         (if x (enable-interrupts x))
         r)))))

(define (with-tasking thunk)
  (assert (not *tasking-on*))
  (call-with-current-continuation
   (lambda (k)
     (let ((valid #t))
       (set! *tasking-exit-k* k)
       (dynamic-wind
	   (lambda () 
	     (if (not valid)
		 (error "Attempting to throw into a tasking continuation")))
	   (lambda () 
	     (tasks/start-tasking thunk))
	   (lambda () 
	     (set! valid #f)
	     (tasks/stop-tasking)))))))

(define (end-tasking . rest)
  (assert *tasking-on*)
  (*tasking-exit-k* (if (not (null? rest)) (car rest))))

(define (current-task) 
  (assert *tasking-on*)
  *current-task*)

(define (all-tasks)
  (assert *tasking-on*)
  (tasks/without-interrupts
   (list-copy *all-tasks*)))

(define (spawn thunk . rest)
  (let ((name (if (null? rest) #f (car rest))))
    (assert *tasking-on*)
    (tasks/without-interrupts
     (tasks/schedule (tasks/create-task thunk name)))))

(define (kill t)
  (assert *tasking-on*)
  (assert (task? t))
  (tasks/without-interrupts
   (tasks/end-task t)))

(define (yield)
  (assert *tasking-on*)
  (let ((critical? (tasks/in-critical-section?)))
    (tasks/without-interrupts
     (tasks/switch #t critical?))))

(define (block t)
  (assert *tasking-on*)
  (assert (task? t))
  (let ((critical? (tasks/in-critical-section?)))
    (tasks/without-interrupts
     (let ((ts (task-state t)))
       (task-state-set! t 'blocked)
       (case ts
	 ((runnable) (queue-remove! *run-queue* t))
	 ((running)  (tasks/switch #f critical?)))))))

(define (unblock t)
  (assert *tasking-on*)
  (assert (task? t))
  (tasks/without-interrupts
   (if (eq? (task-state t) 'blocked)
       (tasks/schedule t))))

; Overridable.  IDLE-HANDLER can eg block on a SELECT so that we avoid
; busy-waiting for I/O.  The default is to kill the tasking system,
; since there is no platform-independent way of getting out of an
; idling state.

(define idle-handler
  (lambda ()
    (newline)
    (display "; The run queue is empty.  Ending tasking.")
    (newline)
    (end-tasking)))

; Overridable.  POLL-HANDLER can eg poll for I/O and unblock tasks
; that have pending I/O.

(define poll-handler
  (lambda ()
    #t))

; Overridable.  TASK-STATE-CHANGED-HANDLER is called after the task
; has changed state and can take action for the task, eg, if task has
; been killed it can remove the task from data structures.

(define task-state-changed-handler
  (lambda (t)
    #t))

; Invariants:
;  * state is one of
;      new         -- not yet started
;      runnable    -- on run queue
;      running     -- only the current task
;      blocked     -- victim of a block
;      dead        -- exited or was killed
;  * critical is #t if task must be rescheduled with interrupts disabled
;  * the current task is never on the run queue.
;  * dead tasks are never on *all-tasks*

(define-record task (k state critical id))

(define task-state-set
  (let ((task-state-set! task-state-set!))
    (lambda (t s)
      (task-state-set! t s)
      (task-state-changed-handler t))))

(define make-task 
  (let ((make-task make-task)
        (id 0))
    (lambda (thunk name)
      (if (not name)
	  (begin
	    (set! id (+ id 1))
	    (set! name id)))
      (make-task (lambda ()
                   (thunk)
                   (tasks/without-interrupts 
		    (tasks/end-task *current-task*)))
		 'new
		 #f
                 name))))

((record-updater (record-type-descriptor task) 'printer) 
 task 
 (lambda (t out) 
   (display "#<task \"" out)
   (display (task-id t) out) 
   (display "\">" out)))

; Scheduler.  Call only with interrupts turned off.
;
; It's vital that errors are never signalled here without interrupts
; being turned on first.

(define (tasks/start-tasking thunk)
  (disable-interrupts)
  (set! *tasking-on* #t)
  (set! *saved-interrupt-handler* (timer-interrupt-handler))
  (set! *saved-timeslice* (standard-timeslice))
  (standard-timeslice *timeslice*)
  (timer-interrupt-handler
   (lambda ()
     (tasks/switch #t #f)))
  (set! *current-task* #f)
  (set! *run-queue* (make-queue))
  (set! *all-tasks* '())
  (set! *poll-counter* *poll-frequency*)
  (enable-interrupts (standard-timeslice))
  (spawn thunk "initial-task")

  ; Scheduler

  (call-with-current-continuation
   (lambda (k)
     (set! *tasking-schedule-k* k)))

  (set! *poll-counter* (- *poll-counter* 1))
  (if (zero? *poll-counter*)
      (begin (set! *poll-counter* *poll-frequency*)
	     (poll-handler)))

  (let loop ()
    (if (queue-empty? *run-queue*)
	(begin (idle-handler)
	       (loop))))

  (let ((t (queue-get! *run-queue*)))
    (set! *current-task* t)
    (task-state-set! t 'running)
    (if (task-critical t)
        (begin (task-critical-set! t #f)
               (enable-interrupts (standard-timeslice)) ; Set time slice
               (disable-interrupts))
        (enable-interrupts (standard-timeslice)))
    ((task-k t))))

(define (tasks/stop-tasking)
  (disable-interrupts)
  (set! *tasking-on* #f)
  (timer-interrupt-handler *saved-interrupt-handler*)
  (standard-timeslice *saved-timeslice*)
  (enable-interrupts (standard-timeslice)))

(define (tasks/end-task t)
  (let ((ts (task-state t)))
    (task-state-set! t 'dead)
    (set! *all-tasks* (remq! t *all-tasks*))
    (case ts
      ((runnable)
       (queue-remove! *run-queue* t))
      ((running) 
       (tasks/switch #f #f)))))

(define (tasks/create-task thunk name)
  (let ((t (make-task 
	    (lambda ()
	      (parameterize ((reset-handler 
			      (lambda () 
				(tasks/without-interrupts
				 (tasks/end-task *current-task*)))))
		(thunk)))
	    name)))
    (set! *all-tasks* (cons t *all-tasks*))
    t))

(define (tasks/schedule t)
  (assert (not (eq? 'dead (task-state t))))
  (task-state-set! t 'runnable)
  (queue-put! *run-queue* t))

(define (tasks/switch reschedule-current? in-critical-section?)
  (call-with-current-continuation
   (lambda (k)
     (let ((t *current-task*))
       (task-k-set! t (lambda () (k #f)))
       (task-critical-set! t in-critical-section?)
       (if reschedule-current?
	   (tasks/schedule t))
       (*tasking-schedule-k* #t)))))

(define (tasks/in-critical-section?)
  (let ((x (disable-interrupts)))
    (if x (enable-interrupts x))
    (not x)))

; eof
