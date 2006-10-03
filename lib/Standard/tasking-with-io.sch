; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Tasking system extensions supporting nonblocking I/O and a
; nonblocking console.  This code is not platform-specific.
;
; It isn't necessary to use the nonblocking console here if your
; program does not require interactive user input from the console,
; and does not invoke the repl.  Just comment out the REQUIRE and the
; parameterization of the factories in WITH-TASKING.

(require 'list)
(require 'tasking)
(require 'poll)
(require 'nonblocking-console)

(define *blocked-on-input* '())		; ((desc . task) ...)
(define *blocked-on-output* '())	; ((desc . task) ...)

(define with-tasking
  (let ((with-tasking with-tasking))
    (lambda (thunk)
      (parameterize ((console-input-port-factory 
		      nonblocking-console-input-port)
		     (console-output-port-factory 
		      nonblocking-console-output-port))
        (parameterize ((current-input-port ((console-input-port-factory)))
		       (current-output-port ((console-output-port-factory))))
          (with-tasking thunk))))))

(define (input-not-ready-handler desc)
  (tasks/without-interrupts
   (set! *blocked-on-input* 
	 (cons (cons desc (current-task)) *blocked-on-input*))
   (block (current-task))))

(define (output-not-ready-handler desc)
  (tasks/without-interrupts
   (set! *blocked-on-output* 
	 (cons (cons desc (current-task)) *blocked-on-output*))
   (block (current-task))))

; The following are all run inside a critical section.

(define idle-handler
  (lambda ()
    (if (and (null? *blocked-on-input*) (null? *blocked-on-output*))
	(begin
	  (newline)
	  (display "; The run queue is empty and no tasks are blocked on I/O")
	  (newline)
	  (display "; Ending tasking")
	  (newline)
	  (end-tasking))
	(tasks/poll-for-io #t))))

(define poll-handler
  (lambda ()
    (tasks/poll-for-io #f)))

(define task-state-changed-handler
  (lambda (t)
    (case (task-state t)
      ((runnable running dead)
       (tasks/remove-from-ioblock-if-blocked t)))))

(define (tasks/poll-for-io block-system?)
  (let ((ready (poll-descriptors (map car *blocked-on-input*)
                                 (map car *blocked-on-output*)
				 block-system?)))
    (do ((ready ready (cdr ready)))
        ((null? ready))
      (cond ((assq (car ready) *blocked-on-input*)
	     => (lambda (probe)
		  (set! *blocked-on-input* (remq! probe *blocked-on-input*))
		  (unblock (cdr probe))))
	    ((assq (assq (car ready) *blocked-on-output*))
	     => (lambda (probe)
		  (set! *blocked-on-output* (remq! probe *blocked-on-output*))
		  (unblock (cdr probe))))
	    (else
	     (error "I/O signalled ready on unblocked task"))))))

(define (tasks/remove-from-ioblock-if-blocked task)
  (cond ((reverse-assq task *blocked-on-input*)
	 => (lambda (x)
	      (set! *blocked-on-input* (remq! x *blocked-on-input*))))
	((reverse-assq task *blocked-on-output*)
	 (lambda (x)
	   (set! *blocked-on-output* (remq! x *blocked-on-output*))))))

; eof
