; Copyright 1999 Lars T Hansen
;
; $Id$
;
; BUG: server does not close server socket when it is stopped.  For the
; other threads, this  program simulates orderly shutdown, but poorly -- 
; effectively a cooperative SIGHUP.  A better solution must be found to 
; signal threads.  (See Thread.AlertWait() in Modula-3.)
;
; BUG: killing waiting threads causes the threads system to crash; this
; is a known bug in that system (see tasking-unix.sch).  Thus stop-server
; should not be called.

; You must call begin-tasking before calling start-server.

(require 'experimental/unix-descriptor)
(require 'experimental/iosys)
(require 'experimental/tasking-unix)
(require 'experimental/socket)

(define server #f)                      ; The server or #f
(define server-threads '())             ; List of (thread . stop-thunk)

(define (start-server)
  (if (not server)
      (begin (set! server-threads '())
             (set! server (spawn (make-server 12345)))))
  server)

(define (stop-server)
  (call-without-interrupts
    (lambda ()
      (for-each stop-server-thread
                (filter task-alive 
                        (map car server-threads)))
      (set! server-threads '())
      (if server
          (let ((s server))
            (set! server #f)
            (kill s))))))

(define (make-server port)
  (lambda ()
    (let ((s (server-socket port)))
      (let accept-loop ()
        (let-values ((ns addr)
                     (wait-for-connection-on-server-socket s 'nonblocking))
          (let-values ((thunk stop) (make-server-thread ns))
            (let ((t (spawn thunk)))
              (set! server-threads (cons (cons t stop) server-threads)))))
        (accept-loop)))))

(define (make-server-thread socket)
  (let ((in   (open-input-descriptor socket 'nonblocking 'char))
        (out  (open-output-descriptor socket 'nonblocking 'char 'flush))
        (self #f))
    (values (lambda ()
              (set! self (current-task))
              (parameterize ((console-input-port-factory (lambda () in))
                             (console-output-port-factory (lambda () out)))
                  (parameterize ((repl-level 0))
                    (repl)))
              (stop-server-thread self))
            (lambda ()
              (close-input-port in)
              (close-output-port out)
              (kill self)))))

(define (stop-server-thread t)
  (call-without-interrupts
    (lambda ()
      (let ((probe (assq t server-threads)))
        (if probe
            (begin
              (set! server-threads (remq! probe server-threads))
              ((cdr probe)))
            (error "Not a server thread: " t))))))

; A hack.

(define exit
  (let ((exit exit))
    (lambda args
      (if (and *tasking-on*
               (assq (current-task) server-threads))
          (stop-server-thread (current-task))
          (apply exit args)))))

; A patch -- remove when REPL exports REPL-LEVEL.

(define repl-level
  (let ((level 0))
    (lambda args
      (cond ((null? args) level)
            ((null? (cdr args))
             (set! level (car args))
             level)
            (else
             (error "Wrong args to REPL-LEVEL."))))))

; eof
