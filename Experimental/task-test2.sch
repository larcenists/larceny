; Copyright 1999 Lars T Hansen
;
; $Id$
;
; This uses a quasi-cooperative KILL to stop the threads, which is really
; the wrong thing.  A better solution would be like Thread.AltertWait() 
; and Thread.Alert() in Modula-3 -- that way the thread can close its own 
; resources, like the server socket.  Here, the server socket is closed by 
; stop-server.

; You must call begin-tasking before calling start-server.

(require 'experimental/unix-descriptor)
(require 'experimental/iosys)
(require 'experimental/tasking-unix)
(require 'experimental/socket)
(require 'experimental/unix)

(define server #f)                      ; The server or #f
(define server-threads '())             ; List of (thread . stop-thunk)
(define the-server-socket #f)           ; Must close on shutdown

(define (start-server)
  (if (not server)
      (begin (set! server-threads '())
             (set! the-server-socket #f)
             (set! server (spawn (make-server 2345)))))
  (unspecified))

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
            (kill s)))
      (unix/close the-server-socket)
      (unspecified))))

(define (stats)
  (do ((t server-threads (cdr t))
       (i 1 (+ i 1)))
      ((null? t))
    (format #t "Client ~a: ~a~%"
            i
            (if (tasks/runnable? (caar t)) 
                'runnable
                'waiting))))

(define (make-server port)
  (lambda ()
    (let ((s (server-socket port)))
      (set! the-server-socket s)
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
                             (console-output-port-factory (lambda () out))
                             (repl-level 0))
                (repl)
                (stop-server-thread self)))
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

; eof
