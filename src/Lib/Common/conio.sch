; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; "Console" I/O ports.
;
; We want the console ports to behave like normal ports (EOF is 
; delivered indefinitely; closing a port prohibits further use) 
; but still have the option of using the console.  In particular,
; the REPL and debugger and other interactive applications should 
; be able to rely on the console _not_ delivering EOF repeatedly
; even when EOF is seen on the port.  This is because EOF is a 
; much-used synonym for "done here" under Unix, and it would be 
; criminal not to support it (though it is really a malfeasance).
;
; To effect this there are three procedures CONSOLE-INPUT-PORT,
; CONSOLE-OUTPUT-PORT, and CONSOLE-ERROR-PORT that normally
; return the console input, output, and error ports but will
; create new console port(s) if the old ports are closed, if
; EOF is seen, or if an error is detected, after first
; returning the EOF or signalling the error once.  Interactive
; applications can use these procedures to get "sane" consoles in
; the presence of the Unix behavior.

; The code in this file relies on the code in fileio.sch -- these 
; console ports are only slight variations on file ports.  That's
; appropriate for Unix but perhaps not for other systems.
;
; FIXME: the three FIXMEs below need to be implemented but it's a 
;        little involved because we need to record that a descriptor
;        has been closed so it won't be closed again, without 
;        making it look like the port is closed (yeah, weird).  
;        But the I/O system doesn't know how to deal with that 
;        scenario yet.  If we don't do it, we may run out of 
;        descriptors in a long session.

($$trace "conio")

(define *current-console-input*  #f)    ; There is only one!
(define *current-console-output* #f)    ; There is only one!
(define *current-console-error* #f)     ; There is only one!

(define (console-io/console-input-port)
  (call-without-interrupts
    (lambda ()
      (let ((ccin *current-console-input*))
        (if (or (not (io/open-port? ccin))
                (io/port-error-condition? ccin)
                (io/port-at-eof? ccin))
            (begin (if (io/open-port? ccin)
                       #t)              ; FIXME: reap the descriptor.
                   (set! *current-console-input*
                         (console-io/open-input-console))))
        *current-console-input*))))

(define (console-io/console-output-port)
  (call-without-interrupts
    (lambda ()
      (let ((ccout *current-console-output*))
        (if (or (not (io/open-port? ccout))
                (io/port-error-condition? ccout))
            (begin (if (io/open-port? ccout)
                       #t)              ; FIXME: reap the descriptor
                   (set! *current-console-output* 
                         (console-io/open-output-console))))
        *current-console-output*))))

(define (console-io/console-error-port)
  (call-without-interrupts
    (lambda ()
      (let ((ccout *current-console-error*))
        (if (or (not (io/open-port? ccout))
                (io/port-error-condition? ccout))
            (begin (if (io/open-port? ccout)
                       #t)              ; FIXME: reap the descriptor
                   (set! *current-console-error* 
                         (console-io/open-error-console))))
        *current-console-error*))))

(define (console-io/initialize)
  (osdep/initialize-console)
  (set! *current-console-input* (console-io/open-input-console))
  (set! *current-console-output* (console-io/open-output-console))
  (set! *current-console-error* (console-io/open-error-console)))

(define (console-io/ioproc op)
  (case op
    ((read)
     (console-io/with-retry (file-io/ioproc op)))
    ((write) 
     (file-io/ioproc op))               ; wrong if console is intermittent
    ((close) 
     (lambda (data)
       (let ((r (osdep/close-console (file-io/fd data))))
         (if (< r 0)
             'error
             'ok))))
    ((ready?)
     (lambda (data)
       (osdep/char-ready-console? (file-io/fd data))))
    ((name)
     (lambda (data)
       (file-io/name data)))
    (else 
     (error "console-io/ioproc: illegal operation: " op)
     #t)))

; Retries once after an error.
; FIXME: This was a workaround for ^Z read errors on MacOS X,
; but it caused more problems than it solved.

;(define (console-io/with-retry proc)
;  (lambda args
;    (let ((r (apply proc args)))
;      (if (or (not (fixnum? r)) (< r 0))
;          (apply proc args)
;          r))))

(define (console-io/with-retry proc)
  (lambda args
    (apply proc args)))

;(define (console-io/open-input-console)
;  (let ((fd (osdep/open-console 'input)))
;    (io/make-port console-io/ioproc
;                  (file-io/data fd "*console-input*")
;                  'input)))

;(define (console-io/open-output-console)
;  (let ((fd (osdep/open-console 'output)))
;    (io/make-port console-io/ioproc
;                  (file-io/data fd "*console-output*")
;                  'output
;                  'flush)))

(define (console-transcoder)
  (default-transcoder))

(define (console-io/open-input-console)
  (let* ((fd (osdep/open-console 'input))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-input*")
                          'binary
                          'input)))
    (transcoded-port p (console-transcoder))))

(define (console-io/open-output-console)
  (let* ((fd (osdep/open-console 'output))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-output*")
                          'binary
                          'output
                          'flush)))
    (transcoded-port p (console-transcoder))))

(define (console-io/open-error-console)
  (let* ((fd (osdep/open-console 'error))
         (p (io/make-port console-io/ioproc
                          (file-io/data fd "*console-error*")
                          'binary
                          'output
                          'flush)))
    (transcoded-port p (console-transcoder))))

; eof
