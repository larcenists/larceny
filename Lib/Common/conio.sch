; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; "console" I/O ports.
;
; The code in this file relies on the code in fileio.sch -- these console 
; ports are only slight variations on file ports.

($$trace "conio")

(define (console-io/initialize)
  (osdep/initialize-console))

(define (console-io/ioproc op)
  (case op
    ((read) 
     (file-io/ioproc op))               ; wrong if console is intermittent
    ((write) 
     (file-io/ioproc op))               ; ditto
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

(define (console-io/open-input-console)
  (let ((fd (osdep/open-console 'input)))
    (io/make-port console-io/ioproc
                  (file-io/data fd "*console-input*")
                  'input)))

(define (console-io/open-output-console)
  (let ((fd (osdep/open-console 'output)))
    (io/make-port console-io/ioproc
                  (file-io/data fd "*console-output*")
                  'output
                  'flush)))

; eof
