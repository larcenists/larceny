; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; "console" I/O ports for Unix-like streams.
;
; The code in this file relies on the code in fileio.sch -- these console 
; ports are only slight variations on file ports.

($$trace "conio")

(define *conio-input-firsttime* #t)
(define *conio-output-firsttime* #t)

(define (console-io/initialize)
  (set! *conio-input-firsttime* #t)
  (set! *conio-output-firsttime* #t)
  #t)

(define (console-io/ioproc op)
  (case op
    ((read) 
     (file-io/ioproc op))		; wrong if console is intermittent
    ((write) 
     (file-io/ioproc op))		; ditto
    ((close) 
     (lambda (data)
       (let ((r (sys$close-terminal (file-io/fd data))))
	 (if (< r 0)
	     'error
	     'ok))))
    ((ready?)
     (lambda (data)
       (sys$char-ready? (file-io/fd data))))
    ((name)
     (lambda (data)
       (file-io/name data)))
    (else 
     (error "console-io/ioproc: illegal operation: " op)
     #t)))

(define (console-io/open-input-console)
  (let ((fd (sys$open-terminal 'input *conio-input-firsttime*)))
    (set! *conio-input-firsttime* #f)
    (io/make-port console-io/ioproc
		  (file-io/data fd "*console-input*")
		  'input)))

(define (console-io/open-output-console)
  (let ((fd (sys$open-terminal 'output *conio-output-firsttime*)))
    (set! *conio-output-firsttime* #f)
    (io/make-port console-io/ioproc
		  (file-io/data fd "*console-output*")
		  'output
		  'flush)))

; eof
