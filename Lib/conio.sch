; Lib/conio.sch
; Larceny -- "console" I/O ports for Unix-like streams.
;
; $Id: conio.sch,v 1.2 1997/03/05 19:28:51 lth Exp $
;
; The code in this file relies on the code in fileio.sch -- these console 
; ports are only slight variations on file ports.

(define (console-io/initialize)
  ; nothing, for now
  #t)

(define (console-io/ioproc op)
  (case op
    ((read) 
     (file-io/ioproc op))
    ((write) 
     (file-io/ioproc op))
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
  (let ((fd (sys$open-terminal 'input)))
    (io/make-port console-io/ioproc
		  (file-io/data fd "*console-input*")
		  'input)))

(define (console-io/open-output-console)
  (let ((fd (sys$open-terminal 'output)))
    (io/make-port console-io/ioproc
		  (file-io/data fd "*console-output*")
		  'output
		  'flush)))

; eof
