; A very crude hack to open ports on descriptors.
; (I can't believe I'm doing this.)
;
; USE AT YOUR OWN RISK.  
; Works with 1.0a1 and 0.42, at least.  Will be replaced by general ports.
;
; 19 April 1999 / lth
;
; For example:
;  (define stderr (open-output-descriptor 2))
;  (port/set-discretionary-flush-flag! stderr #t)

(define (open-input-descriptor fd)
  (let ((p (open-input-file "/dev/null")))
    (let ((port-datum (vector-like-ref p 2)))
      (set-car! port-datum fd)
      (set-cdr! port-datum (string-append "*descriptor "
                                          (number->string fd)
                                          "*"))
      p)))

(define (open-output-descriptor fd)
  (let ((p (open-output-file "/dev/null")))
    (let ((port-datum (vector-like-ref p 2)))
      (set-car! port-datum fd)
      (set-cdr! port-datum (string-append "*descriptor "
                                          (number->string fd)
                                          "*"))
      p)))

; For output ports.
; _flag_ is a boolean: 
;  #t means flush following every output operation except write-char.
;  #f means don't.

(define (port/set-discretionary-flush-flag! output-port flag)
  (vector-like-set! output-port 9 flag))

; Port is an input or output port
; Wrapping-proc is a procedure that takes an ioproc and returns an ioproc.
; An ioproc is a procedure of two arguments: port and operation.  The
; operation is a symbol in the set { read, write, close, ready?, name }
; where input ports respond to read and ready?, output ports to write, 
; and both respond to close and name.  In every case, the response is a 
; procedure that performs the operation.  See Lib/Common/iosys.sch for the
; infrastructure and Lib/Common/{fileio.sch,conio.sch,stringio.sch} for
; some examples.

(define (port/wrap-ioproc! port wrapping-proc)
  (port/set-ioproc! port (wrapping-proc (port/get-ioproc port))))

(define (port/get-ioproc port)
  (vector-like-ref port 3))

(define (port/set-ioproc! port proc)
  (vector-like-set! port 3 proc))

; eof
