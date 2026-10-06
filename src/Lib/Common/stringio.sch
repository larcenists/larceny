; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; MacScheme-compatible string I/O ports.

; A string port's buffer is a string: the string an input port reads,
; or the characters an output port has written, so that no character
; is encoded or decoded.  The ports are made here, and their operations
; are in iosys.sch (see "String ports" there).  Positions are indexes.
;
; (String ports used to be UTF-8 transcoded ports over bytevector
; ports, and then textual ports whose UTF-8 byte buffer was decoded
; into a string, or filled by encoding one.)

(define string-io/utf-8
  (io/make-transcoder 'utf-8 'none 'replace))

(define (string-io/open-input-string s)
  (if (not (string? s))
      (error "open-input-string: " s " is not a string."))
  (let ((p (io/make-port string-io/ioproc
                         (vector 'string-input-port)
                         'input 'text 'string 'set-position!)))
    (vector-like-set! p port.transcoder string-io/utf-8)
    (vector-like-set! p port.mainbuf s)
    (vector-like-set! p port.mainlim (string-length s))
    p))

; Accepts a UTF-8 transcoder as its optional argument,
; which is used to implement string->bytevector.

(define (string-io/open-output-string . rest)
  (assert (or (null? rest)
              (eq? (transcoder-codec (car rest)) 'utf-8)))
  (let ((p (io/make-port string-io/ioproc
                         (vector 'string-output-port)
                         'output 'text 'string 'set-position!)))
    (vector-like-set! p port.transcoder
                      (if (null? rest) string-io/utf-8 (car rest)))
    (vector-like-set! p port.mainbuf (make-string 32))
    p))

; Closing a string port discards its string.

(define (string-io/get-output-string port)
  (if (not (and (string-output-port? port) (io/string-port? port)))
      (assertion-violation 'get-output-string "illegal argument" port))
  (io/string-output port))

(define (string-io/reset-output-string port)
  (if (not (and (string-output-port? port) (io/string-port? port)))
      (error "reset-output-string: " port " is not a string output port."))
  (io/string-output-reset! port)
  (unspecified))

(define (string-io/ioproc op)
  (case op
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*string*"))
    (else     (error "string-io/ioproc: illegal operation: " op))))

(define (string-output-port? port)
  (and (output-port? port)
       (let ((d (vector-like-ref port port.iodata)))
         (and (vector? d)
              (> (vector-length d) 0)
              (eq? (vector-ref d 0) 'string-output-port)))))

; eof
