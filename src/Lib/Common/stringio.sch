; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; MacScheme-compatible string I/O ports.

; Offsets in the string port data structure.

(define string-io.port 1)  ; Output: bytevector output port

; FIXME: using the alist for this, but not for set-position!, is screwy.

(define (string-io/install-port-position-as-binary! p data)
  (let ((get-position
         (lambda () (port-position (vector-ref data string-io.port)))))
    (io/port-alist-set! p
                        (cons (cons 'port-position-in-bytes get-position)
                              (io/port-alist p)))))

(define (string-io/open-input-string s)
  (if (not (string? s))
      (error "open-input-string: " s " is not a string."))
  (io/transcoded-port
   (bytevector-io/open-input-bytevector-no-copy (string->utf8 s))
   (io/make-transcoder 'utf-8 'none 'replace)))

; Accepts a UTF-8 transcoder as its optional argument,
; which is used to implement string->bytevector.

(define (string-io/open-output-string . rest)
  (assert (or (null? rest)
              (eq? (transcoder-codec (car rest)) 'utf-8)))
  (let* ((p (bytevector-io/open-output-bytevector))
         (data (vector 'string-output-port p))
         (q (io/make-port string-io/ioproc
                          data 'output 'binary 'set-position!))
         (t (if (null? rest)
                (io/make-transcoder 'utf-8 'none 'replace)
                (car rest))))
    (string-io/install-port-position-as-binary! q data)
    (io/transcoded-port q t)))

(define (string-io/get-output-string port)
  (if (not (string-output-port? port))
      (assertion-violation 'get-output-string "illegal argument" port))
  (flush-output-port port)
  (let* ((data (vector-like-ref port port.iodata))
	 (p    (vector-ref data string-io.port)))
    (flush-output-port p)
    (utf8->string (get-output-bytevector p))))

(define (string-io/reset-output-string port)
  (if (not (string-output-port? port))
      (error "reset-output-string: " port " is not a string output port."))
  (flush-output-port port)
  (let* ((data (vector-like-ref port port.iodata))
	 (p    (vector-ref data string-io.port)))
    (flush-output-port p)
    (bytevector-io/reset-output-bytevector p)))

(define (string-io/ioproc op)
  (case op
    ((read)   (lambda (data buffer)
                (assertion-error 'string-io/ioproc "internal error")))
    ((write)  string-io/flush-buffer)
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*string*"))
    ((set-position!)
              (lambda (data offset)
                (set-port-position! (vector-ref data string-io.port) offset)
                'ok))
    (else     (error "string-io/ioproc: illegal operation: " op))))

(define (string-io/flush-buffer data buffer count)
  (let ((p    (vector-ref data string-io.port)))
    (put-bytevector p buffer 0 count)
    'ok))

(define (string-output-port? port)
  (and (output-port? port)
       (let ((d (vector-like-ref port port.iodata)))
         (and (vector? d)
              (> (vector-length d) 0)
              (eq? (vector-ref d 0) 'string-output-port)))))

; eof
