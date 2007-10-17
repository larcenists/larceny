; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; MacScheme-compatible string I/O ports.
;
; FIXME: inefficient representation for output strings

; Offsets in the string port data structure.
;
; FIXME:  The string port data structure is no longer used
; for string input ports.
;
; For an input string port, the string-io.i field holds
; the index of the next character to be read from the
; string in the string-io.s field.
;
; For an output string port, the string-io.i field of
; the port data structure holds the number of bytes
; that remain to be fetched to complete the character
; that is currently under construction.  The bits
; that have already been fetched are in the string-io.bits
; field (the "uc" stands for under construction).

(define string-io.type 0)  ; Symbol
(define string-io.s 1)     ; Input: string     ; FIXME: obsolete
(define string-io.i 2)     ; Input: fixnum     ; FIXME: obsolete

(define string-io.port 1)  ; Output: bytevector output port

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
  (let ((p (bytevector-io/open-output-bytevector)))
    (io/transcoded-port
     (io/make-port string-io/ioproc
                   (vector 'string-output-port p)
                   'output
                   'binary)
     (if (null? rest)
         (io/make-transcoder 'utf-8 'none 'replace)
         (car rest)))))

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
    ((read)   ;string-io/fill-buffer  ; FIXME: obsolete
              (lambda (data buffer)
                (assertion-error 'string-io/ioproc "internal error")))
    ((write)  string-io/flush-buffer)
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*string*"))
    (else     (error "string-io/ioproc: illegal operation: " op))))

; FIXME:  This method is no longer used because it works only for Latin-1.

(define (string-io/fill-buffer data buffer)
  (let ((s (vector-ref data string-io.s))
	(i (vector-ref data string-io.i)))
    (let ((n (min (bytevector-like-length buffer) (- (string-length s) i))))
      (if (= n 0)
	  'eof
	  (do ((j 0 (+ j 1))
	       (k i (+ k 1)))
	      ((= j n) 
	       (vector-set! data string-io.i (+ i n))
	       n)
	    (bytevector-like-set! buffer
                                  j
                                  (char->integer (string-ref s k))))))))

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
