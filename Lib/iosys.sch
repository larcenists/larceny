; Lib/iosys.sch
; Larceny -- New I/O system
;
; *$Id: iosys.sch,v 1.3 1997/05/15 00:42:10 lth Exp lth $
;
; Design: the system is designed so that in the common case, very few
; procedure calls are executed.
;
; FIXME:
;  'Port?' needs to be integrable for maximal efficiency.

(define port.input?     0) ; boolean: an open input port
(define port.output?    1) ; boolean: an open output port
(define port.iodata     2) ; port-specific data
(define port.ioproc     3) ; port*symbol -> void
(define port.buffer     4) ; a string or #f: i/o buffer

; input ports

(define port.rd-eof?    5) ; boolean: input port at EOF
(define port.rd-lim     6) ; nonnegative fixnum: index beyond last char
(define port.rd-ptr     7) ; nonnegative fixnum: next loc for input

; output ports

(define port.wr-flush?  8) ; boolean: discretionary output flushing
(define port.wr-ptr     9) ; nonnegative fixnum: next loc for output

(define port.structure-size 10)      ; size of port structure
(define port.buffer-size    1024)    ; length of default I/O buffer


;;; Private procedures

(define (io/fill-buffer p)
  (let ((r (((vector-like-ref p port.ioproc) 'read)
	    (vector-like-ref p port.iodata)
	    (vector-like-ref p port.buffer))))
    (cond ((eq? r 'eof)
	   (vector-like-set! p port.rd-ptr 0)
	   (vector-like-set! p port.rd-lim 0)
	   (vector-like-set! p port.rd-eof? #t))
	  ((eq? r 'error)
	   (error "Read error on port " p)
	   #t)
	  (else
	   (vector-like-set! p port.rd-ptr 0)
	   (vector-like-set! p port.rd-lim r)))))


(define (io/flush-buffer p)
  (if (> (vector-like-ref p port.wr-ptr) 0)
      (let ((r (((vector-like-ref p port.ioproc) 'write)
		(vector-like-ref p port.iodata)
		(vector-like-ref p port.buffer)
		(vector-like-ref p port.wr-ptr))))
	(cond ((eq? r 'ok)
	       (vector-like-set! p port.wr-ptr 0))
	      ((eq? r 'error)
	       (error "Write error on port " p)
	       #t)
	      (else
	       ???)))))


;;; Public low-level interface

(define (io/initialize)
  ; Nothing, for the time being.
  #t)

; 'ioproc' is a procedure of one argument: a symbol that denotes the 
; operation to perform.  It returns a port-specific procedure that, when
; called, performs the operation.  The operations are:
;
;   read : iodata * buffer -> { fixnum, 'eof, 'error }
;   write : iodata * buffer * count -> { 'ok, 'error }
;   close : iodata -> { 'ok, 'error }
;   ready? : iodata -> boolean
;   name : iodata -> string

(define (io/make-port ioproc iodata . rest)
  (let ((v (make-vector port.structure-size #f)))
    (do ((l rest (cdr l)))
	((null? l))
      (case (car l)
	((input)   (vector-set! v port.input? #t))
	((output)  (vector-set! v port.output? #t))
	((flush)   (vector-set! v port.wr-flush? #t))
	(else      (error "make-proc: bad attribute: " (car l))
		   #t)))
    (vector-set! v port.ioproc ioproc)
    (vector-set! v port.iodata iodata)
    (vector-set! v port.buffer (make-string port.buffer-size))
    (vector-set! v port.rd-lim 0)
    (vector-set! v port.rd-ptr 0)
    (vector-set! v port.wr-ptr 0)
    (typetag-set! v sys$tag.port-typetag)
    v))

(define (port? x)
  (and (vector-like? x)
       (= (typetag x) sys$tag.port-typetag)))

(define (io/input-port? p)
  (and (port? p) (vector-like-ref p port.input?)))

(define (io/output-port? p)
  (and (port? p) (vector-like-ref p port.output?)))

; Note: I've inlined port? to avoid a procedure call.

(define (io/read-char p)
  (if (and (vector-like? p)
	   (= (typetag p) sys$tag.port-typetag)
	   (vector-like-ref p port.input?))
      (let ((ptr (vector-like-ref p port.rd-ptr))
	    (lim (vector-like-ref p port.rd-lim))
	    (buf (vector-like-ref p port.buffer)))
	(cond ((< ptr lim)
	       (let ((c (string-ref buf ptr)))
		 (vector-like-set! p port.rd-ptr (+ ptr 1))
		 c))
	      ((vector-like-ref p port.rd-eof?)
	       (eof-object))
	      (else
	       (io/fill-buffer p)
	       (io/read-char p))))
      (begin (error "read-char: not an input port: " p)
	     #t)))

; Note: I've inlined port? to avoid a procedure call.

(define (io/peek-char p)
  (if (and (vector-like? p)
	   (= (typetag p) sys$tag.port-typetag)
	   (vector-like-ref p port.input?))
      (let ((ptr (vector-like-ref p port.rd-ptr))
	    (lim (vector-like-ref p port.rd-lim))
	    (buf (vector-like-ref p port.buffer)))
	(cond ((< ptr lim)
	       (string-ref buf ptr))
	      ((vector-like-ref p port.rd-eof?)
	       (eof-object))
	      (else
	       (io/fill-buffer p)
	       (io/peek-char p))))
      (begin (error "peek-char: not an input port: " p)
	     #t)))

; This is a hack that speeds up the current reader.
; peek-next-char discards the current character and peeks the next one.

(define (io/peek-next-char p)
  (if (and (vector-like? p)
	   (= (typetag p) sys$tag.port-typetag)
	   (vector-like-ref p port.input?))
      (let ((ptr (vector-like-ref p port.rd-ptr))
	    (lim (vector-like-ref p port.rd-lim))
	    (buf (vector-like-ref p port.buffer)))
	(cond ((< ptr lim)
	       (let ((ptr (+ ptr 1)))
		 (vector-like-set! p port.rd-ptr ptr)
		 (if (< ptr lim)
		     (string-ref buf ptr)
		     (io/peek-char p))))
	      ((vector-like-ref p port.rd-eof?)
	       (eof-object))
	      (else
	       (io/fill-buffer p)
	       (io/peek-char p))))
      (begin (error "peek-next-char: not an input port: " p)
	     #t)))

(define (io/char-ready? p)
  (if (and (port? p) (vector-like-ref p port.input?))
      (cond ((< (vector-like-ref p port.rd-ptr)
		(vector-like-ref p port.rd-lim))
	     #t)
	    ((vector-like-ref p port.rd-eof?)
	     #t)
	    (else
	     (((vector-like-ref p port.ioproc) 'ready?)
	      (vector-like-ref p port.iodata))))
      (begin (error "io/char-ready?: not an input port: " p)
	     #t)))

(define (io/write-char c p)
  (if (and (port? p) (vector-like-ref p port.output?))
      (let ((buf (vector-like-ref p port.buffer))
	    (ptr (vector-like-ref p port.wr-ptr)))
	(cond ((< ptr (string-length buf))
	       (string-set! buf ptr c)
	       (vector-like-set! p port.wr-ptr (+ ptr 1))
	       (unspecified))
	      (else
	       (io/flush-buffer p)
	       (io/write-char c p))))
      (begin (error "write-char: not an output port: " p)
	     #t)))

(define (io/discretionary-flush p)
  (if (and (port? p) (vector-like-ref p port.output?))
      (if (vector-like-ref p port.wr-flush?)
	  (io/flush-buffer p))
      (begin (error "io/discretionary-flush: not an output port: " p)
	     #t)))

(define (io/flush p)
  (if (and (port? p) (vector-like-ref p port.output?))
      (io/flush-buffer p)
      (begin (error "io/flush: not an output port: " p)
	     #t)))

(define (io/close-port p)
  (if (port? p)
      (begin
	(if (vector-like-ref p port.output?)
	    (io/flush-buffer p))
	(((vector-like-ref p port.ioproc) 'close)
	 (vector-like-ref p port.iodata))
	(vector-like-set! p port.input? #f)
	(vector-like-set! p port.output? #f)
	(unspecified))
      (begin (error "io/close-port: not a port: " p)
	     #t)))

(define (io/port-name p)
  (((vector-like-ref p port.ioproc) 'name) (vector-like-ref p port.iodata)))

; eof
