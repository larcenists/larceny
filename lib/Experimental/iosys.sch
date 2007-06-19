; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Extensible/generic I/O system for Larceny.  This system is experimental.
; See iosys.txt in this directory for an introduction.
;
; TO DO:
;   The underlying I/O system must support the binary/char distinction better.
;   At the very least,
;    - there must be a field for the content-type somewhere
;    - read-char/write-char should not work on binary ports, and should
;      not take a performance hit to be more selective than now
;    - the string/bytevector distinction must be implemented.
;    - make primitive byte port operations part of the I/O system
;
;  The underlying I/O system must be made thread safe.
;
; The procedures in this file are thread-safe if the underlying I/O system
; is thread-safe.  Currently it isn't but that's only a problem if several
; threads operate on the same port.

; Interface.  Commented-out procedures are already in Larceny.

; General.

(define make-input-port)          ; input-handler * iodata * type -> port
(define make-output-port)         ; output-handler * iodata * type * bool 
                                  ;   -> port

;(define port?)                   ; object -> bool
;(define input-port?)             ; object -> bool
;(define output-port?)            ; object -> bool

;(define port-name)               ; port -> string
;(define port-position)           ; port -> exact-int
(define port-handler)             ; port -> procedure
(define port-datum)               ; port -> object
(define port-content-type)        ; port -> symbol
(define port-buffer)              ; port -> { string | bytevector }
(define port-error-flag)          ; port -> bool
(define port-error-flag-set!)     ; port * bool -> unspecified

; Input ports.

(define port-eof-flag)            ; port -> bool
(define port-eof-flag-set!)       ; port * bool -> unspecified
(define port-read-pointer)        ; port -> fixnum
(define port-read-pointer-set!)   ; port * fixnum -> unspecified
(define port-read-limit)          ; port -> fixnum
(define port-read-limit-set!)     ; port * fixnum -> unspecified

; Output ports.

(define port-flush-flag)          ; port -> bool
(define port-flush-flag-set!)     ; port * bool -> unspecified
(define port-write-pointer)       ; port -> fixnum
(define port-write-pointer-set!)  ; port * fixnum -> unspecified

; Character I/O

;(define read-char)               ; [port] -> { char | eof-object }
;(define peek-char)               ; [port] -> { char | eof-object }
;(define char-ready?)             ; [port] -> bool
;(define write-char)              ; char [* port] -> unspecified
(define read-characters)          ; port * string * fixnum * fixnum
                                  ;    -> { fixnum | eof-object }
(define write-characters)         ; port * string * fixnum * fixnum
                                  ;    -> unspecified

; Byte I/O

(define read-byte)                ; [port] -> { fixnum | eof-object }
(define peek-byte)                ; [port] -> { fixnum | eof-object }
(define byte-ready?)              ; [port] -> bool
(define write-byte)               ; byte [* port] -> unspecified
(define read-bytes)               ; port * bytevector * fixnum * fixnum
                                  ;    -> { fixnum | eof-object }
(define write-bytes)              ; port * bytevector * fixnum * fixnum
                                  ;    -> unspecified

(let ()

  (define sys$tag.port-typetag 4)

  ; Copied from Lib/Common/iosys.sch

  (define port.input?     0) ; boolean: an open input port
  (define port.output?    1) ; boolean: an open output port
  (define port.iodata     2) ; port-specific data
  (define port.ioproc     3) ; port*symbol -> void
  (define port.buffer     4) ; a string or #f: i/o buffer
  (define port.error?     5) ; boolean: #t after error

  ; input ports

  (define port.rd-eof?    6) ; boolean: input port at EOF
  (define port.rd-lim     7) ; nonnegative fixnum: index beyond last char
  (define port.rd-ptr     8) ; nonnegative fixnum: next loc for input

  ; output ports

  (define port.wr-flush?  9) ; boolean: discretionary output flushing
  (define port.wr-ptr    10) ; nonnegative fixnum: next loc for output

  ; common data

  (define port.position  11) ; nonnegative fixnum: number of characters read
                             ; or written, not counting what's in the current
                             ; buffer.

  ;; FIXME: Added by Felix to match changeset:4554

  (define port.type      12) ; fixnum: the inclusive or of
                             ; binary/textual:
                             ;     0 means binary
                             ;     1 means textual
                             ; direction:
                             ;     2 means input
                             ;     4 means output
                             ;     6 means input/output

  (define port.charbuf   13) ; bytevector: verified UTF-8 followed by sentinel
  (define port.charptr   14) ; nonnegative fixnum: next loc in charbuf
  (define port.charlim   15) ; nonnegative fixnum: sentinel in charbuf
  (define port.charpos   16) ; fixnum: character position - charptr
  
  (define port.auxbuf    17) ; bytevector: 4 bytes that straddle buffer boundary
  (define port.auxlim    18) ; number of ready bytes in auxbuf
  (define port.auxneeded 19) ; number of bytes needed to complete the char
  
  (define port.transcoder 20)          ; fixnum: see comment at make-transcoder
  
  (define port.structure-size 21)      ; size of port structure
  
  (define port.buffer-size    1024)    ; length of default I/O buffer
  (define port.charbuf-size   3100)
  
  ; Textual input uses a sentinel byte;
  ; It can be any value in 128..255 that is not a legal code unit of UTF-8.

  (define port.sentinel 255)

  (define (port-getter n name)
    (lambda (p)
      (if (port? p)
          (vector-like-ref p n)
          (error name ": not a port: " p))))

  (define (port-setter n name)
    (lambda (p x)
      (if (port? p)
          (vector-like-set! p n x)
          (error name ": not a port: " p))))

  (define (input-port-getter n name)
    (lambda (p)
      (if (input-port? p)
          (vector-like-ref p n)
          (error name ": not an input port: " p))))

  (define (input-port-setter n name)
    (lambda (p x)
      (if (input-port? p)
          (vector-like-set! p n x)
          (error name ": not an input port: " p))))

  (define (output-port-getter n name)
    (lambda (p)
      (if (output-port? p)
          (vector-like-ref p n)
          (error name ": not an output port: " p))))

  (define (output-port-setter n name)
    (lambda (p x)
      (if (output-port? p)
          (vector-like-set! p n x)
          (error name ": not an output port: " p))))

  ; For now we define these as wrappers around the char procedures, but
  ; that will be fixed at some point.  At that point, these will be
  ; defined only on BYTE ports, and the *-char procedures will be
  ; defined only on CHAR ports.  As it is, both classes of procedures 
  ; can be used on both types of ports.

  (define (internal-read-byte . rest)
    (let ((c (if (null? rest)
                 (read-char)
                 (read-char (car rest)))))
      (if (eof-object? c)
          c
          (char->integer c))))

  (define (internal-write-byte b . rest)
    (if (null? rest)
        (write-char (integer->char b))
        (write-char (integer->char b) (car rest))))
  
  (define (internal-peek-byte . rest)
    (let ((c (if (null? rest)
                 (peek-char)
                 (peek-char (car rest)))))
      (if (eof-object? c)
          c
          (char->integer c))))

  (define (internal-byte-ready? . rest)
    (if (null? rest)
        (char-ready?)
        (char-ready? (car rest))))

  ; Carefully written to work on both strings and bytevectors; see comments
  ; for read-byte et al, above.

  (define (input-bytes port buffer start count)
    (call-without-interrupts
      (lambda ()
        (let loop ((have-read 0) (start start) (count count))
          (let* ((ptr (port-read-pointer port))
                 (lim (port-read-limit port))
                 (input-buffer (port-buffer port))
                 (have (- lim ptr)))
            (cond ((zero? have)
                   (let ((c (peek-char port))) ; Force refill
                     (if (eof-object? c)
                         (if (> have-read 0)
                             have-read
                             c)
                         (loop have-read start count))))
                  (else
                   (let* ((n (min count have))
                          (end (+ start n)))
                     (do ((j start (+ j 1))
                          (p ptr (+ p 1)))
                         ((eq? j end)
                          (port-read-pointer-set! port p))
                       (bytevector-like-set! buffer 
                                             j
                                             (bytevector-like-ref input-buffer
                                                                  p)))
                     (if (< n count)
                         (loop (+ have-read n) (+ start n) (- count n))
                         (+ have-read n))))))))))

  (define (input-chars port buffer start count)
    (input-bytes port buffer start count))

  (define (output-bytes port buffer start count)
    (call-without-interrupts
      (lambda ()
        (let loop ((start start) (count count))
          (let* ((ptr (port-write-pointer port))
                 (output-buffer (port-buffer port))
                 (lim (bytevector-like-length output-buffer))
                 (room (- lim ptr)))
            (cond ((zero? room)
                   (flush-output-port port)
                   (loop start count))
                  (else
                   (let* ((n (min count room))
                          (end (+ start n)))
                     (do ((j start (+ j 1))
                          (p ptr (+ p 1)))
                         ((eq? j end)
                          (port-write-pointer-set! port p))
                       (bytevector-like-set! output-buffer
                                             p
                                             (bytevector-like-ref buffer j)))
                     (cond ((< n count)
                            (loop (+ start n) (- count n)))
                           ((port-flush-flag port)
                            (flush-output-port port))))))))))
    (unspecified))

  (define (output-chars port buffer start count)
    (output-bytes port buffer start count))

  ; Copied from Lib/Common/iosys.sch (edited slightly)

  (define (io/make-port ioproc iodata . rest)
    (let ((v (make-vector port.structure-size #f))
        (input? #f)
        (output? #f)
        (binary? #f)
        (textual? #f))
      (do ((l rest (cdr l)))
          ((null? l))
        (case (car l)
          ((input)   (set! input? #t) (vector-set! v port.input? #t))
          ((output)  (set! output? #t) (vector-set! v port.output? #t))
          ((text)    (set! textual? #t))
          ((binary)  (set! binary? #t))
          ((flush)   (vector-set! v port.wr-flush? #t))
          (else      (error "make-port: bad attribute: " (car l))
                     #t)))
      (vector-set! v port.ioproc ioproc)
      (vector-set! v port.iodata iodata)
      (vector-set! v port.buffer (make-bytevector port.buffer-size))
      (vector-set! v port.rd-lim 0)
      (vector-set! v port.rd-ptr 0)
      (vector-set! v port.wr-ptr 0)
      (vector-set! v port.position 0)
      ; FIXME: added by Felix to match changeset:4554
      ; Note: io/make-port defaults to textual (for backward compatibility)
      (vector-set! v port.type
                     (fxlogior (fxlogior (if input? 2 0) (if output? 4 0))
                               (if binary? 0 1)))
      (let ((charbuf (make-bytevector port.charbuf-size)))
        (vector-set! v port.charbuf charbuf)
        (bytevector-set! charbuf 0 port.sentinel))
      (vector-set! v port.charptr 0)
      (vector-set! v port.charlim 0)
      (vector-set! v port.charpos 0)
      (vector-set! v port.auxbuf (make-bytevector 4))
      (vector-set! v port.auxlim 0)
      (vector-set! v port.auxneeded 0)
      (vector-set! v port.transcoder
                   (if binary?
                       0
                       (native-transcoder)))
      (typetag-set! v sys$tag.port-typetag)
      v))

  (set! make-input-port 
        (lambda (handler datum type)
          (io/make-port handler datum 'input)))

  (set! make-output-port 
        (lambda (handler datum type flush?)
          (if flush?
              (io/make-port handler datum 'output 'flush)
              (io/make-port handler datum 'output))))

  (set! port-handler (port-getter port.ioproc 'port-handler))
  (set! port-datum (port-getter port.iodata 'port-datum))
  (set! port-buffer (port-getter port.buffer 'port-buffer))
; Can't support this yet
;  (set! port-content-type (port-getter ??? 'port-content-type))
  (set! port-error-flag (port-getter port.error? 'port-error-flag))
  (set! port-error-flag-set! (port-setter port.error? 'port-error-flag-set!))

  (set! port-eof-flag
        (input-port-getter port.rd-eof? 'port-eof-flag))
  (set! port-eof-flag-set! 
        (input-port-setter port.rd-eof? 'port-eof-flag-set!))
  (set! port-read-pointer 
        (input-port-getter port.rd-ptr 'port-read-pointer))
  (set! port-read-pointer-set! 
        (input-port-setter port.rd-ptr 'port-read-pointer-set!))
  (set! port-read-limit 
        (input-port-getter port.rd-lim 'port-read-limit))
  (set! port-read-limit-set! 
        (input-port-setter port.rd-lim 'port-read-limit-set!))

  (set! port-flush-flag 
        (output-port-getter port.wr-flush? 'port-flush-flag))
  (set! port-flush-flag-set! 
        (output-port-setter port.wr-flush? 'port-flush-flag-set!))
  (set! port-write-pointer 
        (output-port-getter port.wr-ptr 'port-write-pointer))
  (set! port-write-pointer-set! 
        (output-port-setter port.wr-ptr 'port-write-pointer-set!))

  (set! read-characters input-chars)
  (set! write-characters output-chars)

  (set! read-byte internal-read-byte)
  (set! peek-byte internal-peek-byte)
  (set! byte-ready? internal-byte-ready?)
  (set! write-byte internal-write-byte)

  (set! read-bytes input-bytes)
  (set! write-bytes output-bytes)
  'iosys)

; eof
