; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- I/O system.
;
; Design: the system is designed so that in the common case, very few
; procedure calls are executed.

($$trace "iosys")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Port data structure and invariants.
;
; Latin-1 and UTF-8 transcoding is done on the fly.  UTF-16 and
; nonstandard transcoders are implemented only by custom ports,
; which translate to UTF-8, so UTF-16 and nonstandard transcoders
; are never seen by this level of abstraction.
;
; Every port has:
;
;     type            { binary, textual }
;                   x { closed, input, output, input/output }
;     state           { closed, error, eof, binary, textual, auxstart, auxend }
;     transcoder      { Latin-1, UTF-8 }
;                   x { none, lf, cr, crlf, nel, crnel, ls }
;                   x { raise, replace, ignore }
;     mainbuf
;     auxbuf          4-byte bytevector
;
; An input port p can be in one of these states.
; FIXME: output ports still use old-style i/o system.
;
; closed          type = 0 & state = 'closed
;
; error           type > 0 & state = 'error
;
; eof             type > 0 & state = 'eof
;
; binary          type > 0 & state = 'binary
;
; textual         type > 0 & state = 'textual
;                 & mainbuf[mainlim] = 255
;                 & auxptr = auxlim = 0
;
; auxstart        type > 0 & state = 'auxstart
;                 & 0 = mainptr < mainlim & mainbuf[0] = 255
;                 & 0 <= auxptr < auxlim
;
; auxend          type > 0 & state = 'auxend
;                 & mainbuf[mainlim] = 255
;                 & 0 <= auxptr < auxlim
;
; In the auxstart state, the contents of auxbuf precede mainbuf.
; In the auxend state, the contents of auxbuf follow mainbuf.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The inlined operations (get-u8, get-char, put-u8, put-char)
; always access the mainbuf, calling out-of-line code for these
; specific cases:
;
;     get-u8      mainbuf is empty
;     put-u8      mainbuf is full
;     get-char    next byte is 13 or greater than 127
;     put-char    mainbuf is full or character is non-Ascii
;
; For a textual input port, the mainbuf always contains a
; sentinel that forces complicated situations to be handled
; by out-of-line code.  The other bytes of the mainbuf depend
; upon the transcoder:
;
;     Latin-1     mainbuf contains Latin-1
;     UTF-8       mainbuf contains UTF-8
;
; For output ports with a UTF-16 transcoder, trancoding from
; UTF-8 to UTF-16 will be performed out of line, when the buffer
; is flushed, or by a custom port.  (Haven't decided yet.)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Fields and offsets of a port structure.
; The most frequently accessed fields are at the beginning
; of the port structure, in hope of improving cache performance.

; NOTE that you can *not* change the offsets without also changing
; them in Compiler/common.imp.sch, where they are likely to be
; inlined.  They should not be used in any other files.

; The port type is a fixnum that encodes the binary/textual
; and input/output distinctions.  It is the inclusive or of
;     binary/textual:
;         0 means binary
;         1 means textual
;     direction:
;         0 means closed
;         2 means input
;         4 means output
;         6 means input/output

(define type-mask:binary/textual  1)
(define type-mask:direction       6)

(define type:binary               0)
(define type:textual              1)
(define type:input                2)
(define type:output               4)

(define type:closed               0)
(define type:binary-input         2)
(define type:textual-input        3)
(define type:binary-output        4)
(define type:textual-output       5)
(define type:binary-input/output  6)
(define type:textual-input/output 7)

; FIXME:
; The port.mainpos field is not always updated correctly.
; The output and input/output directions are not yet implemented.
; The input direction has not yet been converted to on-the-fly
; transcoding.

(define port.type       0) ; fixnum: see above for encoding
(define port.mainbuf    1) ; bytevector: verified UTF-8 followed by sentinel
(define port.mainptr    2) ; nonnegative fixnum: next loc in mainbuf
(define port.mainlim    3) ; nonnegative fixnum: sentinel in mainbuf
(define port.mainpos    4) ; fixnum: byte or character position - mainptr
(define port.transcoder 5) ; fixnum: see comment at make-transcoder

; common data

(define port.state      6) ; symbol: closed, error, eof, binary, textual,
                           ;     auxstart, auxend
(define port.iodata     7) ; port-specific data
(define port.ioproc     8) ; port*symbol -> varies-with-symbol

; output ports

(define port.wr-flush?  9) ; boolean: discretionary output flushing

; FIXME: Added by Will for the new R6RS-compatible i/o system.

(define port.auxbuf    10) ; bytevector: 4 bytes before or after mainbuf
(define port.auxptr    11) ; index of next byte in auxbuf
(define port.auxlim    12) ; 1 + index of last byte in auxbuf

; FIXME: These should go away soon.

; input ports

(define port.auxneeded 13) ; number of bytes needed to complete the char
(define port.bytelim   14) ; nonnegative fixnum: index beyond last char
(define port.byteptr   15) ; nonnegative fixnum: next loc for input
(define port.wr-ptr    16) ; nonnegative fixnum: next loc for output
(define port.position  17) ; nonnegative fixnum: number of bytes or
                           ; characters read or written, not counting
                           ; what's in the current mainbuf.
(define port.input?    18) ; boolean: an open input port
(define port.output?   19) ; boolean: an open output port
(define port.bytebuf   20) ; a bytevector or #f: i/o buffer

(define port.structure-size 21)      ; size of port structure

; Lengths of default i/o bytebufs.
;
; The char buffer must be at least two greater than
; the length of the longest UTF-8 encoding of any string
; that can be encoded within the byte buffer.  For the
; standard encodings, the worst case occurs when each
; byte turns into the 3-byte encoding of the replacement
; character.

(define port.bytebuf-size    1024)
(define port.mainbuf-size    1024)

; Textual input uses 255 as a sentinel byte to force
; inline code to call a procedure for the general case.
; Note that 255 is not a legal code unit of UTF-8.

(define port.sentinel 255)

;;; Private procedures (called only from code within this file)

; Works only on input ports.
; The main invariants may not hold here.
; In particular, the port may be in the textual state
; but have a nonempty auxbuf.

(define (io/fill-buffer! p)
  (vector-like-set! p port.mainpos
                    (+ (vector-like-ref p port.mainpos)
                       (vector-like-ref p port.mainptr)))
  (let ((r (((vector-like-ref p port.ioproc) 'read)
            (vector-like-ref p port.iodata)
            (vector-like-ref p port.mainbuf))))
    (cond ((eq? r 'eof)
           (io/set-eof-state! p))
          ((eq? r 'error)
           ; FIXME: should retry before giving up
           (io/set-error-state! p)
           (error "Read error on port " p)
           #t)
          ((and (fixnum? r) (>= r 0))
           (vector-like-set! p port.mainptr 0)
           (vector-like-set! p port.mainlim r))
          (else
           (io/set-error-state! p)
           (error "io/fill-buffer!: bad value " r " on " p)))
    (io/transcode-port! p)))

; The main buffer has just been filled, but the state has not been changed.
; If the port was in the textual state, it should enter the auxstart state.
; If the port was in the auxstart state, it should remain in that state.
; If the port was in the auxend state, it should enter the auxstart state.
; So the main job here is to convert the port to the auxstart state.

(define (io/transcode-port! p)
  (let* ((type       (vector-like-ref p port.type))
         (state      (vector-like-ref p port.state))
         (mainbuf    (vector-like-ref p port.mainbuf))
         (mainptr    (vector-like-ref p port.mainptr))
         (mainlim    (vector-like-ref p port.mainlim))
         (auxbuf     (vector-like-ref p port.auxbuf))
         (auxptr     (vector-like-ref p port.auxptr))
         (auxlim     (vector-like-ref p port.auxlim)))
    (assert (fx= mainptr 0))
    (cond ((fx= type type:binary-input) #t)
          ((fx= type type:textual-input)
           (case state
            ((closed error eof)
             (io/reset-buffers! p))
            ((textual auxstart auxend)
             (assert (< auxlim (bytevector-length auxbuf)))
             (bytevector-set! auxbuf auxlim (bytevector-ref mainbuf 0))
             (vector-like-set! p port.auxlim (+ auxlim 1))
             (bytevector-set! mainbuf 0 port.sentinel)
             (vector-like-set! p port.state 'auxstart))
            (else
             (error 'io/transcode-port! "internal error" p))))
          (else
           (error 'io/transcode-port! "internal error" p)))))

; Works only on output ports.

(define (io/flush-buffer p)
  (let ((wr-ptr (vector-like-ref p port.wr-ptr)))
    (if (> wr-ptr 0)
        (let ((r (((vector-like-ref p port.ioproc) 'write)
                  (vector-like-ref p port.iodata)
                  (vector-like-ref p port.bytebuf)
                  wr-ptr)))
          (vector-like-set! p port.position
                            (+ (vector-like-ref p port.position) wr-ptr))
          (cond ((eq? r 'ok)
                 (vector-like-set! p port.wr-ptr 0))
                ((eq? r 'error)
                 (io/set-error-state! p)
                 (error "Write error on port " p)
                 #t)
                (else
                 (io/set-error-state! p)
                 (error "io/flush-buffer: bad value " r " on " p)
                 #t))))))

; Converts port to a clean error state.

(define (io/set-error-state! p)
  (vector-like-set! p port.state 'error)
  (io/reset-buffers! p))

; Converts port to a clean eof state.

(define (io/set-eof-state! p)
  (vector-like-set! p port.state 'eof)
  (io/reset-buffers! p))

; Converts port to a clean closed state.
; FIXME:  Should this reduce the size of mainbuf?

(define (io/set-closed-state! p)
  (vector-like-set! p port.type  type:closed)
  (vector-like-set! p port.state 'closed)
  (io/reset-buffers! p))

; Resets buffers to an empty state.

(define (io/reset-buffers! p)
  (vector-like-set! p port.mainptr 0)
  (vector-like-set! p port.mainlim 0)
  (vector-like-set! p port.auxptr 0)
  (vector-like-set! p port.auxlim 0)
  (bytevector-set! (vector-like-ref p port.mainbuf) 0 port.sentinel))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public low-level interface (may be called from code in other files)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;
; Note: io/make-port defaults to textual (for backward compatibility)


(define (io/make-port ioproc iodata . rest)
  (let ((v (make-vector port.structure-size #f))
        (input? #f)
        (output? #f)
        (binary? #f)
        (textual? #f))
    (do ((l rest (cdr l)))
        ((null? l))
      (case (car l)
        ((input)   (set! input? #t)  (vector-set! v port.input? #t))
        ((output)  (set! output? #t) (vector-set! v port.output? #t))
        ((text)    (set! textual? #t))
        ((binary)  (set! binary? #t))
        ((flush)   (vector-set! v port.wr-flush? #t))
        (else      (error "make-port: bad attribute: " (car l))
                   #t)))
    (if (and binary? textual?)
        (error "make-port: binary incompatible with textual"))
    (vector-set! v
                 port.type
                 (cond ((and binary? input?)   type:binary-input)
                       ((and binary? output?)  type:binary-output)
                       ((and textual? input?)  type:textual-input)
                       ((and textual? output?) type:textual-output)
                       (input?                 type:textual-input)
                       (output?                type:textual-output)
                       ; FIXME: no input/output ports yet
                       (else
                        (error
                         'io/make-port "neither input nor output" rest))))
    (vector-set! v port.mainbuf (make-bytevector port.mainbuf-size))
    (vector-set! v port.mainptr 0)
    (vector-set! v port.mainlim 0)
    (vector-set! v port.mainpos 0)
    (vector-set! v port.transcoder
                   (if binary?
                       0
                       (native-transcoder)))
    (vector-set! v port.state (if binary? 'binary 'textual))
    (vector-set! v port.iodata iodata)
    (vector-set! v port.ioproc ioproc)
    (vector-set! v port.auxbuf (make-bytevector 4))
    (vector-set! v port.auxlim 0)

    ; These should go away soon.

    (vector-set! v port.auxneeded 0)
    (vector-set! v port.bytebuf (make-bytevector port.bytebuf-size))
    (vector-set! v port.bytelim 0)
    (vector-set! v port.byteptr 0)
    (vector-set! v port.wr-ptr 0)
    (vector-set! v port.position 0)
    (vector-set! v port.input? input?)
    (vector-set! v port.output? output?)

    (typetag-set! v sys$tag.port-typetag)
    (io/reset-buffers! v)                     ; inserts sentinel
    v))

; Port? is integrable.
; Eof-object? is integrable.

(define (io/input-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (vector-like-ref p port.type))))
         (fx= type:input (fxlogand direction type:input)))))

(define (io/output-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (vector-like-ref p port.type))))
         (fx= type:output (fxlogand direction type:output)))))

(define (io/open-port? p)
  (or (io/input-port? p) (io/output-port? p)))

; FIXME:  For v0.94 only, read-char and peek-char can read
; from binary ports, treating them as Latin-1.
;
; FIXME:  After v0.94, io/read-char and io/peek-char should
; just delegate to io/get-char.

(define (io/read-char p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf  (vector-like-ref p port.mainbuf))
            (ptr  (vector-like-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (begin (vector-like-set! p port.mainptr (+ ptr 1))
                            (integer->char unit))
                     (io/get-char p #f))))
              ((eq? type type:binary-input)
               (let ((x (io/get-u8 p #f)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error "read-char: not an input port: " p)
               #t)))
      (begin (error "read-char: not an input port: " p)
             #t)))

; FIXME:  See comments for io/read-char above.

(define (io/peek-char p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf  (vector-like-ref p port.mainbuf))
            (ptr  (vector-like-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (integer->char unit)
                     (io/get-char p #t))))
              ((eq? type type:binary-input)
               (let ((x (io/get-u8 p #t)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error "peek-char: not an input port: " p)
               #t)))
      (begin (error "peek-char: not an input port: " p)
             #t)))

(define (io/write-char c p)
  (if (and (port? p) (vector-like-ref p port.output?))
      (let ((buf (vector-like-ref p port.bytebuf))
            (ptr (vector-like-ref p port.wr-ptr)))
        (cond ((< ptr (bytevector-like-length buf))
               (bytevector-like-set! buf ptr (char->integer c))
               (vector-like-set! p port.wr-ptr (+ ptr 1))
               (unspecified))
              (else
               (io/flush-buffer p)
               (io/write-char c p))))
      (begin (error "write-char: not an output port: " p)
             #t)))

; In v0.93, other parts of the I/O system depended upon a string
; (rather than bytevector-like) buffer.  That should be fixed now,
; but the commented lines remain so they can be un-commented if
; we still need to work around that kind of bug.
;
; Also, for short strings, it might be more effective to copy rather than
; flush.  This procedure is really most useful for long strings, and was
; written to speed up fasl file writing.

(define (io/write-bytevector-like bvl p)
  (if (not (bytevector? bvl))                                         ;FIXME
      (begin (display "***** WARNING ***** from io/write-bytevector")
             (newline)))
  (if (and (port? p) (vector-like-ref p port.output?))
      (let ((buf (vector-like-ref p port.bytebuf))
            (tt  (typetag bvl)))
        (io/flush-buffer p)
        (vector-like-set! p port.bytebuf bvl)
        (vector-like-set! p port.wr-ptr (bytevector-like-length bvl))
       ;(typetag-set! bvl sys$tag.string-typetag)
        (io/flush-buffer p)
       ;(typetag-set! bvl tt)
        (vector-like-set! p port.bytebuf buf)
        (vector-like-set! p port.wr-ptr 0)
        (unspecified))
      (begin (error "io/write-bytevector-like: not an output port: " p)
             #t)))
  
; When writing the contents of an entire string,
; we can do the error checking just once.
; FIXME:  This outputs Ascii characters only.

(define (io/write-string s p)
  (if #t                                                 ; FIXME
      (io/write-substring s 0 (string-length s) p)
      (do ((n (string-length s))
           (i 0 (+ i 1)))
          ((= i n) (unspecified))
        (io/write-char (string-ref s i) p))))

(define (io/write-substring s i j p)
  (if (and (string? s)
           (fixnum? i)
           (fixnum? j)
           (<= 0 i j (string-length s))
           (port? p)
           (vector-like-ref p port.output?))
      (let loop ((i i))
        (if (< i j)
            (let* ((buf (vector-like-ref p port.bytebuf))
                   (len (bytevector-like-length buf))
                   (ptr (vector-like-ref p port.wr-ptr)))
              (let ((count (min (- j i) (- len ptr))))
                (if (< 0 count)
                    (begin
                     (vector-like-set! p port.wr-ptr (+ ptr count))
                     (do ((k (+ i count))
                          (i i (+ i 1))
                          (ptr ptr (+ ptr 1)))
                         ((= i k)
                          (loop i))
                       (let ((c (string-ref s i)))
                         (bytevector-like-set! buf ptr (char->integer c)))))
                    (begin (io/flush-buffer p)
                           (loop i)))))
            (unspecified)))
      (begin (error "io/write-substring: not an output port: " p)
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

; FIXME:  Should release buffers.

(define (io/close-port p)
  (if (port? p)
      (begin
        (if (io/output-port? p)
            (io/flush-buffer p))
        (((vector-like-ref p port.ioproc) 'close)
         (vector-like-ref p port.iodata))
        (io/set-closed-state! p)
        (unspecified))
      (begin (error "io/close-port: not a port: " p)
             #t)))

(define (io/port-name p)
  (((vector-like-ref p port.ioproc) 'name) (vector-like-ref p port.iodata)))

(define (io/port-error-condition? p)
  (and (port? p) (eq? (vector-like-ref p port.state) 'error)))

(define (io/port-at-eof? p)
  (and (port? p) (eq? (vector-like-ref p port.state) 'eof)))

(define (io/port-position p)
  (cond ((or (io/input-port? p) (io/output-port? p))
         (+ (vector-like-ref p port.mainpos)
            (vector-like-ref p port.mainptr)))
        (else
         (error "io/port-position: " p " is not an open port.")
         #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS i/o (preliminary and incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (io/port-transcoder p)
  (assert (port? p))
  (vector-like-ref p port.transcoder))

(define (io/textual-port? p)
  (assert (port? p))
  (not (fx= 0 (fxlogand type-mask:binary/textual
                        (vector-like-ref p port.type)))))

(define (io/binary-port? p)
  (assert (port? p))
  (fx= 0 (fxlogand type-mask:binary/textual (vector-like-ref p port.type))))

; Transcoders et cetera.
;
; Internally, transcoders are represented as fixnums of the form
; xxxyyyzz, where xxx encodes a codec, yyy encodes an eol style,
; and zz encodes an error handling mode.
;
; codec (3 bits):
;     000 means none (port is binary)
;     001 means Latin-1
;     010 means UTF-8
;     011 means UTF-16
;     111 means UTF-16 in little-endian mode (internal only)
; 
; eol style (3 bits):
;     000 means none
;     001 means lf
;     010 means nel
;     011 means ls
;     100 means cr
;     101 means crlf
;     110 means crnel
;
; error handling mode (2 bits):
;     00 means raise
;     01 means replace
;     10 means ignore
;
; FIXME:  The external world should see a more self-explanatory
; representation.  Transcoders etc probably ought to be a new
; category of miscellaneous objects.

(define transcoder-mask:codec    #b11100000)
(define transcoder-mask:eolstyle #b00011100)
(define transcoder-mask:errmode  #b00000011)

(define codec:binary  0)
(define codec:latin-1 #b00100000)
(define codec:utf-8   #b01000000)

(define eolstyle:none  #b00000)
(define eolstyle:lf    #b00100)
(define eolstyle:nel   #b01000)
(define eolstyle:ls    #b01100)
(define eolstyle:cr    #b10000)
(define eolstyle:crlf  #b10100)
(define eolstyle:crnel #b11000)

(define errmode:replace 0)
(define errmode:ignore  1)
(define errmode:raise   2)

(define (io/make-transcoder codec eol-style handling-mode)
  (define (local-error msg irritant)
    (assertion-violation 'make-transcoder msg irritant))
  (let ((xxx (case codec
              ((latin-1) 1)
              ((utf-8)   2)
              ((utf-16)  3)
              (else (local-error "unrecognized codec" codec))))
        (yyy (case eol-style
              ((none)  0)
              ((lf)    1)
              ((nel)   2)
              ((ls)    3)
              ((cr)    4)
              ((crlf)  5)
              ((crnel) 6)
              (else (local-error "unrecognized eol style" eol-style))))
        (zz (case handling-mode
             ((raise) 0)
             ((replace) 1)
             ((ignore) 2)
             (else (local-error "unrecognized error handling mode"
                                handling-mode)))))
    (fxlogior (fxlsh xxx 5) (fxlogior (fxlsh yyy 2) zz))))

; Like transcoded-port, but performs less error checking.

(define (io/transcoded-port p t)
  (assert (io/open-port? p))
  (assert (eq? 'binary (vector-like-ref p port.state)))
  (if (io/output-port? p)
      (io/flush p))
  ; shallow copy
  (let* ((n (vector-like-length p))
         (newport (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (vector-set! newport i (vector-like-ref p i)))
    (vector-set! newport
                 port.type
                 (fxlogior type:textual (vector-like-ref p port.type)))
    (vector-set! newport port.transcoder t)
    (vector-set! newport port.state 'textual)

    ; io/transcode-port! expects a newly filled mainbuf,
    ; so we have to fake it here.
    
    (let* ((mainbuf1 (vector-like-ref p port.mainbuf))
           (mainptr1 (vector-like-ref p port.mainptr))
           (mainlim1 (vector-like-ref p port.mainlim))
           (mainbuf2 (make-bytevector (bytevector-length mainbuf1))))

      (bytevector-copy! mainbuf1 mainptr1 mainbuf2 0 (fx- mainlim1 mainptr1))
      (vector-like-set! newport port.mainbuf mainbuf2))

    ; close original port, destroying original mainbuf

    (io/set-closed-state! p)

    (typetag-set! newport sys$tag.port-typetag)
    (if (io/input-port? newport)
        (io/transcode-port! newport))
    newport))

; FIXME: ignores file options, buffer mode, and mostly ignores transcoder.

(define (io/open-file-input-port filename options bufmode transcoder)
  (file-io/open-file filename 'input (if transcoder 'text 'binary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic operations.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The general case for get-u8 and lookahead-u8.

(define (io/get-u8 p lookahead?)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (cond ((not (eq? type 2))
           (assertion-violation 'get-u8
                                "not an open binary input port" p))
          ((fx< ptr lim)
           (let ((byte (bytevector-ref buf ptr)))
             (if (not lookahead?)
                 (vector-like-set! p port.mainptr (+ ptr 1)))
             byte))
          ((eq? (vector-like-ref p port.state) 'eof)
           (eof-object))
          (else
           ; The error state will be handled by io/fill-buffer!.
           (io/fill-buffer! p)
           (io/get-u8 p lookahead?)))))

; FIXME: transcoding-on-fly not implemented yet (incomplete characters?)
; The general case for get-char and lookahead-char.
; The transcoder will be for Latin-1 or UTF-8.
; FIXME: does not handle end-of-line conversions.
; FIXME: does not detect decoding errors.
; (That shouldn't matter right now since we're using Latin-1 or good UTF-8.)

(define (io/get-char p lookahead?)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (cond ((not (eq? type type:textual-input))
           (assertion-violation 'get-char
                                "argument not a textual input port" p))
          ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (cond ((and #f (fx= unit 13))             ; FIXME
                           ???)
                          (else
                           (if (not lookahead?)
                               (vector-like-set! p port.mainptr (+ ptr 1)))
                           (integer->char unit))))
                   ((and (fx= unit port.sentinel)
                         (fx= ptr 0)
                         (eq? (vector-like-ref p port.state) 'auxstart))
                    (io/get-char-auxstart p lookahead?))

                   (else
                    (let ((state (vector-like-ref p port.state)))
                      (case state
                       ((eof) (eof-object))
                       ((error) (error 'get-char "permanent read error" p))
                       ((textual auxend)
                        (let ((codec (fxlogand
                                      transcoder-mask:codec
                                      (vector-like-ref p port.transcoder))))
                          (cond ((fx= codec codec:latin-1)
                                 ; Latin-1
                                 (if (not lookahead?)
                                     (vector-like-set!
                                      p port.mainptr (+ ptr 1)))
                                 (integer->char unit))
                                ((fx= codec codec:utf-8)
                                 (io/get-char-utf-8
                                  p lookahead? unit buf ptr lim))
                                (else
                                 (error 'io/get-char "unimplemented codec" p)
                                 (eof-object)))))
                       (else
                        (error 'io/get-char "internal error" state p))))))))
          ((eq? (vector-like-ref p port.state) 'eof)
           (io/reset-buffers! p)  ; FIXME: probably redundant
           (eof-object))
          ((eq? (vector-like-ref p port.state) 'error)
           (io/reset-buffers! p)  ; FIXME: probably redundant
           (error 'get-char "permanent read error" p))
          ((eq? (vector-like-ref p port.state) 'auxend)
           (let* ((auxbuf (vector-like-ref p port.auxbuf))
                  (auxptr (vector-like-ref p port.auxptr))
                  (auxlim (vector-like-ref p port.auxlim))
                  (n      (- auxlim auxptr))
                  (mainbuf (vector-like-ref p port.mainbuf)))
             (assert (fx< auxptr auxlim))
             (bytevector-copy! auxbuf auxptr mainbuf 0 n)
             (bytevector-set! mainbuf n port.sentinel)
             (vector-like-set! p port.mainptr 0)
             (vector-like-set! p port.mainlim n)
             (vector-like-set! p port.auxptr 0)
             (vector-like-set! p port.auxlim 0)
             (vector-like-set! p port.state 'textual)
             (io/get-char p lookahead?)))
          (else
           (io/reset-buffers! p)
           (io/fill-buffer! p)
           (io/get-char p lookahead?)))))

; On-the-fly transcoding of a non-Ascii UTF-8 character.
; The first argument is known to be a textual input port
; whose transcoder uses the UTF-8 codec.
; The second argument is true if this is a lookahead operation.
; The third argument is the first code unit of the character.
; The last three arguments are for the active buffer, either
; mainbuf (if the state is textual or auxend)
; or auxbuf (if the state is auxstart).
;
; If the active buffer does not contain enough bytes, then
; the port must be forced into the auxstart state, and bytes
; must be transferred from mainbuf to auxbuf until the auxbuf
; contains a complete character.
;
; FIXME:  This routine is not yet implemented.

(define (io/get-char-utf-8 p lookahead? unit buf ptr lim)

  (define (decoding-error units)
    (for-each (lambda (x) (io/consume-byte! p)) units)
    (let* ((transcoder (vector-like-ref p port.transcoder))
           (errmode (fxlogand transcoder-mask:errmode transcoder)))
      (cond ((fx= errmode errmode:replace)
             (integer->char #xfffd))
            ((fx= errmode errmode:ignore)
             (io/get-char p lookahead?))
            (else
             (error 'get-char "utf-8 decoding error" units)))))

  ; Forces at least one more byte into the active buffer,
  ; and retries.

  (define (read-more-bytes)
    (let* ((state (vector-like-ref p port.state))
           (mainbuf (vector-like-ref p port.mainbuf))
           (mainptr (vector-like-ref p port.mainptr))
           (mainlim (vector-like-ref p port.mainlim))
           (auxbuf (vector-like-ref p port.auxbuf))
           (auxptr (vector-like-ref p port.auxptr))
           (auxlim (vector-like-ref p port.auxlim))
           (m (- mainlim mainptr))
           (n (- auxlim auxptr)))
      (case state

       ((auxend)
        (assert (eq? buf mainbuf))
        (assert (fx< 0 n))
        (bytevector-copy! mainbuf mainptr mainbuf 0 m)
        (bytevector-copy! auxbuf auxptr mainbuf m n)
        (bytevector-set! mainbuf (+ m n) port.sentinel)
        (vector-like-set! p port.mainptr 0)
        (vector-like-set! p port.mainlim (+ m n))
        (vector-like-set! p port.auxptr 0)
        (vector-like-set! p port.auxlim 0)
        (vector-like-set! p port.state 'textual)
        (io/get-char p lookahead?))

       ((textual)
        (assert (eq? buf mainbuf))
        (assert (fx= 0 auxlim))
        (assert (< m 4))
        (bytevector-copy! mainbuf mainptr auxbuf 0 m)
        (vector-like-set! p port.mainptr 0)
        (vector-like-set! p port.mainlim 0)
        (vector-like-set! p port.auxptr 0)
        (vector-like-set! p port.auxlim m)
        (io/fill-buffer! p)
        (io/get-char p lookahead?))

       ((auxstart)
        (assert (eq? buf auxbuf))
        (assert (fx= 0 auxptr))
        (assert (fx< n 3))
        (if (fx>= m 2)
            (begin

             ; Copy one byte from mainbuf to auxbuf,
             ; and move mainbuf down by 1.
             ; FIXME:  This is grossly inefficient, but works for now.

             (bytevector-set! auxbuf auxlim (bytevector-ref mainbuf 1))
             (bytevector-copy! mainbuf 2 mainbuf 1 (- m 2))
             (vector-like-set! p port.mainlim (- mainlim 1))
             (vector-like-set! p port.auxlim (+ auxlim 1))
             (io/get-char p lookahead?))

            (begin
             (io/fill-buffer! p)
             (io/get-char p lookahead?))))

       (else
        ; state is closed, error, eof, or binary
        (error 'io/get-char-utf-8 "internal error" state)))))

  (define (finish k sv)
    (if (not lookahead?)
        (let ((mainbuf (vector-like-ref p port.mainbuf)))
          (if (eq? mainbuf buf)
              (vector-like-set! p
                                port.mainptr
                                (+ k (vector-like-ref p port.mainptr)))
              (begin (io/consume-byte-from-auxbuf! p)
                     (io/consume-byte-from-auxbuf! p)
                     (if (> k 2) (io/consume-byte-from-auxbuf! p))
                     (if (> k 3) (io/consume-byte-from-auxbuf! p))))))
    (integer->char sv))

  (define (decode2) ; decodes 2 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1))))
      (if (<= #x80 unit2 #xbf)
          (finish 2
                  (fxlogior
                   (fxlsh (fxlogand #b00011111 unit) 6)
                   (fxlogand #b00111111 (bytevector-ref buf (+ ptr 1)))))
          (decoding-error (list unit unit2)))))

  (define (decode3) ; decodes 3 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1)))
          (unit3 (bytevector-ref buf (+ ptr 2))))
      (cond ((or (and (fx= unit #xe0)
                      (fx< unit2 #xa0))
                 (not (<= #x80 unit2 #xbf)))
             (decoding-error (list unit unit2)))
            ((not (<= #x80 unit3 #xbf))
             (decoding-error (list unit unit2 unit3)))
            (else
             (finish 3
                     (fxlogior
                      (fxlsh (fxlogand #b00001111 unit) 12)
                      (fxlogior
                       (fxlsh (fxlogand #x3f unit2) 6)
                       (fxlogand #x3f unit3))))))))

  (define (decode4) ; decodes 4 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1)))
          (unit3 (bytevector-ref buf (+ ptr 2)))
          (unit4 (bytevector-ref buf (+ ptr 3))))
      (cond ((or (and (fx= unit #xf0)
                      (fx< unit2 #x90))
                 (and (fx= unit #xf4)
                      (fx> unit2 #x8f))
                 (not (<= #x80 unit2 #xbf)))
             (decoding-error (list unit unit2)))
            ((not (<= #x80 unit3 #xbf))
             (decoding-error (list unit unit2 unit3)))
            ((not (<= #x80 unit4 #xbf))
             (decoding-error (list unit unit2 unit3 unit4)))
            (else
             (finish 4
                     (fxlogior
                      (fxlogior
                       (fxlsh (fxlogand #b00000111 unit) 18)
                       (fxlsh (fxlogand #x3f unit2) 12))
                      (fxlogior
                       (fxlsh (fxlogand #x3f unit3) 6)
                       (fxlogand #x3f unit4))))))))

  (define n (- lim ptr))

' (error 'io/get-char-utf-8 "not yet implemented" p) ;FIXME

  (cond ((< n 2)
         (read-more-bytes))
        ((<= unit #xc1)
         (decoding-error (list unit)))
        ((<= unit #xdf)
         (decode2))
        ((< n 3)
         (read-more-bytes))
        ((<= unit #xef)
         (decode3))
        ((< n 4)
         (read-more-bytes))
        ((<= unit #xf4)
         (decode4))
        (else
         (decoding-error (list unit)))))

; The special case of get-char and lookahead-char on a textual port
; that's in the auxstart state, where it reads from auxbuf instead
; of mainbuf.
; The port is known to be a textual input port in the auxstart state.
; FIXME:  For Latin-1 this is pretty easy, but it's a mess for UTF-8.

(define (io/get-char-auxstart p lookahead?)

  (assert (eq? 'auxstart (vector-like-ref p port.state)))

  (let ((buf  (vector-like-ref p port.auxbuf))
        (ptr  (vector-like-ref p port.auxptr))
        (lim  (vector-like-ref p port.auxlim)))

    (cond ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (cond ((and #f (fx= unit 13))             ; FIXME
                           ???)
                          (else
                           (if (not lookahead?)
                               (io/consume-byte-from-auxbuf! p))
                           (integer->char unit))))
                   ((let ((codec (fxlogand
                                  transcoder-mask:codec
                                  (vector-like-ref p port.transcoder))))
                      (fx= codec codec:latin-1))
                    ; Latin-1
                    (if (not lookahead?)
                        (io/consume-byte-from-auxbuf! p))
                    (integer->char unit))
                   (else  ; FIXME
                    (io/get-char-utf-8 p lookahead? unit buf ptr lim)))))
          (else
           ; In the auxstart state, auxbuf should always be nonempty.
           (error 'io/get-char-auxstart "internal error" p)
           (eof-object)))))

; Given an input port in auxstart state, consumes a byte from its auxbuf.
; If that empties auxbuf, then the port enters a textual or auxend state.

(define (io/consume-byte-from-auxbuf! p)

  (define (leave-auxstart-state!)
    (let ((mainbuf (vector-like-ref p port.mainbuf))
          (mainptr (vector-like-ref p port.mainptr))
          (mainlim (vector-like-ref p port.mainlim)))
      (assert (fx= 0 mainptr))
      (assert (fx< 0 mainlim))
      (vector-like-set! p port.mainptr 1)
      (if (fx< mainlim (bytevector-length mainbuf))
          (begin (bytevector-set! mainbuf mainlim port.sentinel)
                 (vector-like-set! p port.state 'textual))
          (begin (bytevector-set! (vector-like-ref p port.auxbuf)
                                  0
                                  (bytevector-ref mainbuf (- mainlim 1)))
                 (bytevector-set! mainbuf (- mainlim 1) port.sentinel)
                 (vector-like-set! p port.mainlim (- mainlim 1))
                 (vector-like-set! p port.auxptr 0)
                 (vector-like-set! p port.auxlim 1)
                 (vector-like-set! p port.state 'auxend)))))

  (assert (eq? 'auxstart (vector-like-ref p port.state)))

  (let* ((ptr (vector-like-ref p port.auxptr))
         (lim (vector-like-ref p port.auxlim))
         (ptr+1 (+ ptr 1)))

    (cond ((fx= ptr+1 lim)
           (vector-like-set! p port.auxptr 0)
           (vector-like-set! p port.auxlim 0)
           (leave-auxstart-state!))
          (else
           (vector-like-set! p port.auxptr ptr+1)))))

; Given an input textual port, consumes a byte from its buffers.
; This may cause a change of state.
; This procedure is called only during error handling, so it can be slow.

(define (io/consume-byte! p)
  (let ((state (vector-like-ref p port.state))
        (mainbuf (vector-like-ref p port.mainbuf))
        (mainptr (vector-like-ref p port.mainptr))
        (mainlim (vector-like-ref p port.mainlim))
        (auxbuf (vector-like-ref p port.auxbuf))
        (auxptr (vector-like-ref p port.auxptr))
        (auxlim (vector-like-ref p port.auxlim)))
    (case state
     ((auxstart)
      (io/consume-byte-from-auxbuf! p))
     ((textual auxend)
      (cond ((fx< mainptr mainlim)
             (vector-like-set! p mainptr (+ mainptr 1)))
            ((eq? state 'auxend)
             (assert (fx< auxptr auxlim))
             (bytevector-copy! auxbuf auxptr mainbuf 0 (- auxlim auxptr))
             (bytevector-set! mainbuf (- auxlim auxptr) port.sentinel)
             (vector-like-set! p port.mainptr 0)
             (vector-like-set! p port.mainlim (- auxlim auxptr))
             (vector-like-set! p port.auxptr 0)
             (vector-like-set! p port.auxlim 0)
             (vector-like-set! p port.state 'textual)
             (io/consume-byte! p))
            (else
             (io/reset-buffers! p))))
     (else
      ; must be closed, error, eof
      (unspecified)))))

; eof
