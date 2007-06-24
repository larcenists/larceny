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
;     type            { binary, textual } x { input, output, input/output }
;     transcoder      { Latin-1, UTF-8 }
;                   x { none, lf, cr, crlf, nel, crnel, ls }
;                   x { raise, replace, ignore }
;     mainbuf
;     auxbuf          4-byte bytevector
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
; For a textual input port, the contents of the mainbuf depend
; upon the transcoder:
;
;     Latin-1     mainbuf contains Latin-1 followed by sentinel
;     UTF-8       mainbuf contains UTF-8 followed by sentinel
;
; The auxbuf contains bytes that follow the bytes in the mainbuf,
; with one exception:
; If mainptr = 0, mainlim > 0, and mainbuf[0] is the sentinel,
; then the auxbuf contains bytes that precede those in mainbuf.
;
; For a textual output port, the mainbuf contains UTF-8.
; If the transcoding is Latin-1, then the UTF-8 will also be Latin-1.
; If the transcoding is UTF-16, then error-free transcoding from
; UTF-8 to UTF-16 will be performed when the buffer is flushed.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Fields and offsets of a port structure.
; The most frequently accessed fields are at the beginning
; of the port structure, in hope of improving cache performance.

; NOTE that you can *not* change the offsets without also changing
; them in Compiler/common.imp.sch, where they are likely to be
; inlined.  These offsets are also used (at present) in portio.sch.
; They should not be used elsewhere.

; The port type is a fixnum that encodes the binary/textual
; and input/output distinctions.  It is the inclusive or of
;     binary/textual:
;         0 means binary
;         1 means textual
;     direction:
;         2 means input
;         4 means output
;         6 means input/output
;
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

(define port.iodata     6) ; port-specific data
(define port.ioproc     7) ; port*symbol -> void
(define port.error?     8) ; boolean: #t after error

; input ports

(define port.rd-eof?    9) ; boolean: input port at EOF

; output ports

(define port.wr-flush? 10) ; boolean: discretionary output flushing

; FIXME: Added by Will for the new R6RS-compatible i/o system.

(define port.auxbuf    11) ; bytevector: 4 bytes that straddle bytebuf boundary
(define port.auxlim    12) ; number of ready bytes in auxbuf
(define port.auxneeded 13) ; number of bytes needed to complete the char

; FIXME: These should go away soon.

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
(define port.mainbuf-size   3100)

; Textual input uses a sentinel byte;
; It can be any value in 128..255 that is not a legal code unit of UTF-8.

(define port.sentinel 255)

;;; Private procedures

; Works only on input ports.
; FIXME: needs to exchange ports if appropriate

(define (io/fill-buffer p)
  (vector-like-set! p port.position 
                    (+ (vector-like-ref p port.position)
                       (vector-like-ref p port.byteptr)))
  (let ((r (((vector-like-ref p port.ioproc) 'read)
            (vector-like-ref p port.iodata)
            (vector-like-ref p port.bytebuf))))
    (cond ((eq? r 'eof)
           (vector-like-set! p port.byteptr 0)
           (vector-like-set! p port.bytelim 0)
           (vector-like-set! p port.rd-eof? #t))
          ((eq? r 'error)
           (vector-like-set! p port.error? #t)
           (error "Read error on port " p)
           #t)
          ((and (fixnum? r) (>= r 0))
           (vector-like-set! p port.byteptr 0)
           (vector-like-set! p port.bytelim r))
          (else
           (vector-like-set! p port.error? #t)
           (error "io/fill-buffer: bad value " r " on " p)))
    (if (= 2 (vector-like-ref p port.type))
        ; binary
        (begin
         (vector-like-set! p port.mainptr (vector-like-ref p port.byteptr))
         (vector-like-set! p port.mainlim (vector-like-ref p port.bytelim))))))


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
                 (vector-like-set! p port.error? #t)
                 (error "Write error on port " p)
                 #t)
                (else
                 (vector-like-set! p port.error? #t)
                 (error "io/flush-buffer: bad value " r " on " p)
                 #t))))))


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
    (vector-set! v port.ioproc ioproc)
    (vector-set! v port.iodata iodata)
    (vector-set! v port.bytebuf (make-bytevector port.bytebuf-size))
    (vector-set! v port.bytelim 0)
    (vector-set! v port.byteptr 0)
    (vector-set! v port.wr-ptr 0)
    (vector-set! v port.position 0)
    ; FIXME: added by Will
    ; Note: io/make-port defaults to textual (for backward compatibility)
    (vector-set! v port.type
                   (fxlogior (fxlogior (if input? 2 0) (if output? 4 0))
                             (if binary? 0 1)))
    (if binary?
        (vector-set! v port.mainbuf (vector-ref v port.bytebuf))
        (let ((mainbuf (make-bytevector port.mainbuf-size)))
          (vector-set! v port.mainbuf mainbuf)
          (bytevector-set! mainbuf 0 port.sentinel)))
    (vector-set! v port.mainptr 0)
    (vector-set! v port.mainlim 0)
    (vector-set! v port.mainpos 0)
    (vector-set! v port.auxbuf (make-bytevector 4))
    (vector-set! v port.auxlim 0)
    (vector-set! v port.auxneeded 0)
    (vector-set! v port.transcoder
                   (if binary?
                       0
                       (native-transcoder)))
    (typetag-set! v sys$tag.port-typetag)
    v))

; Port? is integrable.
; Eof-object? is integrable.

(define (io/input-port? p)
  (and (port? p) (vector-like-ref p port.input?)))

(define (io/output-port? p)
  (and (port? p) (vector-like-ref p port.output?)))

(define (io/open-port? p)
  (or (io/input-port? p) (io/output-port? p)))

; Moving the constants in-line improves performance because the global
; variable references are heavyweight -- several loads, and a check for
; definedness.
;
; FIXME:  But they aren't inlined right now.
;
; FIXME:  For v0.94 only, read-char and peek-char can read
; from binary ports, treating them as Latin-1.

(define (io/read-char p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf  (vector-like-ref p port.mainbuf))
            (ptr  (vector-like-ref p port.mainptr)))
        (cond ((eq? type 3)
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (begin (vector-like-set! p port.mainptr (+ ptr 1))
                            (integer->char unit))
                     (io/get-char p #f))))
              ((eq? type 2)
               (let ((x (io/get-u8 p #f)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error "read-char: not an input port: " p)
               #t)))
      (begin (error "read-char: not an input port: " p)
             #t)))

(define (io/peek-char p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf  (vector-like-ref p port.mainbuf))
            (ptr  (vector-like-ref p port.mainptr)))
        (cond ((eq? type 3)
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (integer->char unit)
                     (io/get-char p #t))))
              ((eq? type 2)
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

(define (io/port-error-condition? p)
  (vector-like-ref p port.error?))

(define (io/port-at-eof? p)
  (vector-like-ref p port.rd-eof?))

(define (io/port-position p)
  (cond ((io/input-port? p)
         (+ (vector-like-ref p port.position)
            (vector-like-ref p port.byteptr)))
        ((io/output-port? p)
         (+ (vector-like-ref p port.position)
            (vector-like-ref p port.wr-ptr)))
        (else
         (error "io/port-position: " p " is not an open port.")
         #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS i/o (preliminary and incomplete)
;
; FIXME: added by Will for transcoded textual i/o.
; Most of this belongs in ports.sch or stdio.sch.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (if (output-port? p)
      (io/flush p))
  ; shallow copy
  (let* ((n (vector-like-length p))
         (newport (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (vector-set! newport i (vector-like-ref p i)))
    (vector-set! newport port.type (fxlogior 1 (vector-like-ref p port.type)))
    (let ((bytebuf (vector-like-ref p port.bytebuf)))
      (assert bytebuf)
      (vector-set! newport port.mainbuf (make-bytevector port.mainbuf-size)))
    (bytevector-set! (vector-like-ref newport port.mainbuf) 0 port.sentinel)
    (vector-set! newport port.mainptr 0)
    (vector-set! newport port.mainlim 0)
    (vector-set! newport port.mainpos 0)
    (vector-set! newport port.transcoder t)
    ; close original port
    (vector-like-set! p port.input? #f)
    (vector-like-set! p port.output? #f)
    (typetag-set! newport sys$tag.port-typetag)
    (if (input-port? newport)
        (io/transcode-port! newport))
    newport))

; FIXME: ignores file options and buffer mode

(define (io/open-file-input-port filename options bufmode transcoder)
  (file-io/open-file filename 'input (if transcoder 'text 'binary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Transcoding of textual ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For an input port, translates byte buffer into UTF-8 in char buffer.
; For an output port, translates UTF-8 in char buffer to the byte buffer.
; FIXME:  For an input/output port, what should it do?
;
; Precondition: for an input port, the char buffer is empty.

(define (io/transcode-port! p)
  (let* ((type       (vector-like-ref p port.type))
         (mainlim    (vector-like-ref p port.mainlim))
         (transcoder (vector-like-ref p port.transcoder))
         (codec      (fxrshl transcoder 5))
         (eol-style  (fxlogand (fxrshl transcoder 2) 7))
         (err-mode   (fxlogand transcoder 3)))
    (case (fxrshl type 1)
     ((1)
      ; input
      (assert (= 0 mainlim))
      (if (not (vector-like-ref p port.rd-eof?))
          (case codec
           ((1)
            (io/transcode-input-port-latin-1! p eol-style err-mode))
           ((2)
            (io/transcode-input-port-utf-8! p eol-style err-mode))
           (else
            (assert (memq codec '(1 2)))))))
     ((2)
      (case codec
       ((1)
        (io/transcode-output-port-latin-1! p eol-style err-mode))
       ((2)
        (io/transcode-output-port-utf-8! p eol-style err-mode))
       (else
        (assert (memq codec '(1 2))))))
     ((3)
      ; input/output (FIXME)
      (assert (memq type '(input output)))))))

(define (io/transcode-input-port-latin-1! p eol-style err-mode)

  (let* ((bytebuf    (vector-like-ref p port.bytebuf))
         (byteptr    (vector-like-ref p port.byteptr))
         (bytelim    (vector-like-ref p port.bytelim))
         (bytepos    (vector-like-ref p port.position))
         (mainbuf    (vector-like-ref p port.mainbuf))
         (mainptr    (vector-like-ref p port.mainptr))
         (mainlim    (vector-like-ref p port.mainlim)))

    (define (loop i j)
      (if (= i bytelim)
          j
          (let ((sv (bytevector-ref bytebuf i)))
            (cond ((<= sv #x007f)
                   (bytevector-set! mainbuf j sv)
                   (loop (+ i 1) (+ j 1)))
                  (else
                   (let ((u0 (fxlogior #b11000000 (fxrshl sv 6)))
                         (u1 (bitwise-ior #b10000000 (fxlogand sv #x3f))))
                     (bytevector-set! mainbuf j u0)
                     (bytevector-set! mainbuf (+ j 1) u1)
                     (loop (+ i 1) (+ j 2))))))))

    (assert (= 0 (vector-like-ref p port.auxlim)))
    (assert (= 0 (vector-like-ref p port.auxneeded)))
    (assert (= 0 eol-style)) ; FIXME
    (let ((n (loop byteptr 0)))
      (vector-like-set! p port.byteptr 0)
      (vector-like-set! p port.bytelim 0)
      (vector-like-set! p port.position (+ bytepos bytelim))
      (vector-like-set! p port.mainptr 0)
      (vector-like-set! p port.mainlim n)
      (bytevector-set! mainbuf n port.sentinel))))

; Transcoding UTF-8 to UTF-8 is surprisingly painful,
; mainly because of error handling and incomplete
; encodings at the end of the binary buffer.

(define (io/transcode-input-port-utf-8! p eol-style err-mode)

  (let* ((bytebuf    (vector-like-ref p port.bytebuf))
         (byteptr    (vector-like-ref p port.byteptr))
         (bytelim    (vector-like-ref p port.bytelim))
         (bytepos    (vector-like-ref p port.position))
         (auxbuf     (vector-like-ref p port.auxbuf))
         (auxlim     (vector-like-ref p port.auxlim))
         (auxneeded  (vector-like-ref p port.auxneeded))
         (mainbuf    (vector-like-ref p port.mainbuf))
         (mainptr    (vector-like-ref p port.mainptr))
         (mainlim    (vector-like-ref p port.mainlim))
         (replacement0 #b11101111)
         (replacement1 #b10111111)
         (replacement2 #b10111101))

    ; i points to the next byte in bytebuf
    ; j points to the next char in mainbuf

    (define (loop i j)
      (if (= i bytelim)
          j
          (let ((u0 (bytevector-ref bytebuf i))
                (i+1 (+ i 1))
                (j+1 (+ j 1)))
            (bytevector-set! mainbuf j u0)
            (cond ((<= u0 #x7f)
                   (loop i+1 j+1))
                  ((< u0 #xc2)
                   (illegal i j i+1))
                  ((<= u0 #xdf)
                   (multibyte i j+1 i+1 #x80 #xbf 1))
                  ((= u0 #xe0)
                   (multibyte i j+1 i+1 #xa0 #xbf 2))
                  ((<= u0 #xef)
                   (multibyte i j+1 i+1 #x80 #xbf 2))
                  ((= u0 #xf0)
                   (multibyte i j+1 i+1 #x90 #xbf 3))
                  ((<= u0 #xf3)
                   (multibyte i j+1 i+1 #x80 #xbf 3))
                  ((= u0 #xf4)
                   (multibyte i j+1 i+1 #x80 #x8f 3))
                  (else
                   (illegal i j i+1))))))

    ; i points to the first byte of a multibyte encoding in bytebuf
    ; j points to the next byte in mainbuf
    ; k points to the second, third, or fourth byte
    ;     of the multibyte encoding in bytebuf
    ; lower is the inclusive lower bound for byte k
    ; upper is the inclusive upper bound for byte k
    ; count is the number of bytes still to come, including byte k

    (define (multibyte i j k lower upper count)
      (cond ((= 0 count)
             (loop k j))
            ((= k bytelim)
             (let* ((copied (- k i)))
               (leftover i (- j copied) copied count)))
            (else
             (let ((unit (bytevector-ref bytebuf k)))
               (bytevector-set! mainbuf j unit)
               (if (<= lower unit upper)
                   (multibyte i (+ j 1) (+ k 1) #x80 #xbf (- count 1))
                   (illegal i (- j (- k i)) k))))))

    ; Copies the extra bytes into auxbuf.
    ; Calculates and returns the new value for mainlim.

    (define (leftover i j extra needed)
      (do ((i i (+ i 1))
           (k 0 (+ k 1)))
          ((= k extra))
        (bytevector-set! auxbuf k (bytevector-ref bytebuf i)))
      (vector-like-set! p port.auxlim extra)
      (vector-like-set! p port.auxneeded needed)
      j)

    ; i points to the beginning of an illegal encoding in bytebuf
    ; j points to the next char in mainbuf
    ; k points past the illegal encoding in bytebuf

    (define (illegal i j k)
      (case err-mode
       ((1)
        ; replace
        (bytevector-set! mainbuf j replacement0)
        (bytevector-set! mainbuf (+ j 1) replacement1)
        (bytevector-set! mainbuf (+ j 2) replacement2)
        (loop k (+ j 3)))
       ((2)
        ; ignore
        (loop k j))
       ((0)
        ; FIXME: raise not yet implemented
        (assert #f))))

    ; Copies continuation bytes from bytebuf to auxbuf,
    ; converts to UTF-8, copies to mainbuf, and calls main loop.

    (define (auxloop i j needed)
      (cond ((= 0 needed)
             (vector-like-set! p port.byteptr i)
             (vector-like-set! p port.auxlim 0)
             (vector-like-set! p port.auxneeded 0)
             (if (<= j 1)
                 (bytevector-set! auxbuf 1 0))
             (if (<= j 2)
                 (bytevector-set! auxbuf 2 0))
             (if (<= j 3)
                 (bytevector-set! auxbuf 3 0))
             (let* ((s (utf8->string auxbuf))
                    (bv (string->utf8 (substring s 0 1)))
                    (n (bytevector-length bv)))
               ; FIXME: error handling might be needed here
               (bytevector-copy! bv 0 mainbuf 0 n)
               (mainloop i n)))
            ((and (< i bytelim)
                  (< j 4)
                  (<= #x80 (bytevector-ref bytebuf i) #xbf))
             (bytevector-set! auxbuf j (bytevector-ref bytebuf i))
             (auxloop (+ i 1) (+ j 1) (- needed 1)))
            ((and (= i bytelim) (< auxlim j))
             ; not enough bytes available to complete the character
             ; so skip main loop
             (vector-like-set! p port.byteptr i)
             (vector-like-set! p port.auxlim j)
             (vector-like-set! p port.auxneeded needed))
            (else
             ; next byte is wrong, or we made no progress
             (vector-like-set! p port.byteptr i)
             (vector-like-set! p port.auxlim 0)
             (vector-like-set! p port.auxneeded 0)
             ; FIXME:  Depends on illegal ignoring its first argument.
             (illegal i j i))))
            
    (define (mainloop i j)
      (let ((n (loop i j)))
        (vector-like-set! p port.byteptr 0)
        (vector-like-set! p port.bytelim 0)
        (vector-like-set! p port.position (+ bytepos bytelim))
        (vector-like-set! p port.mainptr 0)
        (vector-like-set! p port.mainlim n)
        (bytevector-set! mainbuf n port.sentinel)))

    (assert (= 0 eol-style)) ; FIXME
    (assert (= 2 err-mode)) ; FIXME

    (if (< 0 auxlim)
        (auxloop byteptr auxlim auxneeded)
        (mainloop byteptr 0))))

; eof
