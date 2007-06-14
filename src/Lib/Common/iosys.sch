; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- I/O system.
;
; Design: the system is designed so that in the common case, very few
; procedure calls are executed.

($$trace "iosys")

; NOTE that you can *not* change these values without also changing them
; in io/read-char, below, where they have been in-lined.

(define port.input?     0) ; boolean: an open input port
(define port.output?    1) ; boolean: an open output port
(define port.iodata     2) ; port-specific data
(define port.ioproc     3) ; port*symbol -> void
(define port.buffer     4) ; a bytevector or #f: i/o buffer
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

; FIXME: Added by Will for the new R6RS-compatible i/o system.

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

(define port.transcoder 19); fixnum: 0 for binary ports
                           ; for textual, see comment at make-transcoder

(define port.structure-size 20)      ; size of port structure
(define port.buffer-size    1024)    ; length of default I/O buffer
(define port.charbuf-size   3100)    ; length of default char buffer
                                     ; (must be at least two greater
                                     ; than the length of the longest
                                     ; UTF-8 encoding of any string that
                                     ; can be encoded in port.buffer-size
                                     ; bytes by a supported encoding;
                                     ; worst case is when a single byte
                                     ; turns into the 3-byte encoding
                                     ; of the replacement character)
(define port.sentinel 255)           ; not a legal code unit of UTF-8

;;; Private procedures

(define (io/fill-buffer p)
  (vector-like-set! p port.position 
                    (+ (vector-like-ref p port.position)
                       (vector-like-ref p port.rd-ptr)))
  (let ((r (((vector-like-ref p port.ioproc) 'read)
            (vector-like-ref p port.iodata)
            (vector-like-ref p port.buffer))))
    (cond ((eq? r 'eof)
           (vector-like-set! p port.rd-ptr 0)
           (vector-like-set! p port.rd-lim 0)
           (vector-like-set! p port.rd-eof? #t))
          ((eq? r 'error)
           (vector-like-set! p port.error? #t)
           (error "Read error on port " p)
           #t)
          ((and (fixnum? r) (>= r 0))
           (vector-like-set! p port.rd-ptr 0)
           (vector-like-set! p port.rd-lim r))
          (else
           (vector-like-set! p port.error? #t)
           (error "io/fill-buffer: bad value " r " on " p))))
  ; FIXME
  (if (and #f
           (eq? 1 (fxlogand 1 (vector-like-ref p port.type)))
           (= 0 (vector-like-ref p port.charlim)))
      (io/transcode-port! p)))

(define (io/flush-buffer p)
  (let ((wr-ptr (vector-like-ref p port.wr-ptr)))
    (if (> wr-ptr 0)
        (let ((r (((vector-like-ref p port.ioproc) 'write)
                  (vector-like-ref p port.iodata)
                  (vector-like-ref p port.buffer)
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
    (vector-set! v port.buffer (make-bytevector port.buffer-size))
    (vector-set! v port.rd-lim 0)
    (vector-set! v port.rd-ptr 0)
    (vector-set! v port.wr-ptr 0)
    (vector-set! v port.position 0)
    ; FIXME: added by Will
    ; Note: io/make-port defaults to textual (for backward compatibility)
    (vector-set! v port.type
                   (fxlogior (fxlogior (if input? 2 0) (if output? 4 0))
                             (if binary? 0 1)))
    (vector-set! v
                 port.charbuf
                 (make-bytevector port.charbuf-size))
    (vector-set! v port.charptr 0)
    (vector-set! v port.charlim 0)
    (vector-set! v port.charpos 0)
    (vector-set! v port.auxbuf (make-bytevector 4))
    (vector-set! v port.auxlim 0)
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

(define (io/read-char p)
  (if (and (port? p) (vector-like-ref p 0))          ; 0 = port.input?
      (let ((ptr (vector-like-ref p 8))              ; 8 = port.rd-ptr
            (lim (vector-like-ref p 7))              ; 7 = port.rd-lim
            (buf (vector-like-ref p 4)))             ; 4 = port.buffer
        (cond ((< ptr lim)
               (let ((c (integer->char (bytevector-like-ref buf ptr))))
                 (vector-like-set! p 8 (+ ptr 1))    ; 8 = port.rd-ptr
                 c))
              ((vector-like-ref p 6)                 ; 6 = port.rd-eof?
               (eof-object))
              (else
               (io/fill-buffer p)
               (io/read-char p))))
      (begin (error "read-char: not an input port: " p)
             #t)))

(define (io/peek-char p)
  (if (and (port? p) (vector-like-ref p port.input?))
      (let ((ptr (vector-like-ref p port.rd-ptr))
            (lim (vector-like-ref p port.rd-lim))
            (buf (vector-like-ref p port.buffer)))
        (cond ((< ptr lim)
               (integer->char (bytevector-like-ref buf ptr)))
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
  (if (and (port? p) (vector-like-ref p port.input?))
      (let ((ptr (vector-like-ref p port.rd-ptr))
            (lim (vector-like-ref p port.rd-lim))
            (buf (vector-like-ref p port.buffer)))
        (cond ((< ptr lim)
               (let ((ptr (+ ptr 1)))
                 (vector-like-set! p port.rd-ptr ptr)
                 (if (< ptr lim)
                     (integer->char (bytevector-like-ref buf ptr))
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
      (let ((buf (vector-like-ref p port.buffer))
            (tt  (typetag bvl)))
        (io/flush-buffer p)
        (vector-like-set! p port.buffer bvl)
        (vector-like-set! p port.wr-ptr (bytevector-like-length bvl))
       ;(typetag-set! bvl sys$tag.string-typetag)
        (io/flush-buffer p)
       ;(typetag-set! bvl tt)
        (vector-like-set! p port.buffer buf)
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
            (let* ((buf (vector-like-ref p port.buffer))
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
            (vector-like-ref p port.rd-ptr)))
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

(define (latin-1-codec) 'latin-1)
(define (utf-8-codec) 'utf-8)
(define (utf-16-codec) 'utf-16)

(define (native-eol-style) 'none)   ; FIXME: for backward compatibility

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

(define (make-transcoder codec . rest)
  (cond ((null? rest)
         (io/make-transcoder codec (native-eol-style) 'raise))
        ((null? (cdr rest))
         (io/make-transcoder codec (car rest) 'raise))
        ((null? (cddr rest))
         (io/make-transcoder codec (car rest) (cadr rest)))
        (else
         (assertion-violation 'make-transcoder
                              "wrong number of arguments"
                              (cons codec rest)))))

(define (native-transcoder) (make-transcoder (latin-1-codec) 'none 'ignore))

(define (transcoder-codec t)
  (case (fxrshl t 5)
   ((1) 'latin-1)
   ((2) 'utf-8)
   ((3) 'utf-16)
   (else (assertion-violation 'transcoder-codec
                              "weird transcoder" t))))

(define (transcoder-eol-style t)
  (case (fxlogand (fxrshl t 2) 7)
   ((0) 'none)
   ((1) 'lf)
   ((2) 'nel)
   ((3) 'ls)
   ((4) 'cr)
   ((5) 'crlf)
   ((6) 'crnel)
   (else (assertion-violation 'transcoder-eol-style
                              "weird transcoder" t))))

(define (transcoder-error-handling-mode t)
  (case (fxlogand t 3)
   ((0) 'raise)
   ((1) 'replace)
   ((2) 'ignore)
   (else (assertion-violation 'transcoder-error-handling-mode
                              "weird transcoder" t))))

(define (port-transcoder p)
  (assert (port? p))
  (vector-like-ref p port.transcoder))

(define (textual-port? p)
  (assert (port? p))
  (not (zero? (fxlogand 1 (vector-like-ref p port.type)))))

(define (binary-port? p)
  (assert (port? p))
  (zero? (fxlogand 1 (vector-like-ref p port.type))))

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
    (let ((bytebuf (vector-like-ref p port.buffer)))
      (assert bytebuf)
      (vector-set! newport port.charbuf (make-bytevector port.charbuf-size)))
    (bytevector-set! (vector-ref newport port.charbuf) 0 port.sentinel)
    (vector-set! newport port.charptr 0)
    (vector-set! newport port.charlim 0)
    (vector-set! newport port.charpos 0)
    (vector-set! newport port.transcoder t)
    ; close original port
    (vector-like-set! p port.input? #f)
    (vector-like-set! p port.output? #f)
    (typetag-set! newport sys$tag.port-typetag)
    (if (input-port? newport)
        (io/transcode-port! newport))
    newport))

(define (transcoded-port p t)
  (if (and (binary-port? p)
           (memq (transcoder-codec t) '(latin-1 utf-8))
           (eq? (transcoder-eol-style t) 'none)
           (eq? (transcoder-error-handling-mode t) 'ignore))
      (io/transcoded-port p t)
      (assertion-violation 'transcoded-port
                           "bad port or unsupported transcoder" p t)))

; FIXME:  For now, all binary and textual ports support port-position.

(define (port-has-port-position? p)
  (or (binary-port? p) (textual-port? p)))

(define (port-position p)
  (if (binary-port? p)
      (+ (vector-like-ref p port.position)
         (if (input-port? p)
             (vector-like-ref p port.rd-ptr)
             (vector-like-ref p port.wr-lim)))
      (+ (vector-like-ref p port.charpos)
         (vector-like-ref p port.charptr))))

; FIXME:  For now, no ports support set-port-position!.

(define (port-has-set-port-position!? p) #f)

(define (set-port-position! p pos)
  (assertion-violation 'set-port-position! "not yet implemented"))

(define (close-port p)
  (io/close-port p))

(define (call-with-port p f)
  (call-with-values
   (lambda () (f p))
   (lambda results
     (if (not (io/open-port? p)) (io/close-port p))
     (apply values results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: input-port? is defined elsewhere.

(define (port-eof? p)
  (assert (io/input-port? p))
  (cond ((binary-port? p)
         (eof-object? (lookahead-u8 p)))
        ((textual-port? p)
         (eof-object? (lookahead-char p)))
        (else
         ; probably closed
         #f)))

; FIXME: ignores file options and buffer mode

(define (io/open-file-input-port filename options bufmode transcoder)
  (file-io/open-file filename 'input (if transcoder 'text 'binary)))

; FIXME: fakes file options

(define (open-file-input-port filename . rest)
  (cond ((null? rest)
         (io/open-file-input-port filename '() 'block #f))
        ((null? (cdr rest))
         (io/open-file-input-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (io/open-file-input-port filename (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (io/open-file-input-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-input-port
                              "wrong number of arguments"
                              (cons filename rest)))))

; FIXME:  This is crude, and belongs in another file.

(define (open-bytevector-input-port bv . rest)
  (let ((transcoder (if (null? rest) #f (car rest)))
        (port (bytevector-io/open-input-bytevector bv)))
    (if transcoder
        (transcoded-port port transcoder)
        port)))

; FIXME:  This is crude, and belongs in another file.

(define (open-string-input-port s)
  (let ((transcoder (make-transcoder (utf-8-codec) 'none 'ignore))
        (port (bytevector-io/open-input-bytevector (string->utf8 s))))
    (transcoded-port port transcoder)))

; FIXME: not implemented yet

(define (standard-input-port)
  (assertion-violation 'standard-input-port "not yet implemented"))

; FIXME: current-input-port is implemented elsewhere

; FIXME: not implemented yet

(define (make-custom-binary-input-port
         id read! get-position set-position! close)
  (assertion-violation 'make-custom-binary-input-port "not yet implemented"))

; FIXME: not implemented yet

(define (make-custom-textual-input-port
         id read! get-position set-position! close)
  (assertion-violation 'make-custom-textual-input-port "not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic input (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The fast path for get-u8.

(define (get-u8 p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.buffer))
        (ptr  (vector-like-ref p port.rd-ptr))
        (lim  (vector-like-ref p port.rd-lim)))
    (if (and (eq? (fxlogand type 3) 2)              ; must be binary input port
             (fx< ptr lim))
        (let ((byte (bytevector-ref buf ptr)))
          (vector-like-set! p port.rd-ptr (+ ptr 1))
          byte)
        (io/get-u8 p))))

; The general case for get-u8.

(define (io/get-u8 p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.buffer))
        (ptr  (vector-like-ref p port.rd-ptr))
        (lim  (vector-like-ref p port.rd-lim)))
    (cond ((not (eq? (fxlogand type 3) 2))
           (assertion-violation 'get-u8 "argument not a binary input port" p))
          ((fx< ptr lim)
           (let ((byte (bytevector-ref buf ptr)))
             (vector-like-set! p port.rd-ptr (+ ptr 1))
             byte))
          ((vector-like-ref p port.rd-eof?)
           (eof-object))
          (else
           (io/fill-buffer p)
           (io/get-u8 p)))))

; The fast path for get-char.

(define (get-char p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.charbuf))
        (ptr  (vector-like-ref p port.charptr)))
    (assert (eq? (fxlogand type 3) 3))
    (let ((unit (bytevector-ref buf ptr)))
      (if (< unit 128)
          (begin (vector-like-set! p port.charptr (+ ptr 1))
                 (integer->char unit))
          (io/get-char p)))))

; The general case for get-char.

(define (io/get-char p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.charbuf))
        (ptr  (vector-like-ref p port.charptr))
        (lim  (vector-like-ref p port.charlim)))
    (cond ((not (eq? (fxlogand type 3) 3))
           (assertion-violation 'get-char
                                "argument not a textual input port" p))
          ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (vector-like-set! p port.charptr (+ ptr 1))
                    (integer->char unit))
                   ((<= unit #xdf)
                    (vector-like-set! p port.charptr (+ ptr 2))
                    (integer->char
                     (fxlogior
                      (fxlsh (fxlogand #b00011111 unit) 6)
                      (fxlogand #b00111111 (bytevector-ref buf (+ ptr 1))))))
                   ((<= unit #xef)
                    (vector-like-set! p port.charptr (+ ptr 3))
                    (integer->char
                     (fxlogior
                      (fxlsh (fxlogand #b00001111 unit) 12)
                      (fxlogior
                       (fxlsh (fxlogand #x3f (bytevector-ref buf (+ ptr 1)))
                              6)
                       (fxlogand #x3f (bytevector-ref buf (+ ptr 2)))))))
                   (else
                    (vector-like-set! p port.charptr (+ ptr 4))
                    (integer->char
                     (fxlogior
                      (fxlogior
                       (fxlsh (fxlogand #b00000111 unit) 18)
                       (fxlsh (fxlogand #x3f (bytevector-ref buf (+ ptr 1)))
                              12))
                      (fxlogior
                       (fxlsh (fxlogand #x3f (bytevector-ref buf (+ ptr 2)))
                              6)
                       (fxlogand #x3f (bytevector-ref buf (+ ptr 3))))))))))
          ((vector-like-ref p port.rd-eof?)
           (vector-like-set! p port.charptr 0)
           (vector-like-set! p port.charlim 0)
           (bytevector-set! buf 0 port.sentinel)
           (eof-object))
          (else
           (vector-like-set! p port.charptr 0)
           (vector-like-set! p port.charlim 0)
           (io/fill-buffer p)
           (io/transcode-port! p)
           (io/get-char p)))))

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
         (charlim    (vector-like-ref p port.charlim))
         (transcoder (vector-like-ref p port.transcoder))
         (codec      (fxrshl transcoder 5))
         (eol-style  (fxlogand (fxrshl transcoder 2) 7))
         (err-mode   (fxlogand transcoder 3)))
    (case (fxrshl type 1)
     ((1)
      ; input
      (assert (= 0 charlim))
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

  (let* ((bytebuf    (vector-like-ref p port.buffer))
         (byteptr    (vector-like-ref p port.rd-ptr))
         (bytelim    (vector-like-ref p port.rd-lim))
         (bytepos    (vector-like-ref p port.position))
         (charbuf    (vector-like-ref p port.charbuf))
         (charptr    (vector-like-ref p port.charptr))
         (charlim    (vector-like-ref p port.charlim)))

    (define (loop i j)
      (if (= i bytelim)
          j
          (let ((sv (bytevector-ref bytebuf i)))
            (cond ((<= sv #x007f)
                   (bytevector-set! charbuf j sv)
                   (loop (+ i 1) (+ j 1)))
                  (else
                   (let ((u0 (fxlogior #b11000000 (fxrshl sv 6)))
                         (u1 (bitwise-ior #b10000000 (fxlogand sv #x3f))))
                     (bytevector-set! charbuf j u0)
                     (bytevector-set! charbuf (+ j 1) u1)
                     (loop (+ i 1) (+ j 2))))))))

    (assert (= 0 (vector-like-ref p port.auxlim)))
    (assert (= 0 eol-style)) ; FIXME
    (let ((n (loop byteptr 0)))
      (vector-like-set! p port.rd-ptr 0)
      (vector-like-set! p port.rd-lim 0)
      (vector-like-set! p port.position (+ bytepos bytelim))
      (vector-like-set! p port.charptr 0)
      (vector-like-set! p port.charlim n)
      (bytevector-set! charbuf n port.sentinel))))

; FIXME:  This implementation makes two passes over the char buffer.

(define (io/transcode-input-port-utf-8! p eol-style err-mode)

  (let* ((bytebuf    (vector-like-ref p port.buffer))
         (byteptr    (vector-like-ref p port.rd-ptr))
         (bytelim    (vector-like-ref p port.rd-lim))
         (bytepos    (vector-like-ref p port.position))
         (auxbuf     (vector-like-ref p port.auxbuf))
         (auxlim     (vector-like-ref p port.auxlim))
         (charbuf    (vector-like-ref p port.charbuf))
         (charptr    (vector-like-ref p port.charptr))
         (charlim    (vector-like-ref p port.charlim))
         (replacement0 #b11101111)
         (replacement1 #b10111111)
         (replacement2 #b10111101))

    ; i points to the next byte in bytebuf
    ; j points to the next char in charbuf

    (define (loop i j)
      (if (= i bytelim)
          j
          (let ((u0 (bytevector-ref bytebuf i))
                (i+1 (+ i 1))
                (j+1 (+ j 1)))
            (bytevector-set! charbuf j u0)
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
    ; j points to the next byte in charbuf
    ; k points to the second, third, or fourth byte
    ;     of the multibyte encoding in bytebuf
    ; lower is the inclusive lower bound for byte k
    ; upper is the inclusive upper bound for byte k
    ; count is the number of bytes still to come, including byte k

    (define (multibyte i j k lower upper count)
      (cond ((= 0 count)
             (loop k j))
            ((= k bytelim)
             (leftover i j (- k i)))
            (else
             (let ((unit (bytevector-ref bytebuf (+ i 1))))
               (bytevector-set! charbuf j unit)
               (if (<= lower unit upper)
                   (multibyte i (+ j 1) (+ k 1) #x80 #xbf (- count 1))
                   (illegal i (- j (- k i)) k))))))

    ; Copies the extra bytes into auxbuf.
    ; Calculates and returns the new value for charlim.

    (define (leftover i j extra)
      (let ((i (- i extra))
            (j (- j extra)))
        (do ((i i (+ i 1))
             (k 0 (+ k 1)))
            ((= k extra))
          (bytevector-set! auxbuf k (bytevector-ref bytebuf i)))
        (vector-like-set! p port.auxlim extra)
        j))

    ; i points to the beginning of an illegal encoding in bytebuf
    ; j points to the next char in charbuf
    ; k points past the illegal encoding in bytebuf

    (define (illegal i j k)
      (case err-mode
       ((1)
        ; replace
        (bytevector-set! charbuf j replacement0)
        (bytevector-set! charbuf (+ j 1) replacement1)
        (bytevector-set! charbuf (+ j 2) replacement2)
        (loop k (+ j 3)))
       ((2)
        ; ignore
        (loop k j))
       ((0)
        ; FIXME: raise not yet implemented
        (assert #f))))

    ; Copies continuation bytes from bytebuf to auxbuf,
    ; converts to UTF-8, copies to charbuf, and calls main loop.
    ;
    ; FIXME:  This isn't working yet.  It also has a second bug:
    ; it doesn't allow for auxbuf to be incomplete even after
    ; a new byte is added.

    (define (auxloop i j)
      (cond ((and (< i bytelim)
                  (< j 4)
                  (<= #x80 (bytevector-ref bytebuf i) #xbf))
             (bytevector-set! auxbuf j (bytevector-ref bytebuf i))
             (auxloop (+ i 1) (+ j 1)))
            (else
             (if (<= j 1)
                 (bytevector-set! auxbuf 1 0))
             (if (<= j 2)
                 (bytevector-set! auxbuf 2 0))
             (if (<= j 3)
                 (bytevector-set! auxbuf 3 0))
             (let* ((s (utf8->string auxbuf))
                    (bv (string->utf8 (substring s 0 1)))
                    (n (bytevector-length bv)))
               (do ((i 0 (+ i 1)))
                   ((= i n))
                 (bytevector-set! charbuf 0 (bytevector-ref auxbuf i)))
               (mainloop i 1)))))

    (define (mainloop i j)
      (let ((n (loop i j)))
        (vector-like-set! p port.rd-ptr 0)
        (vector-like-set! p port.rd-lim 0)
        (vector-like-set! p port.position (+ bytepos bytelim))
        (vector-like-set! p port.charptr 0)
        (vector-like-set! p port.charlim n)
        (bytevector-set! charbuf n port.sentinel)))

    (assert (= 0 eol-style)) ; FIXME
    (assert (= 2 err-mode)) ; FIXME

    (if (< 0 auxlim)
        (auxloop byteptr auxlim)
        (mainloop byteptr 0))))

; eof
