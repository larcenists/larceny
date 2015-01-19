; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- I/O system.
;
; Design: the system is designed so that in the common case
; of an Ascii character being read from or written to a port
; whose buffer is neither empty nor full, very few procedure
; calls are executed.

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
; An input (but not input/output) port p can be in one of these states:
;
; closed          type = 0 & state = 'closed
;
; error           type > 0 & state = 'error
;
; eof             type > 0 & state = 'eof
;
; binary          type = <binary,input> & state = 'binary
;
; textual         (type = <textual,input> & state = 'textual
;                 & mainbuf[mainlim] = 255
;                 & auxptr = auxlim = 0)
;              or (type = <textual,output> & state = 'textual
;                 & mainptr = 0)
;
; auxstart        type = <textual,input> & state = 'auxstart
;                 & 0 = mainptr < mainlim & mainbuf[0] = 255
;                 & 0 <= auxptr < auxlim
;
; auxend          type = <textual,input> & state = 'auxend
;                 & mainbuf[mainlim] = 255
;                 & 0 <= auxptr < auxlim
;
; In the auxstart state, the contents of auxbuf precede mainbuf.
; In the auxend state, the contents of auxbuf follow mainbuf.
;
; In the textual state, the buffered bytes lie between mainptr
; and mainlim.
;
; Input/output ports preserve this invariant:
;
; closed          type = <_,input/output> & state = 'closed
;
; error           type > <_,input/output> & state = 'error
;
; eof             type = <_,input/output> & state = 'eof
;
; input/output    type = <_,input/output> & state = 'input/output
;                 & mainptr = 0
;                 & mainlim <= 4
;
; The buffer is empty iff mainptr = 0.  If mainlim > 0, then
; the buffer contains exactly one lookahead byte or character
; (in UTF-8).  Aside from the one byte or character of
; lookahead, input/output ports are never buffered.
;
; Input/output ports are different in several other ways:
;
;     Input/output ports must support set-port-position! so
;     they can undo lookahead buffering when switching from
;     input to output.  (See io/put-X-input/output.)
;
;     A binary input/output port's ioproc never reads or writes
;     more than one byte at a time.
;
;     A textual input/output port's ioproc never reads or writes
;     more than one character at a time.
;
;     If a textual input/output port is transcoded, then all
;     transcoding and end-of-line handling is done by the port's
;     ioproc, not by the on-the-fly transcoding in this file.
;
; Inlined read and write operations on input/output ports
; always perform a closed call to the main read and write
; routines defined in this file.
;
; FIXME:  For textual output ports, keeping the buffered bytes
; between 0 and mainptr *might* be better.  This might improve
; combined input/output ports.
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

; The port type is a fixnum that encodes the binary/textual
; and input/output distinctions.  It is the inclusive or of
;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fields and offsets of a port structure.
; The most frequently accessed fields are at the beginning
; of the port structure, in hope of improving cache performance.
;
; NOTE that you can *not* change the offsets without also changing
; them in Compiler/common.imp.sch, where they are likely to be
; inlined.  The offsets may also be used in the following files:
;
;     bytevectorio.sch
;     stringio.sch
;     transio.sch
;
; They should not be used in any other files.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port.type       0) ; fixnum: see above for encoding
(define port.mainbuf    1) ; bytevector: Latin-1 or UTF-8 encodings
(define port.mainptr    2) ; nonnegative fixnum: next loc in mainbuf (input)
(define port.mainlim    3) ; nonnegative fixnum: sentinel in mainbuf (input)

; For input ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainptr field.
;
; For output ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainlim field.

(define port.mainpos    4) ; integer: byte/char position - (mainptr or mainlim)
(define port.transcoder 5) ; fixnum: see comment at make-transcoder

; The state is always one of the following symbols:
;     closed error eof
;     binary
;     textual auxstart auxend
;     input/output

(define port.state      6) ; symbol: see above
(define port.iodata     7) ; port-specific data
(define port.ioproc     8) ; port*symbol -> varies-with-symbol

; output ports

(define port.bufmode    9) ; symbol: none, line, datum, block
(define port.wr-flush? 10) ; boolean: true iff bufmode is datum

; textual input ports

(define port.auxbuf    11) ; bytevector: 4 bytes before or after mainbuf
(define port.auxptr    12) ; fixnum: index of next byte in auxbuf
(define port.auxlim    13) ; fixnum: 1 + index of last byte in auxbuf
(define port.linesread 14) ; integer: number of line endings read so far
(define port.linestart 15) ; integer: character position after last line ending
(define port.wasreturn 16) ; boolean: last line ending was #\return
(define port.readmode  17) ; fixnum: see comment before default-read-mode

; all ports

(define port.setposn   18) ; boolean: true iff supports set-port-position!
(define port.alist     19) ; association list: used mainly by custom ports

(define port.structure-size 20)      ; size of port structure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous constants.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Default length of i/o buffer.

(define port.mainbuf-size    1024)

; Textual input uses 255 as a sentinel byte to force
; inline code to call a procedure for the general case.
; Note that 255 is not a legal code unit of UTF-8.

(define port.sentinel 255)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public low-level interface (may be called from code in other files)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (io/initialize)
  ; Nothing, for the time being.
  #t)

(define (io/finalize)
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
; If the port supports set-port-position!, then ioproc also performs
;
;   set-position! : iodata * posn -> { 'ok, 'error }
;
; Note: io/make-port defaults to textual (for backward compatibility)

(define (io/make-port ioproc iodata . rest)
  (let ((v (make-vector port.structure-size #f))
        (input? #f)
        (output? #f)
        (binary? #f)
        (textual? #f)
        (set-position? #f))

    (vector-set! v port.bufmode 'block) ; default buffer mode is block

    ; Parse keyword arguments.

    (for-each
     (lambda (keyword)
       (case keyword
        ((input)         (set! input? #t))
        ((output)        (set! output? #t))
        ((text)          (set! textual? #t))
        ((binary)        (set! binary? #t))
        ((set-position!) (set! set-position? #t))
        ((none)          (vector-set! v port.bufmode 'none))
        ((line)          (vector-set! v port.bufmode 'line))
        ((datum flush)   (vector-set! v port.bufmode 'datum)
                         (vector-set! v port.wr-flush? #t))
        ((block)         (vector-set! v port.bufmode 'block))
        (else
         (assertion-violation 'io/make-port "bad attribute" (car rest))
         #t)))
     rest)

    (if (and binary? textual?)
        (assertion-violation 'io/make-port "binary incompatible with textual"))
    (vector-set! v
                 port.type
                 (cond
                  ((and binary? input? output?)  type:binary-input/output)
                  ((and binary? input?)          type:binary-input)
                  ((and binary? output?)         type:binary-output)
                  ((and textual? input? output?) type:textual-input/output)
                  ((and textual? input?)         type:textual-input)
                  ((and textual? output?)        type:textual-output)
                  (input?                        type:textual-input)
                  (output?                       type:textual-output)
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
    (vector-set! v port.linesread 0)
    (vector-set! v port.linestart 0)
    (vector-set! v port.wasreturn #f)
    (vector-set! v port.readmode
                   (if (not binary?)
                       (default-read-mode)
                       readmode:binary))

    (vector-set! v port.setposn set-position?)
    (vector-set! v port.alist '())

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

(define (io/buffer-mode p)
  (assert (port? p))
  (vector-like-ref p port.bufmode))

; FIXME:  For v0.94 only, read-char and peek-char can read
; from binary ports, treating them as Latin-1.
;
; FIXME:  After v0.94, io/read-char and io/peek-char should
; just delegate to io/get-char.
;
; FIXME:  For v0.963, uses of io/if have been removed, but
; their old locations are still identified by FIXME comments.

;(define-syntax io/if
;  (syntax-rules ()
;   ((_ expr) #f)         ;FIXME: disabled for 0.95
;   ((_ expr) expr)
;   ))

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
              ((eq? type type:binary-input)                 ; FIXME (was io/if)
               (let ((x (io/get-u8 p #f)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'read-char "not a textual input port" p)
               #t)))
      (begin (error 'read-char "not a textual input port" p)
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
              ((eq? type type:binary-input)                 ; FIXME (was io/if)
               (let ((x (io/get-u8 p #t)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'peek-char "not a textual input port" p)
               #t)))
      (begin (error 'peek-char "not a textual input port" p)
             #t)))

; FIXME: deprecated in Larceny.  This was dropped in R6RS
; because its semantics as specified by the R5RS aren't
; really useful.  See below.
;
; FIXME: works only when an Ascii character is ready on a
; textual port, which is a restriction permitted by the R5RS.
;
; FIXME: makes no effort to fill a depleted buffer, which
; is a limitation permitted by the R5RS.

(define (io/char-ready? p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf  (vector-like-ref p port.mainbuf))
            (ptr  (vector-like-ref p port.mainptr)))
        (cond ((eq? type type:textual-input)
               (let ((unit (bytevector-ref buf ptr)))
                 (or (< unit 128)
                     (eq? (vector-like-ref p port.state)
                          'eof))))
              (else #f)))
      (error 'char-ready? "not a textual input port" p)))

; FIXME:  For v0.94 only, io/write-char can write to binary
; ports, treating them as Latin-1.
;
; FIXME:  After v0.94, io/write-char should just delegate
; to io/put-char.

(define (io/write-char c p)
  (if (port? p)
      (let ((type (vector-like-ref p port.type)))
        (cond ((eq? type type:binary-output)                ; FIXME (was io/if)
               (let ((sv (char->integer c)))
                 (if (fx< sv 256)
                     (io/put-u8 p sv)
                     (error 'write-char
                            "non-latin-1 character to binary port"
                            c p))))
              ((eq? type type:textual-output)
               (io/put-char p c))
              (else
               (error 'write-char "not a textual output port" p)
               #t)))
      (begin (error 'write-char "not a textual output port" p)
             #t)))

; FIXME:  The name is misleading, since it now requires a bytevector.
; FIXME:  Asm/Shared/makefasl.sch uses this procedure, mainly
; to write codevectors.
;
; For short bytevectors, it might be more effective to copy rather than
; flush.  This procedure is really most useful for long bytevectors, and
; was written to speed up fasl file writing.
;
; With the advent of transcoded i/o in v0.94, this procedure
; is useful only for writing fasl files, which are either
; binary or Latin-1 with no end-of-line translation.

(define (io/write-bytevector-like bvl p)
  (assert (port? p))
  (assert (bytevector? bvl))
  (assert (let ((t (vector-like-ref p port.transcoder)))
            (or (eq? t codec:binary)
                (eq? (io/transcoder-codec t) 'latin-1))))
  (let ((buf (vector-like-ref p port.mainbuf))
        (tt  (typetag bvl)))
    (io/flush-buffer p)
    (vector-like-set! p port.mainbuf bvl)
    (vector-like-set! p port.mainlim (bytevector-like-length bvl))
    (io/flush-buffer p)
    (vector-like-set! p port.mainbuf buf)
    (vector-like-set! p port.mainlim 0)
    (unspecified)))
  
; When writing the contents of an entire string,
; we could do the error checking just once, but
; this should be fast enough for now.

(define (io/write-string s p)
  (do ((n (string-length s))
       (i 0 (+ i 1)))
      ((= i n) (unspecified))
    (io/write-char (string-ref s i) p)))

(define (io/discretionary-flush p)
  (if (and (port? p) (io/output-port? p))
      (if (vector-like-ref p port.wr-flush?)
          (io/flush-buffer p))
      (begin (error "io/discretionary-flush: not an output port: " p)
             #t)))

; Flushes output-only ports, but does not flush combined input/output
; ports because they are unbuffered on the output side.

(define (io/flush p)
  (if (and (port? p) (io/output-port? p))
      (if (not (io/input-port? p))
          (io/flush-buffer p))
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

; The port alist was added to implement R6RS semantics for
; port-position and set-port-position!, and may eventually
; be used for other things also.

(define (io/port-alist p)
  (cond ((port? p)
         (vector-like-ref p port.alist))
        (else
         (error 'io/port-alist (errmsg 'msg:illegal) p)
         #t)))

(define (io/port-alist-set! p alist)
  (cond ((not (port? p))
         (error 'io/port-alist (errmsg 'msg:illegal1) p))
        ((not (and (list? alist)
                   (every? pair? alist)))
         (error 'io/port-alist (errmsg 'msg:illegal2) p))
        (else
         (vector-like-set! p port.alist alist)
         (unspecified))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters.
;
; These parameters control how the read and get-datum procedures
; parse data from a textual input port.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If #t, symbols beginning with : are self-quoting.
;;
;; FIXME:  This affects macro expansion, but not the reader.

(define recognize-keywords? (make-parameter "recognize-keywords?" #f))

(define recognize-javadot-symbols?
  (make-parameter "recognize-javadot-symbols?" #f boolean?))

(define case-sensitive? (make-parameter "case-sensitive?" #f boolean?))

; FIXME: deprecated

(define read-square-bracket-as-paren
  (make-parameter "read-square-bracket-as-paren" #t boolean?))

;; If #t, the reader keeps track of source locations.

(define datum-source-locations?
  (make-parameter "datum-source-locations?" #f boolean?))

;; Enables flags such as #!r5rs and #!larceny

(define read-r6rs-flags?
  (make-parameter "read-r6rs-flags?" #t boolean?))

;; Enables:
;; # as an insignificant digit
;; #!null #!false #!true #!unspecified #!undefined
;; embedded vertical bars within symbols
;; #^B #^C #^F #^P #^G randomness (used in FASL files)

(define read-larceny-weirdness?
  (make-parameter "read-larceny-weirdness?" #t boolean?))

;; Enables:
;; vertical bars surrounding symbols
;; backslash escaping of symbols (disables *all* case-folding in symbol)
;; non-number implies symbol (at least for some things)
;; some nonstandard peculiar identifiers (-- -1+ 1+ 1-)
;; backslashes in strings before characters that don't have to be escaped
;; unconditional downcasing of the character following #
;; nonstandard character names (?)
;; #! ... !# comments (see lib/Standard/exec-comment.sch)
;; #.(...) read-time evaluation (see lib/Standard/sharp-dot.sch)
;; #&... (see lib/Standard/box.sch)

(define read-traditional-weirdness?
  (make-parameter "read-traditional-weirdness?" #f boolean?))

;; Enables:
;; MzScheme #\uXX character notation
;; MzScheme #% randomness
;; #"..." randomness

(define read-mzscheme-weirdness?
  (make-parameter "read-mzscheme-weirdness?" #f boolean?))

;; Enables:
;; R5RS lexical syntax, including the now-deprecated weird parts

(define read-r5rs-weirdness?
  (make-parameter "read-r5rs-weirdness?" #t boolean?))

;; Enables:
;; R6RS lexical syntax

(define read-r6rs-weirdness?
  (make-parameter "read-r6rs-weirdness?" #t boolean?))

;; Enables:
;; R7RS lexical syntax

(define read-r7rs-weirdness?
  (make-parameter "read-r7rs-weirdness?" #t boolean?))

;; The following were supported in v0.93 but will not be in the
;; future:
;;
;; #r randomness (regular expressions?)
;; some kind of weirdness in flush-whitespace-until-rparen

;; This thunk is called whenever a #!fasl flag is read.
;; Its result is the value of the #!fasl flag.

(define fasl-evaluator
  (make-parameter "fast-evaluator" (lambda () (unspecified)) procedure?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reader/writer mode of a textual input or output port.
;
; A binary port always has mode 0.
; The reader/writer mode of a textual port is encoded by a fixnum
; that is the inclusive or of
;
;     nofoldcase/foldcase:
;         0 means #!no-fold-case
;         1 means #!fold-case
;     source locations:
;         0 means don't record source locations
;         2 means record source locations
;     javadot:
;         0 means don't recognize JavaDot symbols
;         4 means recognize JavaDot symbols
;     flags:
;         0 means recognize all flags
;         8 means recognize #!r6rs flag only
;     weirdness:
;         0 means to enforce minimal lexical syntax
;        16 means to allow Larceny weirdness
;        32 means to allow traditional weirdness
;        64 means to allow MzScheme weirdness
;       128 means to allow R5RS weirdness
;       256 means to allow R6RS weirdness
;       512 means to allow R7RS weirdness
;
; If the weirdness is 0, then the port uses lexical conventions
; that approximate the intersection of R6RS and R7RS syntax.
; (For output ports, R7RS slashification may be necessary even
; when the weirdness is 0.)
;
; If the weirdness is 256, then strict R6RS lexical syntax
; should be enforced (modulo case and javadot, which may be
; allowed or disallowed independently).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define readmode-mask:foldcase        1)
(define readmode-mask:locations       2)
(define readmode-mask:javadot         4)
(define readmode-mask:flags           8)
(define readmode-mask:weirdness    1008)    ; (+ 16 32 64 128 256 512)

(define readmode:binary               0)
(define readmode:nofoldcase           0)
(define readmode:foldcase             1)
(define readmode:nolocations          0)
(define readmode:locations            2)
(define readmode:nojavadot            0)
(define readmode:javadot              4)
(define readmode:noflags              0)
(define readmode:flags                8)
(define readmode:noweird              0)
(define readmode:larceny             16)
(define readmode:traditional         32)
(define readmode:mzscheme            64)
(define readmode:r5rs               128)
(define readmode:r6rs               256)
(define readmode:r7rs               512)

(define (default-read-mode)
  (define (default parameter iftrue iffalse)
    (if (parameter) iftrue iffalse))
  (+ (default case-sensitive?             readmode:nofoldcase
                                          readmode:foldcase)
     (default datum-source-locations?     readmode:locations
                                          readmode:nolocations)
     (default recognize-javadot-symbols?  readmode:javadot
                                          readmode:nojavadot)
     (default read-r6rs-flags?            readmode:flags
                                          readmode:noflags)
     (default read-larceny-weirdness?     readmode:larceny
                                          readmode:noweird)
     (default read-traditional-weirdness? readmode:traditional
                                          readmode:noweird)
     (default read-mzscheme-weirdness?    readmode:mzscheme
                                          readmode:noweird)
     (default read-r5rs-weirdness?        readmode:r5rs
                                          readmode:noweird)
     (default read-r6rs-weirdness?        readmode:r6rs
                                          readmode:noweird)
     (default read-r7rs-weirdness?        readmode:r7rs
                                          readmode:noweird)))


(define (io/complain-of-illegal-argument proc arg)
  (error proc "illegal argument" arg)
  #t)

(define (io/port-folds-case? p)
  (cond ((io/input-port? p)
         (eq? (fxlogand readmode-mask:foldcase
                        (vector-like-ref p port.readmode))
              readmode:foldcase))
        (else
         (io/complain-of-illegal-argument 'io/port-folds-case? p))))

(define (io/port-folds-case! p bool)
  (cond ((and (io/input-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (vector-like-ref p port.readmode))
                (mode (fxlogand mode (fxlognot readmode-mask:foldcase)))
                (mode (fxlogior mode
                                (if bool
                                    readmode:foldcase
                                    readmode:nofoldcase))))
           (vector-like-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          'io/port-folds-case!
          (if (boolean? bool) p bool)))))

(define (io/port-records-source-locations? p)
  (cond ((io/input-port? p)
         (eq? (fxlogand readmode-mask:locations
                        (vector-like-ref p port.readmode))
              readmode:locations))
        (else
         (io/complain-of-illegal-argument 'io/port-records-locations? p))))

(define (io/port-recognizes-javadot-symbols? p)
  (cond ((io/input-port? p)
         (eq? (fxlogand readmode-mask:javadot
                        (vector-like-ref p port.readmode))
              readmode:javadot))
        (else
         (io/complain-of-illegal-argument 'io/port-recognizes-javadot-symbols?
                                          p))))

(define (io/port-recognizes-javadot-symbols! p bool)
  (cond ((and (io/input-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (vector-like-ref p port.readmode))
                (mode (fxlogand mode (fxlognot readmode-mask:javadot)))
                (mode (fxlogior mode
                                (if bool
                                    readmode:javadot
                                    readmode:nojavadot))))
           (vector-like-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          'io/port-recognizes-javadot-symbols!
          (if (boolean? bool) p bool)))))

(define (io/port-allows-flags? p)
  (cond ((io/input-port? p)
         (eq? (fxlogand readmode-mask:flags
                        (vector-like-ref p port.readmode))
              readmode:flags))
        (else
         (io/complain-of-illegal-argument 'io/port-allows-flags? p))))

(define (allows-weirdness-getter p themode name)
  (cond ((and (io/open-port? p) (io/textual-port? p))
         (eq? (fxlogand themode
                        (vector-like-ref p port.readmode))
              themode))
        (else
         (io/complain-of-illegal-argument name p))))

(define (allows-weirdness-setter p bool themode name)
  (cond ((and (io/open-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (vector-like-ref p port.readmode))
                (mode (fxlogand mode (fxlognot themode)))
                (mode (fxlogior mode
                                (if bool
                                    themode
                                    readmode:noweird))))
           (vector-like-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          name
          (if (boolean? bool) p bool)))))

(define (io/port-allows-larceny-weirdness? p)
  (allows-weirdness-getter p
                           readmode:larceny
                           'io/port-allows-larceny-weirdness?))

(define (io/port-allows-larceny-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:larceny
                           'io/port-allows-larceny-weirdness!))

(define (io/port-allows-traditional-weirdness? p)
  (allows-weirdness-getter p
                           readmode:traditional
                           'io/port-allows-traditional-weirdness?))

(define (io/port-allows-traditional-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:traditional
                           'io/port-allows-traditional-weirdness!))

(define (io/port-allows-mzscheme-weirdness? p)
  (allows-weirdness-getter p
                           readmode:mzscheme
                           'io/port-allows-mzscheme-weirdness?))

(define (io/port-allows-mzscheme-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:mzscheme
                           'io/port-allows-mzscheme-weirdness!))

(define (io/port-allows-r5rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r5rs
                           'io/port-allows-r5rs-weirdness?))

(define (io/port-allows-r5rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r5rs
                           'io/port-allows-r5rs-weirdness!))

(define (io/port-allows-r6rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r6rs
                           'io/port-allows-r6rs-weirdness?))

(define (io/port-allows-r6rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r6rs
                           'io/port-allows-r6rs-weirdness!))

(define (io/port-allows-r7rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r7rs
                           'io/port-allows-r7rs-weirdness?))

(define (io/port-allows-r7rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r7rs
                           'io/port-allows-r7rs-weirdness!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS i/o
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; R6RS port positions are a mess, mainly because they mimic
; a Posix feature that works only for file descriptors and
; whose semantics is specified in terms of bytes, not characters.
; As specified in Scheme, port positions also interact (badly)
; with other misfeatures, including input/output ports (whose
; semantics were modelled on Posix features that assume one
; byte per character) and custom ports (whose semantics mandates
; at least one character of lookahead buffering, but does not
; include any provision for buffer corrections when calculating
; port positions).
;
; In Larceny, all ports support the port-position operation.
; Binary ports report the position in bytes, and textual ports
; report the position in characters.
;
; For set-port-position!, the situation is more complex:
;
; some ports do not support set-port-position!
;     e.g. pipes and sockets
; binary file ports support set-port-position!
; custom ports may support set-port-position!
;     If so, then Larceny assumes their positions are reported
;     in bytes (for binary ports) or characters (for textual
;     ports) and relies on those assumptions to implement
;     buffer corrections.
; custom input/output (combined) ports must support set-port-position!
;     Larceny relies on set-port-position! to implement any buffer
;     correction required when switching from input to output.
; textual input file ports support set-port-position!
;     With Latin-1 or UTF-8 transcoding, the implementation
;     uses a cache and a port-position-in-bytes procedure.
;     If the position has not been cached, then a warning
;     is issued and the implementation proceeds as for UTF-16
;     transcoding.
;     With UTF-16 transcoding, set-port-position! is implemented
;     by resetting to the beginning of the file and reading the
;     specified number of characters.
; textual input/output file ports support set-port-position!
;     Implementation is the same as for input-only file ports.
; textual output-only files ports support set-port-position!
;     Buffer corrections are implemented using caching and
;     a port-position-in-bytes procedure that is part of the
;     port structure.
; bytevector and string ports support set-port-position!
;     They mimic file ports.
;
; For textual ports, if the argument to set-port-position! is
; nonzero and is also not the result of a previous call to
; port-position on the port, then an exception should be raised.
;
; A position cache that maps character positions to byte positions
; is maintained for textual ports that support set-port-position!.
; If the port uses variable-length transcoding (with a codec other
; than Latin-1, or an eol-style other than none), then the port
; must supply one of the following procedures in the port's alist:
;
;     port-position-in-bytes
;     port-position-in-chars (used by custom ports)

(define (io/port-position p)
  (if (io/binary-port? p)
      (io/port-position-nocache p)
      (let ((posn (io/port-position-nocache p)))

        (if (and (> posn 0)
                 (vector-like-ref p port.setposn))

            ; Cache the position for future use by set-port-position!

            (let* ((posn (io/port-position-nocache p))
                   (t (io/port-transcoder p))
                   (alist (io/port-alist p))
                   (probe1 (assq 'port-position alist))
                   (port-position-in-chars (if probe1 (cdr probe1) #f))
                   (probe2 (assq 'port-position-in-bytes alist))
                   (port-position-in-bytes (if probe2 (cdr probe2) #f))
                   (probe3 (assq 'cached-positions alist))
                   (ht (if probe3
                           (cdr probe3)
                           (let* ((ht (make-hashtable abs =))
                                  (entry (cons 'cached-positions ht)))
                             (io/port-alist-set! p (cons entry alist))
                             ht))))

              (cond ((and (eq? 'latin-1 (io/transcoder-codec t))
                          (eq? 'none (io/transcoder-eol-style t)))
                     (hashtable-set! ht posn posn))
                    (port-position-in-chars
                     (hashtable-set! ht posn posn))
                    (port-position-in-bytes
                     (let* ((byte-posn (port-position-in-bytes))
                            (byte-posn
                             (if (io/input-port? p)
                                 (- byte-posn
                                    (- (vector-like-ref p port.mainlim)
                                       (vector-like-ref p port.mainptr))
                                    (- (vector-like-ref p port.auxlim)
                                       (vector-like-ref p port.auxptr)))
                                 byte-posn)))
                       (hashtable-set! ht posn byte-posn)))
                    (else
                     (assertion-violation
                      'port-position
                      "internal error: no support for set-port-position!")))))

        posn)))

; Like io/port-position, but faster and more space-efficient
; because it doesn't cache.  The call to io/flush is necessary
; for correct caching when called by io/port-position above.

(define (io/port-position-nocache p)
  (cond ((io/input-port? p)
         (+ (vector-like-ref p port.mainpos)
            (vector-like-ref p port.mainptr)))
        ((io/output-port? p)
         (io/flush p)
         (+ (vector-like-ref p port.mainpos)
            (vector-like-ref p port.mainlim)))
        (else
         (error "io/port-position: " p " is not an open port.")
         #t)))

(define (io/port-lines-read p)
  (cond ((io/input-port? p)
         (vector-like-ref p port.linesread))
        (else
         (error 'io/port-lines-read "not a textual input port" p)
         #t)))

(define (io/port-line-start p)
  (cond ((io/input-port? p)
         (vector-like-ref p port.linestart))
        (else
         (error 'io/port-line-start "not a textual input port" p)
         #t)))

(define (io/port-has-set-port-position!? p)
  (cond ((port? p)
         (vector-like-ref p port.setposn))
        (else
         (error 'io/has-set-port-position!? "illegal argument" p)
         #t)))

; FIXME: for textual output ports with variable-length encoding,
; any output operation should invalidate all cached positions
; that lie beyond the position of the output operation.

(define (io/set-port-position! p posn)
  (if (io/output-port? p)
      (io/flush p))
  (cond ((not (and (port? p)
                   (vector-like-ref p port.setposn)))
         (error 'io/set-port-position! (errmsg 'msg:illegalarg1) p posn))
        ((eq? (vector-like-ref p port.state) 'closed)
         (unspecified))
        ((not (and (exact? posn) (integer? posn)))
         (error 'io/set-port-position! "illegal argument" posn))
        ((or (= posn 0)
             (io/binary-port? p))
         (io/reset-buffers! p)
         (vector-like-set! p port.mainpos posn)
         (io/set-port-position-as-binary! p posn))
        (else

         ; Lookup the corresponding byte position.

         (let* ((t (io/port-transcoder p))
                (codec (io/transcoder-codec t))
                (input? (io/input-port? p))
                (output? (io/output-port? p))
                (alist (io/port-alist p))
                (probe1 (assq 'port-position alist))
                (port-position-in-chars (if probe1 (cdr probe1) #f))
                (probe2 (assq 'port-position-in-bytes alist))
                (port-position-in-bytes (if probe2 (cdr probe2) #f))
                (probe3 (assq 'cached-positions alist))
                (ht (if probe3 (cdr probe3) #f))
                (byte-posn (and ht (hashtable-ref ht posn #f))))

           (define (reposition!)
             (io/reset-buffers! p)
             (vector-like-set! p port.mainpos posn)
             (io/set-port-position-as-binary! p byte-posn))

           ; We can't enforce the R6RS restriction for combined
           ; input/output ports because it may be a lookahead correction.

           (cond ((or byte-posn
                      (and input? output?))

                  (if (and (not input?)
                           output?
                           (not (eq? codec 'latin-1))
                           (not port-position-in-chars))
                      (issue-warning-deprecated
                       'set-port-position!...on_Unicode_output_port))

                  (reposition!))

                 (else

                  ; error case: posn > 0 and not in cache

                  (if (not (issue-deprecated-warnings?))

                      (assertion-violation 'set-port-position!
                                           (errmsg 'msg:uncachedposition)
                                           p posn)

                      ; FIXME: ad hoc warning message

                      (let ((out (current-error-port)))
                        (display "Warning from set-port-position!: " out)
                        (newline out)
                        (display (errmsg 'msg:uncachedposition) out)
                        (display ": " out)
                        (write posn out)
                        (newline out)

                        ; Attempt the operation anyway.  Hey, it might work.

                        (cond ((or port-position-in-chars
                                   (and
                                    (eq? 'latin-1 codec)
                                    (eq? 'none (io/transcoder-eol-style t))))
                               (reposition!))
                              ((io/input-port? p)
                               (io/set-port-position! p 0)
                               (do ((posn posn (- posn 1)))
                                   ((= posn 0))
                                 (read-char p)))
                              (else
                               (reposition!))))))))))

  (unspecified))

(define (io/set-port-position-as-binary! p posn)
  (let ((r (((vector-like-ref p port.ioproc) 'set-position!)
            (vector-like-ref p port.iodata)
            posn)))
    (cond ((eq? r 'ok)
           (if (eq? (vector-like-ref p port.state) 'eof)
               (vector-like-set!
                p
                port.state
                (if (binary-port? p) 'binary 'textual)))
           (unspecified))
          (else
           (error 'set-port-position! "io error" p posn)))))

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
;     00 means ignore
;     01 means replace
;     10 means raise
;
; FIXME:  The external world should see a more self-explanatory
; representation of transcoders, and the three accessors for
; transcoders should return the original symbols instead of
; their canonical equivalents.

(define transcoder-mask:codec    #b11100000)
(define transcoder-mask:eolstyle #b00011100)
(define transcoder-mask:errmode  #b00000011)

(define codec:binary  0)
(define codec:latin-1 #b00100000)
(define codec:utf-8   #b01000000)
(define codec:utf-16  #b01100000)

(define eolstyle:none  #b00000)
(define eolstyle:lf    #b00100)
(define eolstyle:nel   #b01000)
(define eolstyle:ls    #b01100)
(define eolstyle:cr    #b10000)
(define eolstyle:crlf  #b10100)
(define eolstyle:crnel #b11000)

(define errmode:ignore  0)
(define errmode:replace 1)
(define errmode:raise   2)

; May be redefined at startup as specified by system-features.
; (See also command-line processing).

(define default-transcoder
  (make-parameter "default-transcoder"
                  codec:latin-1
                  (lambda (t)
                    (and (fixnum? t)
                         (<= codec:latin-1 t transcoder-mask:codec)))))

; In Larceny, *every* symbol names an end-of-line style,
; and *every* symbol names an error handling mode.

(define (io/make-transcoder codec eol-style handling-mode)
  (define (local-error msg irritant)
    (if (issue-deprecated-warnings?)
        (let ((out (current-error-port)))
          (display "Warning: " out)
          (display msg out)
          (display ": " out)
          (write irritant out)
          (newline out)
          (display "Using Larceny-specific interpretation." out)
          (newline out))))
  (let ((bits:codec (case codec
                     ((latin-1) codec:latin-1)
                     ((utf-8)   codec:utf-8)
                     ((utf-16)  codec:utf-16)
                     (else (local-error "nonstandard codec" codec)
                           codec:latin-1)))
        (bits:eol (case eol-style
                   ((none)  eolstyle:none)
                   ((lf)    eolstyle:lf)
                   ((nel)   eolstyle:nel)
                   ((ls)    eolstyle:ls)
                   ((cr)    eolstyle:cr)
                   ((crlf)  eolstyle:crlf)
                   ((crnel) eolstyle:crnel)
                   (else (local-error "nonstandard eol style" eol-style)
                         eolstyle:none)))
        (bits:ehm (case handling-mode
                   ((ignore)  errmode:ignore)
                   ((replace) errmode:replace)
                   ((raise)   errmode:raise)
                   (else (local-error "nonstandard error handling mode"
                                      handling-mode)
                         errmode:replace))))
    (+ bits:codec bits:eol bits:ehm)))

; Programmers should never see a transcoder with binary codec.

(define (io/transcoder-codec t)
  (let ((codec (fxlogand t transcoder-mask:codec)))

    (cond ((fx= codec codec:binary)  'binary)
          ((fx= codec codec:latin-1) 'latin-1)
          ((fx= codec codec:utf-8)   'utf-8)
          ((fx= codec codec:utf-16)  'utf-16)
          (else
           (assertion-violation 'transcoder-codec
                                "weird transcoder" t)))))

(define (io/transcoder-eol-style t)
  (let ((style (fxlogand t transcoder-mask:eolstyle)))
    (cond ((fx= style eolstyle:none)  'none)
          ((fx= style eolstyle:lf)    'lf)
          ((fx= style eolstyle:nel)   'nel)
          ((fx= style eolstyle:ls)    'ls)
          ((fx= style eolstyle:cr)    'cr)
          ((fx= style eolstyle:crlf)  'crlf)
          ((fx= style eolstyle:crnel) 'crnel)
          (else
           (assertion-violation 'transcoder-eol-style
                                "weird transcoder" t)))))

(define (io/transcoder-error-handling-mode t)
  (let ((mode (fxlogand t transcoder-mask:errmode)))
    (cond ((fx= mode errmode:ignore)  'ignore)
          ((fx= mode errmode:replace) 'replace)
          ((fx= mode errmode:raise)   'raise)
          (else
           (assertion-violation 'transcoder-error-handling-mode
                                "weird transcoder" t)))))

; Like transcoded-port, but performs less error checking.

(define (io/transcoded-port p t)
  (assert (io/open-port? p))
  (assert (eq? 'binary (vector-like-ref p port.state)))
  (if (io/output-port? p)
      (io/flush p))

  (if (not (memq (transcoder-codec t) '(latin-1 utf-8)))
      (io/transcoded-port-random p t)

      ; shallow copy

      (let ((newport (io/clone-port p)))
        (vector-like-set! newport
                          port.type
                          (fxlogior type:textual
                                    (vector-like-ref p port.type)))
        (vector-like-set! newport port.transcoder t)
        (vector-like-set! newport port.state 'textual)
        (vector-like-set! newport port.readmode (default-read-mode))

        ; io/transcode-port! expects a newly filled mainbuf,
        ; so we have to fake it here.
        ;
        ; FIXME: Is the above really true?
    
        (let* ((mainbuf1 (vector-like-ref p port.mainbuf))
               (mainptr1 (vector-like-ref p port.mainptr))
               (mainlim1 (vector-like-ref p port.mainlim))
               (mainbuf2 (make-bytevector (bytevector-length mainbuf1)))
               (mainlim2 (fx- mainlim1 mainptr1)))

          ; FIXME:  Unclear what port-position should do.

          (vector-like-set! newport port.mainpos 0)

          (bytevector-copy! mainbuf1
                            mainptr1
                            mainbuf2 0 mainlim2)
          (vector-like-set! newport port.mainbuf mainbuf2)
          (vector-like-set! newport port.mainptr 0)
          (vector-like-set! newport port.mainlim mainlim2))

        ; close original port, destroying original mainbuf

        (io/set-closed-state! p)

        (cond ((and (io/input-port? newport)
                    (io/output-port? newport))
               (assert #t))
              ((io/input-port? newport)
               (io/transcode-port! newport)))
        newport)))

; Latin-1 and UTF-8 are transcoded on the fly, but other
; codecs are transcoded by interposing an extra binary port.

(define (io/transcoded-port-random p t)

  ; FIXME: for now, we support only Latin-1, UTF-8, and UTF-16.

  (assert (eq? (transcoder-codec t) 'utf-16))

  ; shallow copy

  (let* ((newport (io/clone-port p))
         (mainbuf1 (vector-like-ref p port.mainbuf))
         (mainptr1 (vector-like-ref p port.mainptr))
         (mainlim1 (vector-like-ref p port.mainlim))
         (mainbuf2 (make-bytevector (bytevector-length mainbuf1))))

      (bytevector-copy! mainbuf1
                        mainptr1
                        mainbuf2 0 (fx- mainlim1 mainptr1))
      (vector-like-set! newport port.mainbuf mainbuf2)

      ; close original port, destroying original mainbuf

      (io/set-closed-state! p)

      (let* ((p (utf16/transcoded-binary-port newport))
             (t (make-transcoder (utf-8-codec)
                                 (transcoder-eol-style t)
                                 (transcoder-error-handling-mode t))))

        ; FIXME: this will look like it's transcoded as UTF-8

        (io/transcoded-port p t))))

; Like transcoded-port but preserves slightly different state
; and uses the correct transcoder for custom textual ports.

(define (io/custom-transcoded-port p)
  (assert (port? p))
  (let* ((t (make-transcoder (utf-8-codec) 'none 'ignore))
         (newport (io/transcoded-port p t)))
    newport))

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
    (cond ((not (eq? type type:binary-input))
           (if (eq? type type:binary-input/output)
               (io/get-u8-input/output p lookahead?)
               (assertion-violation 'get-u8
                                    "not an open binary input port" p)))
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

; The general case for get-char and lookahead-char.
; The transcoder will be for Latin-1 or UTF-8.

(define (io/get-char p lookahead?)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (cond ((not (eq? type type:textual-input))
           (if (eq? type type:textual-input/output)
               (io/get-char-input/output p lookahead?)
               (assertion-violation 'get-char
                                    "argument not a textual input port" p)))
          ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (cond ((fx> unit 13)
                           ; not #\linefeed, #\return, #\nel, or #\x2028
                           (if (not lookahead?)
                               (vector-like-set! p port.mainptr (+ ptr 1)))
                           (integer->char unit))
                          ((or (fx= unit 10)                       ; #\linefeed
                               (fx= unit 13))                        ; #\return
                           (if (not lookahead?)
                               (vector-like-set! p port.mainptr (+ ptr 1)))
                           (io/return-eol p lookahead? unit))
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
                                 (if (fx= unit #x85)
                                     (io/return-eol p lookahead? unit)
                                     (integer->char unit)))
                                ((fx= codec codec:utf-8)
                                 (io/get-char-utf-8
                                  p lookahead? unit buf ptr lim))
                                (else
                                 (error 'io/get-char
                                        "unimplemented codec" codec p)
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
             (vector-like-set! p
                               port.mainpos
                               (+ (vector-like-ref p port.mainpos) ptr))
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

(define (io/put-u8 p byte)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf (vector-like-ref p port.mainbuf))
        (lim (vector-like-ref p port.mainlim)))
    (cond ((eq? type type:binary-output)
           (cond ((< lim (bytevector-like-length buf))
                  (bytevector-like-set! buf lim byte)
                  (vector-like-set! p port.mainlim (+ lim 1))
                  (unspecified))
                 (else
                  (io/flush-buffer p)
                  (io/put-u8 p byte))))
          ((eq? type type:binary-input/output)
           (io/put-u8-input/output p byte))
          (else
           (error 'put-u8 "not a binary output port" p)
           #t))))

(define (io/put-char p c)
  (if (port? p)
      (let ((type (vector-like-ref p port.type))
            (buf (vector-like-ref p port.mainbuf))
            (lim (vector-like-ref p port.mainlim)))
        (cond ((eq? type type:textual-output)
               (let ((sv (char->integer c))
                     (n  (bytevector-length buf)))
                 (cond ((fx>= lim n)
                        (io/flush-buffer p)
                        (io/put-char p c))
                       ((fx= sv 10)
                        (io/put-eol p))
                       ((fx<= sv #x7f)
                        (bytevector-set! buf lim sv)
                        (vector-like-set! p port.mainlim (+ lim 1))
                        (unspecified))
                       ((and (fx<= sv #xff)
                             (fx= codec:latin-1
                                  (fxlogand
                                   transcoder-mask:codec
                                   (vector-like-ref p port.transcoder))))
                        (bytevector-set! buf lim sv)
                        (vector-like-set! p port.mainlim (+ lim 1))
                        (unspecified))
                       ((not (fx= codec:utf-8
                                  (fxlogand
                                   transcoder-mask:codec
                                   (vector-like-ref p port.transcoder))))
                        (let* ((t (vector-like-ref p port.transcoder))
                               (mode (fxlogand transcoder-mask:errmode t)))
                          (cond ((fx= mode errmode:ignore)
                                 (unspecified))
                                ((fx= mode errmode:replace)
                                 (io/put-char p #\?))
                                ((fx= mode errmode:raise)
                                 (raise-r6rs-exception
                                  (make-i/o-encoding-error p c)
                                  'put-char "encoding error" (list p c)))
                                (else
                                 (assertion-violation 'put-char
                                                      "internal error" p c)))))
                       ((fx>= lim (- n 4))
                        (io/flush-buffer p)
                        (io/put-char p c))
                       ((<= sv #x07ff)
                        (let ((u0 (fxlogior #b11000000
                                            (fxrshl sv 6)))
                              (u1 (fxlogior #b10000000
                                            (fxlogand sv #b00111111)))
                              (pos (vector-like-ref p port.mainpos)))
                          (bytevector-set! buf lim u0)
                          (bytevector-set! buf (+ lim 1) u1)
                          (vector-like-set! p port.mainpos (- pos 1))
                          (vector-like-set! p port.mainlim (+ lim 2))))
                       ((<= sv #xffff)
                        (let ((u0 (fxlogior #b11100000
                                            (fxrshl sv 12)))
                              (u1 (fxlogior #b10000000
                                            (fxlogand (fxrshl sv 6)
                                                      #b00111111)))
                              (u2 (fxlogior #b10000000
                                            (fxlogand sv #b00111111)))
                              (pos (vector-like-ref p port.mainpos)))
                          (bytevector-set! buf lim u0)
                          (bytevector-set! buf (+ lim 1) u1)
                          (bytevector-set! buf (+ lim 2) u2)
                          (vector-like-set! p port.mainpos (- pos 2))
                          (vector-like-set! p port.mainlim (+ lim 3))))
                       (else
                        (let ((u0 (fxlogior #b11110000
                                            (fxrshl sv 18)))
                              (u1 (fxlogior #b10000000
                                            (fxlogand (fxrshl sv 12)
                                                      #b00111111)))
                              (u2 (fxlogior #b10000000
                                            (fxlogand (fxrshl sv 6)
                                                      #b00111111)))
                              (u3 (fxlogior #b10000000
                                            (fxlogand sv #b00111111)))
                              (pos (vector-like-ref p port.mainpos)))
                          (bytevector-set! buf lim u0)
                          (bytevector-set! buf (+ lim 1) u1)
                          (bytevector-set! buf (+ lim 2) u2)
                          (bytevector-set! buf (+ lim 3) u3)
                          (vector-like-set! p port.mainpos (- pos 3))
                          (vector-like-set! p port.mainlim (+ lim 4)))))))
              ((eq? type type:textual-input/output)
               (io/put-char-input/output p c))
              (else
               (error 'put-char "not an output port" p)
               #t)))
      (begin (error "put-char: not an output port: " p)
             #t)))

; Operations on input/output ports, which are peculiar.
; Currently, the only input/output ports are
;
;     bytevector input/output ports
;     custom input/output ports
;     transcoded input/output ports
;
; These operations are invoked only when p is known to be an
; input/output port of the correct type (binary/textual).

(define (io/get-u8-input/output p lookahead?)
  (let* ((state   (vector-like-ref p port.state))
         (mainbuf (vector-like-ref p port.mainbuf))
         (mainlim (vector-like-ref p port.mainlim))
         (iodata  (vector-like-ref p port.iodata))
         (ioproc  (vector-like-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
          ((eq? state 'error)
           (error 'get-u8 "permanent read error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'get-u8 "read attempted on closed port " p))
          ((> mainlim 0)
           (let ((r (bytevector-ref mainbuf 0)))
             (if (not lookahead?)
                 (let ((mainpos (vector-like-ref p port.mainpos)))
                   (vector-like-set! p port.mainpos (+ mainpos 1))
                   (vector-like-set! p port.mainlim 0)))
             r))
          (else
           (let ((n ((ioproc 'read) iodata mainbuf)))
             (cond ((eq? n 1)
                    (let ((r (bytevector-ref mainbuf 0)))
                      (if (not lookahead?)
                          (let ((mainpos (vector-like-ref p port.mainpos)))
                            (vector-like-set! p port.mainpos (+ mainpos 1))
                            (vector-like-set! p port.mainlim 0))
                          (vector-like-set! p port.mainlim 1))
                      r))
                   ((or (eq? n 0) (eq? n 'eof))
                    (io/set-eof-state! p)
                    (eof-object))
                   (else
                    (io/set-error-state! p)
                    (io/get-u8-input/output p lookahead?))))))))

(define (io/get-char-input/output p lookahead?)
  (let* ((state   (vector-like-ref p port.state))
         (mainbuf (vector-like-ref p port.mainbuf))
         (mainlim (vector-like-ref p port.mainlim))
         (iodata  (vector-like-ref p port.iodata))
         (ioproc  (vector-like-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
          ((eq? state 'error)
           (error 'get-char "Read error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'get-char "Read attempted on closed port " p))
          ((> mainlim 0)
           (let* ((bv (make-bytevector mainlim))
                  (s  (begin (bytevector-copy! mainbuf 0 bv 0 mainlim)
                             (utf8->string bv))))
             (if (not lookahead?)
                 (let ((mainpos (vector-like-ref p port.mainpos)))
                   (vector-like-set! p port.mainpos (+ mainpos 1))
                   (vector-like-set! p port.mainlim 0)))
             (string-ref s 0)))
          (else
           (let ((n ((ioproc 'read) iodata mainbuf)))
             (cond ((and (fixnum? n) (> n 0))
                    (let* ((bv (make-bytevector n))
                           (s  (begin (bytevector-copy! mainbuf 0 bv 0 n)
                                      (utf8->string bv))))
                      (if (not lookahead?)
                          (let ((mainpos (vector-like-ref p port.mainpos)))
                            (vector-like-set! p port.mainpos (+ mainpos 1))
                            (vector-like-set! p port.mainlim 0))
                          (vector-like-set! p port.mainlim n))
                      (string-ref s 0)))
                   ((or (eq? n 0) (eq? n 'eof))
                    (io/set-eof-state! p)
                    (eof-object))
                   (else
                    (io/set-error-state! p)
                    (io/get-char-input/output p lookahead?))))))))

(define (io/put-u8-input/output p byte)
  (let* ((state   (vector-like-ref p port.state))
         (mainbuf (vector-like-ref p port.mainbuf))
         (mainpos (vector-like-ref p port.mainpos))
         (mainlim (vector-like-ref p port.mainlim))
         (iodata  (vector-like-ref p port.iodata))
         (ioproc  (vector-like-ref p port.ioproc))
         (buf     mainbuf))
    (cond ((eq? state 'error)
           (error 'put-u8 "permanent write error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'put-u8 "write attempted on closed port " p))
          ((> mainlim 0)
           (if (vector-like-ref p port.setposn)
               (begin (io/set-port-position! p mainpos)
                      (io/put-u8-input/output p byte))
               (begin (io/set-error-state! p)
                      (error 'put-u8
                             "input/output port without set-port-position!"
                             p))))
          (else
           (bytevector-set! buf 0 byte)
           (let ((r ((ioproc 'write) iodata buf 1)))
             (cond ((eq? r 'ok)
                    (vector-like-set! p port.mainpos (+ mainpos 1))
                    (unspecified))
                   (else
                    (io/set-error-state! p)
                    (io/put-u8-input/output p byte))))))))

(define (io/put-char-input/output p c)
  (let* ((state   (vector-like-ref p port.state))
         (mainpos (vector-like-ref p port.mainpos))
         (mainlim (vector-like-ref p port.mainlim))
         (iodata  (vector-like-ref p port.iodata))
         (ioproc  (vector-like-ref p port.ioproc))
         (buf     (string->utf8 (string c))))
    (cond ((eq? state 'error)
           (error 'put-char "permanent write error on port " p)
           (eof-object))
          ((eq? state 'closed)
           (error 'put-char "write attempted on closed port " p))
          ((> mainlim 0)

           ; Must correct for buffered lookahead character.

           (if (vector-like-ref p port.setposn)
               (begin (io/set-port-position! p mainpos)
                      (io/put-char-input/output p c))
               (begin (io/set-error-state! p)
                      (error 'put-char
                             "input/output port without set-port-position!"
                             p))))
          (else
           (let* ((n0 (bytevector-length buf))
                  (r ((ioproc 'write) iodata buf n0)))
             (cond ((eq? r 'ok)
                    (vector-like-set! p port.mainpos (+ mainpos 1))
                    (unspecified))
                   (else
                    (io/set-error-state! p)
                    (io/put-char-input/output p c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bulk i/o.
;;;
;;; Most of these handle a common case by returning the same
;;; value as a corresponding R6RS library procedure, but may
;;; fail on complex or unusual cases by returning #f.
;;;
;;; These should be majorly bummed, else there's no point.
;;;
;;; FIXME: could add a few more, such as
;;;     get-bytevector-n!
;;;     get-string-n!
;;;     put-bytevector
;;;
;;; Note, however, that io/put-string-maybe didn't help as much
;;; as io/get-line-maybe, and probably wasn't worth the effort
;;; and code size.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Handles the common case in which the line is all-Ascii,
; terminated by a linefeed, and lies entirely within the buffer.

(define (io/get-line-maybe p)
  (and (port? p)
       (let ((type (.vector-ref:trusted p port.type))
             (buf  (.vector-ref:trusted p port.mainbuf))
             (ptr  (.vector-ref:trusted p port.mainptr)))
         (define (loop i)
           (let ((unit (bytevector-ref buf i)))     ; FIXME: should be trusted
             (cond ((and (.<:fix:fix 13 unit)       ; 13 = #\return
                         (.<:fix:fix unit 128))
                    (loop (.+:idx:idx i 1)))
                   ((.=:fix:fix 10 unit)            ; 10 = #\linefeed
                    (let* ((n (.-:idx:idx i ptr))
                           (s (make-string n)))
                      (loop2 ptr i s 0)))
                   (else #f))))
         (define (loop2 j k s i)
           (cond ((.<:fix:fix j k)
                  (.string-set!:trusted s i (.integer->char:trusted
                                             (bytevector-ref buf j)))
                  (loop2 (.+:idx:idx j 1) k s (.+:idx:idx i 1)))
                 (else
                  (.vector-set!:trusted:nwb p port.mainptr (.+:idx:idx k 1))
                  s)))
         (and (eq? type type:textual-input)
              (not (vector-like-ref p port.wasreturn))
              (loop ptr)))))

; Handles the common case in which the string is all-Ascii
; and can be buffered without flushing.

(define (io/put-string-maybe p s start count)
  (and (port? p)
       (string? s)
       (fixnum? start)
       (fixnum? count)
       (.<=:fix:fix 0 start)
       (let ((k (.+:fix:fix start count))
             (n (.string-length:str s))
             (type (.vector-ref:trusted p port.type))
             (buf  (.vector-ref:trusted p port.mainbuf))
             (lim  (.vector-ref:trusted p port.mainlim)))
         (define (loop i j)
           (cond ((.<:fix:fix i k)
                  (let* ((c (.string-ref:trusted s i))
                         (sv (.char->integer:chr c)))
                    (if (and (.<:fix:fix 10 sv)    ; 10 = #\newline
                             (.<:fix:fix sv 128))
                        (begin (bytevector-set! buf j sv) ; FIXME
                               (loop (.+:idx:idx i 1) (.+:idx:idx j 1)))
                        #f)))
                 (else
                  (.vector-set!:trusted:nwb p port.mainlim j)
                  #t)))
         (and (.<:fix:fix start n)
              (<= k n)
              (eq? type type:textual-output)
              (.<=:fix:fix (.+:idx:idx lim count) (bytevector-length buf))
              (loop start lim)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private procedures (called only from code within this file)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Works only on input ports.
; The main invariants may not hold here.
; In particular, the port may be in the textual state
; but have a nonempty auxbuf.

(define (io/fill-buffer! p)
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
           (vector-like-set! p port.mainpos
                             (+ (vector-like-ref p port.mainpos)
                                (vector-like-ref p port.mainptr)))
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
  (let ((wr-ptr (vector-like-ref p port.mainlim)))
    (if (> wr-ptr 0)
        (let ((r (((vector-like-ref p port.ioproc) 'write)
                  (vector-like-ref p port.iodata)
                  (vector-like-ref p port.mainbuf)
                  wr-ptr)))
          (cond ((eq? r 'ok)
                 (vector-like-set! p port.mainpos
                                   (+ (vector-like-ref p port.mainpos) wr-ptr))
                 (vector-like-set! p port.mainlim 0))
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
  (vector-like-set! p
                    port.mainpos
                    (+ (vector-like-ref p port.mainpos)
                       (vector-like-ref p port.mainptr)))
  (vector-like-set! p port.mainptr 0)
  (vector-like-set! p port.mainlim 0)
  (vector-like-set! p port.auxptr 0)
  (vector-like-set! p port.auxlim 0)
  (bytevector-set! (vector-like-ref p port.mainbuf) 0 port.sentinel)
  (case (vector-like-ref p port.state)
   ((auxstart auxend)
    (vector-like-set! p port.state 'textual))))

; Shallow-clones a port without closing it.

(define (io/clone-port p)
  (let* ((n (vector-like-length p))
         (newport (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n)
         (typetag-set! newport sys$tag.port-typetag)
         newport)
      (vector-set! newport i (vector-like-ref p i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End-of-line processing.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Whenever io/get-char is about to return one of the four
; end-of-line characters (#\linefeed, #\return, #\x85, #x2028),
; it should perform a tail call to this procedure instead, with
; the scalar value of the specific end-of-line character as the
; last argument.
;
; That call should occur *after* the port's state has been
; updated to consume the character (unless lookahead?
; is true), but before the linesread, linestart, and wasreturn
; fields have been updated.  Updating those fields is the
; responsibility of these procedures.

(define (io/return-eol p lookahead? sv)
  (case sv
   ((13)
    (io/return-cr p lookahead?))
   ((10 #x85 #x2028)
    (if (vector-like-ref p port.wasreturn)
        (begin (vector-like-set! p port.wasreturn #f)
               (io/return-char-following-cr p lookahead? sv))
        (let* ((pos (vector-like-ref p port.mainpos))
               (ptr (vector-like-ref p port.mainptr))
               (line (vector-like-ref p port.linesread))
               (transcoder (vector-like-ref p port.transcoder))
               (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))
          (cond ((or (fx= sv 10)
                     (not (fx= eolstyle eolstyle:none)))
                 (if (not lookahead?)
                     (begin (vector-like-set! p port.linesread (+ line 1))
                            (vector-like-set! p port.linestart (+ pos ptr))))
                 #\newline)
                (else
                 (integer->char sv))))))
   (else
    (assertion-violation 'io/return-eol "internal error" p lookahead? sv))))

; Unless the eolstyle is none, a #\linefeed or #\x85 following
; a #\return should be ignored.
;
; When a #\return is consumed, the port.wasreturn field is set
; true and the port.linestart field is set to the character
; position following the #\return.

(define (io/return-cr p lookahead?)
  (let* ((transcoder (vector-like-ref p port.transcoder))
         (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))
    (cond (lookahead?
           (if (fx= eolstyle eolstyle:none)
               #\return
               #\linefeed))
          ((fx= eolstyle eolstyle:none)
           #\return)
          (else
           (let* ((mainptr (vector-like-ref p port.mainptr))
                  (mainpos (vector-like-ref p port.mainpos))
                  (pos (+ mainpos mainptr))
                  (linesread (vector-like-ref p port.linesread)))
             (vector-like-set! p port.linesread (+ linesread 1))
             (vector-like-set! p port.linestart pos)
             (vector-like-set! p port.wasreturn #t))
           #\linefeed))))

(define (io/return-char-following-cr p lookahead? sv)
  (assert (not (vector-like-ref p port.wasreturn)))
  (let* ((c (integer->char sv))
         (pos (+ (vector-like-ref p port.mainpos)
                 (vector-like-ref p port.mainptr)))
         (linesread (vector-like-ref p port.linesread))
         (linestart (vector-like-ref p port.linestart))
         (transcoder (vector-like-ref p port.transcoder))
         (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))
    (cond ((or (fx= sv 10)
               (fx= sv #x85))
           (if (and (= pos (if lookahead? linestart (+ linestart 1)))
                    (not (eq? eolstyle eolstyle:none)))
               ; consume the character while preserving position
               ; and retry the get-char operation
               (begin (if lookahead? (io/get-char p #f))
                      (let ((mainpos (vector-like-ref p port.mainpos)))
                        (vector-like-set! p port.mainpos (- mainpos 1)))
                      (vector-like-set! p port.linesread linesread)
                      (vector-like-set! p port.linestart linestart)
                      (io/get-char p lookahead?))
               ; treat the character as a normal linefeed
               (io/return-eol p lookahead? 10)))
          ((char=? c #\return)
           (io/return-cr p lookahead?))
          ((fx= sv #x2028)
           (io/return-eol p lookahead? sv))
          (else
           c))))

; Whenever io/put-char is about to output a #\newline,
; it should perform a tail call to this procedure instead.

(define (io/put-eol p)
  (let* ((buf        (vector-like-ref p port.mainbuf))
         (mainlim    (vector-like-ref p port.mainlim))
         (transcoder (vector-like-ref p port.transcoder))
         (eolstyle (fxlogand transcoder transcoder-mask:eolstyle)))

    (define (put-byte b)
      (bytevector-set! buf mainlim b)
      (let* ((mainpos (vector-like-ref p port.mainpos))
             (linesread (vector-like-ref p port.linesread)))
        (vector-like-set! p port.mainlim (+ mainlim 1))
        (vector-like-set! p port.linesread (+ linesread 1))
        (vector-like-set! p port.linestart (+ mainlim mainpos)))
      (unspecified))

    (define (put-bytes2 b0 b1)
      (bytevector-set! buf mainlim b0)
      (bytevector-set! buf (+ mainlim 1) b1)
      (finish 2))
    (define (put-bytes3 b0 b1 b2)
      (bytevector-set! buf mainlim b0)
      (bytevector-set! buf (+ mainlim 1) b1)
      (bytevector-set! buf (+ mainlim 2) b2)
      (finish 3))

    (define (finish count)
      (let* ((mainpos (vector-like-ref p port.mainpos))
             (mainpos (+ mainpos (- count 1)))
             (linesread (vector-like-ref p port.linesread)))
        (vector-like-set! p port.mainlim (+ mainlim count))
        (vector-like-set! p port.mainpos mainpos)
        (vector-like-set! p port.linesread (+ linesread 1))
        (vector-like-set! p port.linestart (+ mainlim mainpos)))
      (unspecified))

    (cond ((< (- (bytevector-length buf) mainlim) 4)
           (io/flush-buffer p)
           (io/put-eol p))

          ((or (eq? eolstyle eolstyle:none)
               (eq? eolstyle eolstyle:lf))
           (put-byte 10))

          ((eq? eolstyle eolstyle:cr)
           (put-byte 13))

          ((eq? eolstyle eolstyle:crlf)
           (put-bytes2 13 10))

          ((eq? codec:latin-1 (fxlogand transcoder transcoder-mask:codec))
           (cond ((eq? eolstyle eolstyle:nel)
                  (put-byte #x85))
                 ((eq? eolstyle eolstyle:crnel)
                  (put-bytes2 13 #x85))
                 (else
                  (assertion-violation 'put-char "internal error" p))))

          ((eq? codec:utf-8 (fxlogand transcoder transcoder-mask:codec))
           (cond ((eq? eolstyle eolstyle:nel)
                  (put-bytes2 #xc2 #x85))
                 ((eq? eolstyle eolstyle:crnel)
                  (put-bytes3 13 #xc2 #x85))
                 ((eq? eolstyle eolstyle:ls)
                  (put-bytes3 #xe2 #x80 #xa8))
                 (else
                  (assertion-violation 'put-char "internal error" p))))

          (else
           (assertion-violation 'put-char "internal error" p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; On-the-fly transcoding of UTF-8.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             (let* ((line (+ 1 (port-lines-read p)))
                    (msg (string-append "utf-8 decoding error in line "
                                        (number->string line))))
               (raise-r6rs-exception (make-i/o-decoding-error p)
                                     'get-char
                                     msg
                                     units))))))

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

        (vector-like-set! p
                          port.mainpos
                          (+ (vector-like-ref p port.mainpos) mainptr))
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
        (vector-like-set! p
                          port.mainpos
                          (+ (vector-like-ref p port.mainpos) mainptr))
        (vector-like-set! p port.mainptr 0)
        (vector-like-set! p port.mainlim 0)
        (vector-like-set! p port.auxptr 0)
        (vector-like-set! p port.auxlim m)
        (io/fill-buffer! p)
        (io/get-char p lookahead?))

       ((auxstart)
        (assert (eq? buf auxbuf))
        (assert (fx= 0 auxptr))
        (assert (fx< n 4))
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
        (let ((mainbuf (vector-like-ref p port.mainbuf))
              (mainptr (vector-like-ref p port.mainptr))
              (mainpos (vector-like-ref p port.mainpos)))
          (if (eq? mainbuf buf)
              (begin (vector-like-set! p port.mainpos (+ mainpos (- 1 k)))
                     (vector-like-set! p port.mainptr (+ k mainptr)))
              (begin (vector-like-set! p port.mainpos (+ mainpos 1))
                     (io/consume-byte-from-auxbuf! p)
                     (io/consume-byte-from-auxbuf! p)
                     (if (> k 2) (io/consume-byte-from-auxbuf! p))
                     (if (> k 3) (io/consume-byte-from-auxbuf! p))))))
    (case sv
     ((#x85 #x2028)
      (io/return-eol p lookahead? sv))
     (else
      (integer->char sv))))

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

(define (io/get-char-auxstart p lookahead?)

  (assert (eq? 'auxstart (vector-like-ref p port.state)))

  (let ((buf  (vector-like-ref p port.auxbuf))
        (ptr  (vector-like-ref p port.auxptr))
        (lim  (vector-like-ref p port.auxlim)))

    (cond ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (cond ((fx> unit 13)
                           ; not #\linefeed, #\return, #\nel, or #\x2028
                           (if (not lookahead?)
                               (let ((pos  (vector-like-ref p port.mainpos)))
                                 (vector-like-set! p port.mainpos (+ pos 1))
                                 (io/consume-byte-from-auxbuf! p)))
                           (integer->char unit))
                          ((or (fx= unit 10)                       ; #\linefeed
                               (fx= unit 13))                        ; #\return
                           (let ((pos (vector-like-ref p port.mainpos)))
                             (if (not lookahead?)
                                 (begin
                                  (vector-like-set! p port.mainpos (+ pos 1))
                                  (io/consume-byte-from-auxbuf! p)))
                             (io/return-eol p lookahead? unit)))
                          (else
                           (if (not lookahead?)
                               (let ((pos  (vector-like-ref p port.mainpos)))
                                 (vector-like-set! p port.mainpos (+ pos 1))
                                 (io/consume-byte-from-auxbuf! p)))
                           (integer->char unit))))
                   ((let ((codec (fxlogand
                                  transcoder-mask:codec
                                  (vector-like-ref p port.transcoder))))
                      (fx= codec codec:latin-1))
                    ; Latin-1
                    (if (not lookahead?)
                        (let ((pos  (vector-like-ref p port.mainpos)))
                          (vector-like-set! p port.mainpos (+ pos 1))
                          (io/consume-byte-from-auxbuf! p)))
                    (if (fx= unit #x85)
                        (io/return-eol p lookahead? unit)
                        (integer->char unit)))
                   (else
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
          (mainlim (vector-like-ref p port.mainlim))
          (mainpos (vector-like-ref p port.mainpos)))
      (assert (fx= 0 mainptr))
      (assert (fx< 0 mainlim))
      (vector-like-set! p port.mainpos (- mainpos 1))
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
; This procedure is called only during error handling
; and #\return handling, so it can be fairly slow.

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
             (vector-like-set! p port.mainpos
                               (- (vector-like-ref p port.mainpos) 1))
             (vector-like-set! p port.mainptr (+ mainptr 1)))
            ((eq? state 'auxend)
             (assert (fx< auxptr auxlim))
             (bytevector-copy! auxbuf auxptr mainbuf 0 (- auxlim auxptr))
             (bytevector-set! mainbuf (- auxlim auxptr) port.sentinel)
             (vector-like-set! p
                               port.mainpos
                               (+ (vector-like-ref p port.mainpos) mainptr))
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
