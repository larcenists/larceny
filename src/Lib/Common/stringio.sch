; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; MacScheme-compatible string I/O ports.

; String output ports are textual output ports whose ioproc keeps
; the characters written so far in a string of its own.  The port's
; buffer holds UTF-8, as for any textual port (so the in-line put-char
; stays as it is), and each flush decodes it into the string, so that
; get-output-string needs only to copy it out.  (They used to be UTF-8
; transcoded ports over bytevector output ports, whose bytes were
; copied into a growing bytevector and decoded as a whole by every
; get-output-string: several copies of every character, and three
; ports and their buffers to make.)
;
; Positions are in characters.  set-port-position! moves the place
; where characters are written; get-output-string returns all the
; characters up to the last one written.

(define string-io.string 1)     ; the characters
(define string-io.index 2)      ; where the next character goes
(define string-io.length 3)     ; how many there are

(define string-io/utf-8
  (io/make-transcoder 'utf-8 'none 'replace))

; String input ports are textual input ports whose ioproc fills the
; buffer by encoding the next characters of the string in UTF-8, as
; they are needed (they used to encode the whole string at once and
; read it through a transcoded bytevector input port).  Positions are
; in characters, and set-port-position! moves to a character index.

(define string-io.input-string 1)       ; the string
(define string-io.input-index 2)        ; index of the next character to encode

(define (string-io/open-input-string s)
  (if (not (string? s))
      (error "open-input-string: " s " is not a string."))
  (let* ((data (vector 'string-input-port s 0))
         (p (io/make-port string-io/input-ioproc data 'input 'text
                          'set-position!)))
    (vector-like-set! p port.transcoder string-io/utf-8)
    (bytevector-set! (vector-like-ref p port.mainbuf) 0 port.sentinel)
    (io/port-alist-set! p (list (cons 'port-position #t)))  ; in characters
    p))

(define (string-io/input-ioproc op)
  (case op
    ((read)   string-io/fill-buffer)
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*string*"))
    ((set-position!)
              (lambda (data posn)
                (if (<= 0 posn (string-length
                                (vector-ref data string-io.input-string)))
                    (begin (vector-set! data string-io.input-index posn)
                           'ok)
                    'error)))
    (else     (error "string-io/input-ioproc: illegal operation: " op))))

; Encodes whole characters of the string, from the next one, into the
; buffer, while at least 4 bytes are left in it.

(define (string-io/fill-buffer data buffer)
  (let* ((s (vector-ref data string-io.input-string))
         (i0 (vector-ref data string-io.input-index))
         (n (string-length s))
         (limit (- (bytevector-length buffer) 4)))
    (if (>= i0 n)
        'eof
        (let loop ((i i0) (j 0))
          (if (or (>= i n) (> j limit))
              (begin (vector-set! data string-io.input-index i)
                     j)
              (let ((sv (char->integer (string-ref s i))))
                (cond ((< sv #x80)
                       (bytevector-set! buffer j sv)
                       (loop (+ i 1) (+ j 1)))
                      ((< sv #x800)
                       (bytevector-set! buffer j
                                        (fxlogior #xc0 (fxrshl sv 6)))
                       (bytevector-set! buffer (+ j 1)
                                        (fxlogior #x80 (fxlogand sv #x3f)))
                       (loop (+ i 1) (+ j 2)))
                      ((< sv #x10000)
                       (bytevector-set! buffer j
                                        (fxlogior #xe0 (fxrshl sv 12)))
                       (bytevector-set! buffer (+ j 1)
                                        (fxlogior #x80 (fxlogand
                                                        (fxrshl sv 6) #x3f)))
                       (bytevector-set! buffer (+ j 2)
                                        (fxlogior #x80 (fxlogand sv #x3f)))
                       (loop (+ i 1) (+ j 3)))
                      (else
                       (bytevector-set! buffer j
                                        (fxlogior #xf0 (fxrshl sv 18)))
                       (bytevector-set! buffer (+ j 1)
                                        (fxlogior #x80 (fxlogand
                                                        (fxrshl sv 12) #x3f)))
                       (bytevector-set! buffer (+ j 2)
                                        (fxlogior #x80 (fxlogand
                                                        (fxrshl sv 6) #x3f)))
                       (bytevector-set! buffer (+ j 3)
                                        (fxlogior #x80 (fxlogand sv #x3f)))
                       (loop (+ i 1) (+ j 4))))))))))

; Accepts a UTF-8 transcoder as its optional argument,
; which is used to implement string->bytevector.

(define (string-io/open-output-string . rest)
  (assert (or (null? rest)
              (eq? (transcoder-codec (car rest)) 'utf-8)))
  (let* ((data (vector 'string-output-port (make-string 32) 0 0))
         (p (io/make-port string-io/ioproc data 'output 'text 'set-position!)))
    (vector-like-set! p port.transcoder
                      (if (null? rest) string-io/utf-8 (car rest)))
    (io/port-alist-set! p (list (cons 'port-position #t)))  ; in characters
    p))

(define (string-io/get-output-string port)
  (if (not (string-output-port? port))
      (assertion-violation 'get-output-string "illegal argument" port))
  (flush-output-port port)
  (let ((data (vector-like-ref port port.iodata)))
    (substring (vector-ref data string-io.string)
               0
               (vector-ref data string-io.length))))

(define (string-io/reset-output-string port)
  (if (not (string-output-port? port))
      (error "reset-output-string: " port " is not a string output port."))
  (set-port-position! port 0)
  (vector-set! (vector-like-ref port port.iodata) string-io.length 0)
  (unspecified))

(define (string-io/ioproc op)
  (case op
    ((read)   (lambda (data buffer)
                (assertion-error 'string-io/ioproc "internal error")))
    ((write)  string-io/flush-buffer)
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*string*"))
    ((set-position!)
              (lambda (data posn)
                (string-io/room! data posn)
                (vector-set! data string-io.index posn)
                'ok))
    (else     (error "string-io/ioproc: illegal operation: " op))))

; Makes room in the string for n characters.

(define (string-io/room! data n)
  (let ((s (vector-ref data string-io.string)))
    (if (> n (string-length s))
        (let ((s2 (make-string (max n (* 2 (string-length s))))))
          (string-copy! s2 0 s 0 (vector-ref data string-io.length))
          (vector-set! data string-io.string s2)))))

; Decodes the UTF-8 in buffer[0..count) into the string.  The buffer
; never ends within a character: put-char flushes before encoding a
; character that would not fit.

(define (string-io/flush-buffer data buffer count)
  (let ((i0 (vector-ref data string-io.index)))
    (string-io/room! data (+ i0 count))
    (let ((s (vector-ref data string-io.string)))
      (define (byte k) (bytevector-ref buffer k))
      (define (cont k) (fxlogand (byte k) #x3f))
      (let loop ((j 0) (i i0))
        (if (< j count)
            (let ((b (byte j)))
              (cond ((< b #x80)
                     (string-set! s i (integer->char b))
                     (loop (+ j 1) (+ i 1)))
                    ((< b #xe0)
                     (string-set! s i (integer->char
                                       (fxlogior (fxlsh (fxlogand b #x1f) 6)
                                                 (cont (+ j 1)))))
                     (loop (+ j 2) (+ i 1)))
                    ((< b #xf0)
                     (string-set! s i (integer->char
                                       (fxlogior (fxlsh (fxlogand b #x0f) 12)
                                                 (fxlogior
                                                  (fxlsh (cont (+ j 1)) 6)
                                                  (cont (+ j 2))))))
                     (loop (+ j 3) (+ i 1)))
                    (else
                     (string-set! s i (integer->char
                                       (fxlogior
                                        (fxlogior (fxlsh (fxlogand b #x07) 18)
                                                  (fxlsh (cont (+ j 1)) 12))
                                        (fxlogior (fxlsh (cont (+ j 2)) 6)
                                                  (cont (+ j 3))))))
                     (loop (+ j 4) (+ i 1)))))
            (begin
              (vector-set! data string-io.index i)
              (if (> i (vector-ref data string-io.length))
                  (vector-set! data string-io.length i)))))))
  'ok)

(define (string-output-port? port)
  (and (output-port? port)
       (let ((d (vector-like-ref port port.iodata)))
         (and (vector? d)
              (> (vector-length d) 0)
              (eq? (vector-ref d 0) 'string-output-port)))))

; eof
