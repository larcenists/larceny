; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny -- R6RS-compatible I/O system.

; FIXME:  These are supposed to be syntax,
; but I'm hoping they go away before the R6RS is ratified.

(define (file-options . args) '())    ; FIXME

; FIXME: not yet implemented, because I hope it goes away
;     buffer-mode

(define (buffer-mode? mode)
  (case mode
   ((none line block) #t)
   (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Transcoders et cetera.
; For representations, see iosys.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latin-1-codec) 'latin-1)
(define (utf-8-codec) 'utf-8)
(define (utf-16-codec) 'utf-16)

; FIXME: not yet implemented, because I hope it goes away
;     eol-style

(define (native-eol-style) 'none)   ; FIXME: for backward compatibility

; FIXME:  &i/o-decoding, &i/o-encoding, and their associated
; operations aren't yet implemented because the raise mode
; no longer makes sense.  A formal comment has been submitted.

; FIXME: not yet implemented, because I hope it goes away
;     error-handling-mode

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

(define (native-transcoder)
  (make-transcoder (latin-1-codec) 'none 'ignore))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Operations on ports.
; R5RS operations are in stdio.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (port-transcoder p)
  (assert (port? p))
  (vector-like-ref p port.transcoder))

(define (textual-port? p)
  (assert (port? p))
  (not (zero? (fxlogand 1 (vector-like-ref p port.type)))))

(define (binary-port? p)
  (assert (port? p))
  (zero? (fxlogand 1 (vector-like-ref p port.type))))

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

; FIXME:  This depends on the representation of ports.

(define (port-position p)
  (if (binary-port? p)
      (+ (vector-like-ref p port.position)
         (if (input-port? p)
             (vector-like-ref p port.byteptr)      ; FIXME
             (vector-like-ref p port.wr-lim)))     ; FIXME
      (+ (vector-like-ref p port.mainpos)          ; FIXME
         (vector-like-ref p port.mainptr))))       ; FIXME

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
     (if (io/open-port? p) (io/close-port p))
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

(define (open-bytevector-input-port bv . rest)
  (let ((transcoder (if (null? rest) #f (car rest)))
        (port (bytevector-io/open-input-bytevector bv)))
    (if transcoder
        (transcoded-port port transcoder)
        port)))

(define (open-string-input-port s)
  (let ((transcoder (make-transcoder (utf-8-codec) 'none 'ignore))
        (port (bytevector-io/open-input-bytevector-no-copy (string->utf8 s))))
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

; FIXME:  The next three definitions are broken.

; The fast path for lookahead-u8.

(define (lookahead-u8 p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))        ; FIXME: should be trusted
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (if (and (eq? type 2)                           ; 2 = input, binary
             (fx< ptr lim))
        (bytevector-ref buf ptr)                    ; FIXME
        (io/get-u8 p #t))))

; The fast path for get-u8.

(define (get-u8 p)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))        ; FIXME: should be trusted
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (if (and (eq? type 2)                           ; 2 = input, binary
             (fx< ptr lim))
        (begin (vector-like-set! p port.mainptr (+ ptr 1))
               (bytevector-ref buf ptr))            ; FIXME
        (io/get-u8 p #f))))

; The general case for get-u8 and lookahead-u8.

(define (io/get-u8 p lookahead?)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (cond ((not (eq? type 2))
           (assertion-violation 'get-u8 "argument not a binary input port" p))
          ((fx< ptr lim)
           (let ((byte (bytevector-ref buf ptr)))
             (if (not lookahead?)
                 (vector-like-set! p port.mainptr (+ ptr 1)))
             byte))
          ((vector-like-ref p port.rd-eof?)
           (eof-object))
          (else
           (io/fill-buffer p)
           (io/get-u8 p lookahead?)))))

; The fast path for lookahead-char.

(define (lookahead-char p)
  (assert (port? p))                           ; FIXME
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr)))
    (let ((unit (if (eq? type 3)               ; 3 = input, textual
                    (bytevector-ref buf ptr)   ; FIXME
                    255)))
      (if (and (not (eq? unit 13))             ; 13 = #\return
               (fx< unit 128))
          (integer->char unit)
          (i/get-char p #t)))))

; The fast path for get-char.

(define (get-char p)
  (assert (port? p))                           ; FIXME
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr)))
    (let ((unit (if (eq? type 3)               ; 3 = input, textual
                    (bytevector-ref buf ptr)   ; FIXME
                    255)))
      (if (and (not (eq? unit 13))             ; 13 = #\return
               (fx< unit 128))
          (begin
           (vector-set! p port.mainptr (fx+ ptr 1))
           (integer->char unit))
          (read-char p)))))

; FIXME: transcoding-on-fly not implemented yet (incomplete characters?)
; FIXME: io/fill-buffer doesn't switch buffers yet
; The general case for get-char and lookahead-char.

(define (io/get-char p lookahead?)
  (assert (port? p))
  (let ((type (vector-like-ref p port.type))
        (buf  (vector-like-ref p port.mainbuf))
        (ptr  (vector-like-ref p port.mainptr))
        (lim  (vector-like-ref p port.mainlim)))
    (cond ((not (eq? type 3))
           (assertion-violation 'get-char
                                "argument not a textual input port" p))
          ((fx< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (if (not lookahead?)
                        (vector-like-set! p port.mainptr (+ ptr 1)))
                    (integer->char unit))
                   ((<= unit #xdf)
                    (if (not lookahead?)
                        (vector-like-set! p port.mainptr (+ ptr 2)))
                    (integer->char
                     (fxlogior
                      (fxlsh (fxlogand #b00011111 unit) 6)
                      (fxlogand #b00111111 (bytevector-ref buf (+ ptr 1))))))
                   ((<= unit #xef)
                    (if (not lookahead?)
                        (vector-like-set! p port.mainptr (+ ptr 3)))
                    (integer->char
                     (fxlogior
                      (fxlsh (fxlogand #b00001111 unit) 12)
                      (fxlogior
                       (fxlsh (fxlogand #x3f (bytevector-ref buf (+ ptr 1)))
                              6)
                       (fxlogand #x3f (bytevector-ref buf (+ ptr 2)))))))
                   (else
                    (if (not lookahead?)
                        (vector-like-set! p port.mainptr (+ ptr 4)))
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
           (vector-like-set! p port.mainptr 0)
           (vector-like-set! p port.mainlim 0)
           (bytevector-set! buf 0 port.sentinel)
           (eof-object))
          (else
           (vector-like-set! p port.mainptr 0)
           (vector-like-set! p port.mainlim 0)
           (bytevector-set! buf 0 port.sentinel)
           (io/fill-buffer p)
           (io/transcode-port! p)
           (io/get-char p lookahead?)))))

; FIXME: not yet implemented
;     get-bytevector-n
;     get-bytevector-n!
;     get-bytevector-some
;     get-bytevector-all
;     get-string-n
;     get-string-n!
;     get-string-all
;     get-line
