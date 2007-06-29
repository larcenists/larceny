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

; eol-style is implemented in Compiler/usual.sch

(define (native-eol-style) 'none)   ; FIXME: for backward compatibility

; FIXME:  &i/o-decoding, &i/o-encoding, and their associated
; operations aren't yet implemented because conditions aren't
; implemented yet.

; error-handling-mode is implemented in Compiler/usual.sch

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
  (io/transcoder-codec t))

(define (transcoder-eol-style t)
  (io/transcoder-eol-style t))

(define (transcoder-error-handling-mode t)
  (io/transcoder-error-handling-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Operations on ports.
; R5RS operations are in stdio.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (port-transcoder p)
  (let ((t (io/port-transcoder p)))
    (if (eq? (io/transcoder-codec t) 'binary)
        #f
        t)))

(define (textual-port? p) (io/textual-port? p))

(define (binary-port? p) (io/binary-port? p))

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

(define (port-position p) (io/port-position p))

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
         (file-io/open-file-input-port filename '() 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-input-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-input-port filename (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-input-port filename
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
; Output ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: output-port? is defined in stdio.sch

; FIXME: flush-output-port is defined in stdio.sch

(define (output-port-buffer-mode p)
  (if (output-port? p)
      (io/buffer-mode p)
      (assertion-violatoin 'output-port-buffer-mode "not an output port" p)))

; FIXME: fakes file options

(define (open-file-output-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-output-port filename '() 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-output-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-output-port filename (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-output-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-output-port
                              "wrong number of arguments"
                              (cons filename rest)))))

(define (open-bytevector-output-port . rest)
  (let ((transcoder (if (null? rest) #f (car rest)))
        (port (bytevector-io/open-output-bytevector)))
    (if transcoder
        (transcoded-port port transcoder)
        port)))

; FIXME:  Doesn't check legitimacy of the transcoder.

(define (call-with-bytevector-output-port f . rest)
  (if (and (procedure? f)
           (or (null? rest)
               (null? (cdr rest))))
      (let* ((transcoder (if (null? rest) #f (car rest)))
             (p (open-bytevector-output-port transcoder)))
        (dynamic-wind (lambda () #t)
                      (lambda () (f p))
                      (lambda () (close-output-port p))))
      (assertion-violation 'call-with-bytevector-output-port
                           "illegal argument(s)" f)))

(define (open-string-output-port)
  (let* ((transcoder (make-transcoder (utf-8-codec) 'none 'ignore))
         (port (bytevector-io/open-output-bytevector))
         (port (transcoded-port port transcoder))
         (f (lambda ()
              (let ((s (utf8->string
                        (bytevector-io/get-output-bytevector port))))
                (bytevector-io/reset-output-bytevector port)
                s))))
    (values port f)))

(define (call-with-string-output-port f)
  (if (procedure? f)
      (let ((p (open-string-output-port)))
        (dynamic-wind (lambda () #t)
                      (lambda () (f p))
                      (lambda () (close-output-port p))))
      (assertion-violation 'call-with-string-output-port
                           "illegal argument" f)))

; FIXME: not implemented yet

(define (standard-output-port)
  (assertion-violation 'standard-output-port "not yet implemented"))

; FIXME: not implemented yet

(define (standard-error-port)
  (assertion-violation 'standard-error-port "not yet implemented"))

; FIXME: not implemented yet

(define (current-error-port)
  (assertion-violation 'current-error-port "not yet implemented"))

; FIXME: not implemented yet

(define (make-custom-binary-output-port
         id read! get-position set-position! close)
  (assertion-violation 'make-custom-binary-output-port "not yet implemented"))

; FIXME: not implemented yet

(define (make-custom-textual-output-port
         id read! get-position set-position! close)
  (assertion-violation 'make-custom-textual-output-port "not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic input (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookahead-u8 p)   (io/get-u8 p #t))
(define (get-u8 p)         (io/get-u8 p #f))
(define (lookahead-char p) (io/get-char p #t))
(define (get-char p)       (io/get-char p #f))

; FIXME: not yet implemented
;     get-bytevector-n
;     get-bytevector-n!
;     get-bytevector-some
;     get-bytevector-all
;     get-string-n
;     get-string-n!
;     get-string-all
;     get-line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic output (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-u8 p k)   (io/put-u8 p k))
(define (put-char p c) (io/put-char p c))

(define (put-bytevector p bv . rest)
  (define (put-bytevector p bv start count)
    (if (and (null? (cddr rest))
             (binary-port? p)
             (output-port? p)
             (bytevector? bv)
             (fixnum? start)
             (fixnum? count)
             (fx<= 0 start)
             (fx<= 0 count)
             (fx<= (fx+ start count) (bytevector-length bv)))
        (let ((n (fx+ start count)))
          (do ((i start (+ i 1)))
              ((fx= i n))
            (put-u8 p (bytevector-ref bv i))))
        (assertion-violation 'put-bytevector
                             "illegal argument(s)" p bv start count)))
  (cond ((null? rest)
         (put-bytevector p bv 0 (bytevector-length bv)))
        ((null? (cdr rest))
         (put-bytevector p bv (car rest) (- (bytevector-length bv) (car rest)))
        ((null? (cddr rest))
         (put-bytevector p bv (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-bytevector
                              "too many arguments" (cons p (cons bv rest)))))))

(define (put-string p s . rest)
  (define (put-string p s start count)
    (if (and (textual-port? p)
             (output-port? p)
             (string? s)
             (fixnum? start)
             (fixnum? count)
             (fx<= 0 start)
             (fx<= 0 count)
             (fx<= (fx+ start count) (string-length s)))
        (let ((n (fx+ start count)))
          (do ((i start (+ i 1)))
              ((fx= i n))
            (put-char p (string-ref s i))))
        (assertion-violation 'put-string
                             "illegal argument(s)" p s start count)))
  (cond ((null? rest)
         (put-string p s 0 (string-length s)))
        ((null? (cdr rest))
         (put-string p s (car rest) (- (string-length s) (car rest)))
        ((null? (cddr rest))
         (put-string p s (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-string
                              "too many arguments" (cons p (cons s rest)))))))

(define (put-datum p x)
  (write x p))
