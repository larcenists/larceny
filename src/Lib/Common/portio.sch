; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny -- R6RS-compatible I/O system.

($$trace "portio")

; The deprecated buffer-mode syntax is supported only by R6RS modes,
; but the enumeration set has to be defined here so the i/o system
; can use it.  Several nonstandard symbols are included so Spanky
; mode can do arbitrary things with them.

(define *file-option-symbols*
  '(no-create no-fail no-truncate
    spanky0 spanky1 spanky2))

; Enumeration sets can't be created until late in the initialization
; process, so the creation of this enumeration set must be delayed.

(define *file-options-enumeration-set* #f)

(define (file-options-enumeration-set)
  (if (not *file-options-enumeration-set*)
      (set! *file-options-enumeration-set*
            (make-enumeration *file-option-symbols*)))
  *file-options-enumeration-set*)  

(define (make-file-options-set syms)
  ((enum-set-constructor (file-options-enumeration-set)) syms))

(define (file-options->list options)
  (enum-set->list options))

; toplevel.sch defines variables no-create, no-fail, and
; no-truncate so they don't have to be quoted when passed
; to file-options.

(define (file-options . symbols)
  (make-file-options-set
   (filter (lambda (sym) (memq sym *file-option-symbols*))
           symbols)))

; The R6RS specification of buffer-mode? does not allow it
; to return true for the datum buffer mode, so this predicate
; is worse than useless.

(define (buffer-mode? mode)
  (issue-warning-deprecated 'buffer-mode?)
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

; The deprecated eol-style syntax is supported only by R6RS modes.

(define (native-eol-style) 'none)   ; FIXME: for backward compatibility

; FIXME:  &i/o-decoding, &i/o-encoding, and their associated
; operations aren't yet implemented because conditions aren't
; implemented yet.

; The deprecated error-handling-mode syntax is supported only by R6RS modes.

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

(define (bytevector->string bv t)
  (call-with-port
   (transcoded-port (open-input-bytevector bv) t)
   get-string-all))

; When converting a string to a bytevector using the UTF-8
; or UTF-16 encoding forms, no encoding errors are possible
; so the transcoder's error-handling mode doesn't matter.

(define (string->bytevector s t)
  (let ((n (string-length s))
        (codec (transcoder-codec t))
        (eolstyle (transcoder-eol-style t))
        (errmode (transcoder-error-handling-mode t)))
    (cond ((eq? eolstyle 'none)
           (case codec
            ((latin-1)
             (call-with-port
              (open-output-bytevector)
              (lambda (out)
                (do ((bv (make-bytevector n))
                     (i 0 (+ i 1)))
                    ((= i n) (get-output-bytevector out))
                  (let ((sv (char->integer (string-ref s i))))
                    (cond ((< sv 256)
                           (put-u8 out sv))
                          ((eq? errmode 'replace)
                           (put-u8 out (char->integer #\?)))
                          ((eq? errmode 'raise)
                           (assertion-violation
                            'string->bytevector "encoding error" sv))
                          (else
                           'ignore)))))))
            ((utf-8)
             (string->utf8 s))
            ((utf-16)
             (string->utf16 s))
            (else
             (assertion-violation 'string->bytevector
                                  "illegal arguments" s t))))
          ((eq? codec 'utf-8)
           (call-with-port
            (string-io/open-output-string t)
            (lambda (out)
              (put-string out s)
              (string->utf8 (get-output-string out)))))
          (else
           (string->bytevector
            (utf8->string
             (string->bytevector
              s (make-transcoder (utf-8-codec) eolstyle 'ignore)))
            (make-transcoder codec 'none errmode))))))

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
           (memq (transcoder-codec t) '(latin-1 utf-8 utf-16))
           (memq (transcoder-eol-style t) '(none lf cr crlf nel crnel ls))
           (memq (transcoder-error-handling-mode t) '(ignore replace raise))
           (if (and (input-port? p) (output-port? p))
               (and (eq? (transcoder-codec t) 'latin-1)
                    (eq? (transcoder-eol-style t) 'none))
               #t))
      (io/transcoded-port p t)
      (assertion-violation 'transcoded-port
                           "bad port or unsupported transcoder" p t)))

; FIXME:  For now, all binary and textual ports support port-position.

(define (port-has-port-position? p)
  (or (binary-port? p) (textual-port? p)))

(define (port-position p) (io/port-position p))

; FIXME:  Do these extensions to R6RS i/o belong in this file?

(define (port-lines-read p) (io/port-lines-read p))
(define (port-line-start p) (io/port-line-start p))

(define (port-has-set-port-position!? p)
  (io/port-has-set-port-position!? p))

(define (set-port-position! p pos)
  (io/set-port-position! p pos))

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

(define (open-file-input-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-input-port filename (file-options) 'block #f))
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
      (assertion-violation 'output-port-buffer-mode "not an output port" p)))

; FIXME: fakes file options

(define (open-file-output-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-output-port filename (file-options) 'block #f))
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

; FIXME:  Doesn't check legitimacy of the transcoder.

(define (open-bytevector-output-port . rest)
  (issue-warning-deprecated 'open-bytevector-output-port)
  (let ((transcoder (if (null? rest) #f (car rest))))
    (if transcoder
        (let ((out (open-output-string)))
          (values out
                  (lambda ()
                    (let* ((s (get-output-string out))
                           (bv (string->bytevector s transcoder)))
                      (reset-output-string out)
                      bv))))
        (let ((out (open-output-bytevector)))
          (values out
                  (lambda ()
                    (let ((bv (get-output-bytevector out)))
                      (reset-output-bytevector out)
                      bv)))))))

; FIXME:  Doesn't check legitimacy of the transcoder.

(define (call-with-bytevector-output-port f . rest)
  (if (and (procedure? f)
           (or (null? rest)
               (null? (cdr rest))))
      (let ((transcoder (if (null? rest) #f (car rest))))
        (if transcoder
            (call-with-port
             (open-output-string)
             (lambda (out)
               (f out)
               (let ((s (get-output-string out)))
                 (reset-output-string out)
                 (string->bytevector s transcoder))))
            (call-with-port
             (open-output-bytevector)
             (lambda (out)
               (f out)
               (get-output-bytevector out)))))
      (assertion-violation 'call-with-bytevector-output-port
                           "illegal argument(s)" f)))

(define (open-string-output-port)
  (issue-warning-deprecated 'open-string-output-port)
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
      (call-with-port
       (open-output-string)
       (lambda (out) (f out) (get-output-string out)))
      (assertion-violation 'call-with-string-output-port
                           "illegal argument" f)))

(define (open-file-input/output-port filename . rest)
  (cond ((null? rest)
         (file-io/open-file-input/output-port
          filename (file-options) 'block #f))
        ((null? (cdr rest))
         (file-io/open-file-input/output-port filename (car rest) 'block #f))
        ((null? (cddr rest))
         (file-io/open-file-input/output-port filename
                                              (car rest) (cadr rest) #f))
        ((null? (cdddr rest))
         (file-io/open-file-input/output-port filename
                                  (car rest) (cadr rest) (caddr rest)))
        (else
         (assertion-violation 'open-file-input/output-port
                              "wrong number of arguments"
                              (cons filename rest)))))

; FIXME:  This belongs in fileio.sch, and should be implemented better.

(define (file-io/open-file-input/output-port filename opts bufmode t)
  (let ((dir (current-directory)))
    (cond ((not t)
           (let* ((initial-contents
                   (call-with-port
                    (open-file-input-port filename)
                    get-bytevector-all))
                  (bvport (open-input/output-bytevector initial-contents))
                  (show
                   (lambda ()
                     (display " ")
                     (write (vector-like-ref bvport 7))
                     (newline)))
                  (read-method
                   (lambda (bv start count)
                     (write (list 'reading start count))
                     (show)
                     (get-bytevector-n! bvport bv start count)))
                  (write-method
                   (lambda (bv start count)
                     (write (list 'writing start count))
                     (show)
                     (put-bytevector bvport bv start count)
                     count))
                  (get-position-method
                   (lambda () (port-position bvport)))
                  (set-position-method
                   (lambda (posn) (set-port-position! bvport posn)))
                  (close-method
                   (lambda ()
                     (let* ((final-contents (get-output-bytevector bvport))
                            (current-dir (current-directory)))
                       (dynamic-wind
                        (lambda () (current-directory dir))
                        (lambda ()
                          (call-with-port
                           (open-file-output-port filename opts bufmode)
                           (lambda (out)
                             (put-bytevector out final-contents))))
                        (lambda () (current-directory current-dir)))))))
             (make-custom-binary-input/output-port
              filename
              read-method write-method
              get-position-method set-position-method close-method)))
          ((eq? (transcoder-codec t) 'latin-1)
           (transcoded-port
            (file-io/open-file-input/output-port filename opts bufmode #f)
            t))
          (else
           (assertion-violation
            'open-file-input/output-port
            "illegal codec" t)))))

; FIXME: not implemented yet

(define (standard-output-port)
  (assertion-violation 'standard-output-port "not yet implemented"))

; FIXME: not implemented yet

(define (standard-error-port)
  (assertion-violation 'standard-error-port "not yet implemented"))

; FIXME: not implemented yet

(define (current-error-port)
  (assertion-violation 'current-error-port "not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custom ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-custom-binary-input-port
         id read! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-binary-input-port...get-port-position))
  (customio/make-binary-input-port
   id read! get-position set-position! close))

(define (make-custom-binary-output-port
         id write! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-binary-output-port...get-port-position))
  (customio/make-binary-output-port
   id write! get-position set-position! close))

(define (make-custom-binary-input/output-port
         id read! write! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-binary-input/output-port...get-port-position))
  (if (not set-position!)
      (issue-warning-deprecated
       'make-custom-binary-input/output-port...set-port-position!))
  (customio/make-binary-input/output-port
   id read! write! get-position set-position! close))

(define (make-custom-textual-input-port
         id read! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-textual-input-port...get-port-position))
  (customio/make-textual-input-port
   id read! get-position set-position! close))

(define (make-custom-textual-output-port
         id write! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-textual-output-port...get-port-position))
  (customio/make-textual-output-port
   id write! get-position set-position! close))

(define (make-custom-textual-input/output-port
         id read! write! get-position set-position! close)
  (if get-position
      (issue-warning-deprecated
       'make-custom-textual-input/output-port...get-port-position))
  (if (not set-position!)
      (issue-warning-deprecated
       'make-custom-textual-input/output-port...set-port-position!))
  (customio/make-textual-input/output-port
   id read! write! get-position set-position! close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic input (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookahead-u8 p)   (io/get-u8 p #t))
(define (get-u8 p)         (io/get-u8 p #f))
(define (lookahead-char p) (io/get-char p #t))
(define (get-char p)       (io/get-char p #f))

(define (get-bytevector-n p count)
  (if (and (input-port? p)
           (binary-port? p)
           (fixnum? count)
           (fx<= 0 count))
      (call-with-bytevector-output-port
       (lambda (out)
         (do ((count count (fx- count 1)))
             ((or (port-eof? p) (fx= count 0)))
           (put-u8 out (get-u8 p)))))
      (portio/illegal-arguments 'get-bytevector-n p count)))

(define (get-bytevector-n! p bv start count)
  (if (and (input-port? p)
           (binary-port? p)
           (bytevector? bv)
           (fixnum? start)
           (fx<= 0 start)
           (fixnum? count)
           (fx<= 0 count)
           (fx<= (fx+ start count) (bytevector-length bv)))
      (do ((n    (fx+ start count))
           (i    start      (fx+ i 1)))
          ((or (port-eof? p) (fx= i n))
           (- i start))
        (bytevector-set! bv i (get-u8 p)))
      (portio/illegal-arguments 'get-bytevector-n! p bv start count)))

; FIXME:  This is extremely inefficient.

(define (get-bytevector-some p)
  (issue-warning-deprecated 'get-bytevector-some)
  (if (and (input-port? p)
           (binary-port? p))
      (let ((byte (get-u8 p)))
        (if (eof-object? byte)
            byte
            (make-bytevector 1 byte)))
      (portio/illegal-arguments 'get-bytevector-some p)))

(define (get-bytevector-all p)
  (if (and (input-port? p)
           (binary-port? p))
      (let ((bv (call-with-port
                 (open-output-bytevector)
                 (lambda (out)
                   (do ((byte (get-u8 p) (get-u8 p)))
                       ((eof-object? byte)
                        (get-output-bytevector out))
                     (put-u8 out byte))))))
        (if (fx= 0 (bytevector-length bv))
            (eof-object)
            bv))
      (portio/illegal-arguments 'get-bytevector-all p)))

(define (get-string-n p count)
  (if (and (input-port? p)
           (textual-port? p)
           (fixnum? count)
           (fx<= 0 count))
      (call-with-string-output-port
       (lambda (out)
         (do ((count count (fx- count 1))
              (char (get-char p) (get-char p)))
             ((or (eof-object? char) (fx<= count 0)))
           (put-char out char))))
      (portio/illegal-arguments 'get-string-n p count)))

(define (get-string-n! p s start count)
  (if (and (input-port? p)
           (textual-port? p)
           (string? s)
           (fixnum? start)
           (fx<= 0 start)
           (fixnum? count)
           (fx<= 0 count)
           (fx< (fx+ start count) (string-length s)))
      (do ((n    (fx+ start count))
           (i    start      (fx+ i 1))
           (char (get-char p) (get-char p)))
          ((or (eof-object? char) (fx= i n))
           (- i start))
        (string-set! s i char))
      (portio/illegal-arguments 'get-string-n! p s start count)))

(define (get-string-all p)
  (if (and (input-port? p)
           (textual-port? p))
      (let ((s (call-with-string-output-port
                 (lambda (out)
                   (do ((char (get-char p) (get-char p)))
                       ((eof-object? char))
                     (put-char out char))))))
        (if (fx= 0 (string-length s))
            (eof-object)
            s))
      (portio/illegal-arguments 'get-string-all p)))

(define (get-line p)
  (if (and (input-port? p)
           (textual-port? p))
      (let* ((eof? #f)
             (s (call-with-string-output-port
                 (lambda (out)
                   (do ((char (get-char p) (get-char p)))
                       ((or (eof-object? char) (char=? char #\linefeed))
                        (if (eof-object? char) (set! eof? #t)))
                     (put-char out char))))))
        (if (and eof? (fx= 0 (string-length s)))
            (eof-object)
            s))
      (portio/illegal-arguments 'get-string-all p)))

(define (portio/illegal-arguments who . irritants)
  (apply assertion-violation who "illegal argument(s)" irritants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic output (way incomplete)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-u8 p k)   (io/put-u8 p k))
(define (put-char p c) (io/put-char p c))

(define (put-bytevector p bv . rest)
  (define (put-bytevector p bv start count)
    (if (and (binary-port? p)
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
         (let* ((start (car rest))
                (count (- (bytevector-length bv) start)))
           (put-bytevector p bv start count)))
        ((null? (cddr rest))
         (put-bytevector p bv (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-bytevector
                              "too many arguments" (cons p (cons bv rest))))))

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
         (put-string p s (car rest) (- (string-length s) (car rest))))
        ((null? (cddr rest))
         (put-string p s (car rest) (cadr rest)))
        (else
         (assertion-violation 'put-string
                              "too many arguments" (cons p (cons s rest))))))

(define (put-datum p x)
  (write x p))
