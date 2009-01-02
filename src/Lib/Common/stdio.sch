; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- standard Scheme I/O library
;
; Procedures we could add:
;   file-port?
;   console-port?
;   closed-port?

($$trace "stdio")

; Making current-input-port and current-output-port into parameters
; lost the pure elegance of Lars Hansen's factory idea, but it still
; works (to some extent) by going through an error handler.

(define current-input-port 
  (make-parameter "current-input-port" #f (lambda (x) (input-port? x))))

(define current-output-port 
  (make-parameter "current-output-port" #f (lambda (x) (output-port? x))))

; Rebinding the current-error-port can cause an infinite loop
; when errors occur, so current-error-port isn't a parameter.

(define (current-error-port)
  (console-error-port))

(define (initialize-io-system)
  (io/initialize)
  (file-io/initialize)
  (console-io/initialize)
  (current-input-port (console-input-port))
  (current-output-port (console-output-port))
; (current-error-port (console-error-port))     ; see comment above
  (unspecified))

(define (shutdown-io-system)
  (file-io/finalize)
  (io/finalize)
  (unspecified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Deprecated procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For backwards compatibility, Larceny's old-style binary files
; are represented by R6RS-style textual file ports with Latin-1
; transcoding and no end-of-line conversion.

(define (open-raw-latin-1-input-file filename)
  (open-file-input-port filename
                        (file-options)
                        'block
                        (make-transcoder (latin-1-codec) 'none 'ignore)))

(define (open-raw-latin-1-output-file filename)
  (open-file-output-port filename
                         (file-options)
                         'block
                         (make-transcoder (latin-1-codec) 'none 'ignore)))

(define (call-with-raw-latin-1-input-file file proc)
  (let ((port (open-raw-latin-1-input-file file)))
    (let ((r (proc port)))
      (close-input-port port)
      r)))

(define (call-with-raw-latin-1-output-file file proc)
  (let ((port (open-raw-latin-1-output-file file)))
    (let ((r (proc port)))
      (close-output-port port)
      r)))

(define (with-input-from-raw-latin-1-file fn thunk)
  (call-with-raw-latin-1-input-file fn
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-raw-latin-1-file fn thunk)
  (call-with-raw-latin-1-output-file fn
    (lambda (p)
      (with-output-to-port p thunk))))

; FIXME:  These names should go away as soon as possible.

(define open-binary-input-file open-raw-latin-1-input-file)
(define open-binary-output-file open-raw-latin-1-output-file)
(define call-with-binary-input-file call-with-raw-latin-1-input-file)
(define call-with-binary-output-file call-with-raw-latin-1-output-file)
(define with-input-from-binary-file with-input-from-raw-latin-1-file)
(define with-output-to-binary-file with-output-to-raw-latin-1-file)

; The main problem with these is that their names suggest
; the presence of binary analogues.

(define (open-text-input-file filename)
  (file-io/open-file filename 'input 'text))

(define (open-text-output-file filename)
  (file-io/open-file filename 'output 'text))

(define (call-with-text-input-file file proc)
  (let ((port (open-text-input-file file)))
    (let ((r (proc port)))
      (close-input-port port)
      r)))

(define (call-with-text-output-file file proc)
  (let ((port (open-text-output-file file)))
    (let ((r (proc port)))
      (close-output-port port)
      r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of deprecated procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Read-char has been re-coded in MAL for performance; see Lib/malcode.mal.
;
;(define (read-char . rest)
;  (cond ((null? rest)
;        (io/read-char (current-input-port)))
;       ((null? (cdr rest))
;        (io/read-char (car rest)))
;       (else
;        (error "read-char: too many arguments.")
;        #t)))

(define (peek-char . rest)
  (cond ((null? rest)
         (io/peek-char (current-input-port)))
        ((null? (cdr rest))
         (io/peek-char (car rest)))
        (else
         (error "peek-char: too many arguments.")
         #t)))

; FIXME:  This no longer works!

(define (char-ready? . rest)
  (cond ((null? rest)
         (io/char-ready? (current-input-port)))
        ((null? (cdr rest))
         (io/char-ready? (car rest)))
        (else
         (error "char-ready?: too many arguments.")
         #t)))

; Write-char has been re-coded in MAL for performance; see Lib/malcode.mal.
;
;(define (write-char c . rest)
;  (cond ((null? rest)
;        (io/write-char c (current-output-port)))
;       ((null? (cdr rest))
;        (io/write-char c (car rest)))
;       (else
;        (error "write-char: too many arguments.")
;        #t)))

(define (write-bytevector-like bvl . rest)
  (if (pair? rest)
      (if (null? (cdr rest))
          (io/write-bytevector-like bvl (car rest))
          (begin (error "write-bytevector-like: too many arguments.")
                 #t))
      (io/write-bytevector-like bvl (current-output-port))))

;; Simply emits the characters in string to the port.
(define (write-string string . rest)
  (if (pair? rest)
      (if (null? (cdr rest))
          (io/write-string string (car rest))
          (begin (error "write-string: too many arguments.")
                 #t))
      (io/write-string string (current-output-port))))

(define (input-port? p)
  (io/input-port? p))

(define (output-port? p)
  (io/output-port? p))

(define (port-name p)
  (io/port-name p))

(define (port-folds-case? p)
  (io/port-folds-case? p))

(define (port-folds-case! p bool)
  (io/port-folds-case! p bool))

(define (open-input-file filename)
  (open-file-input-port filename
                        (file-options)
                        'block
                        (native-transcoder)))

(define (open-output-file filename)
  (open-file-output-port filename
                         (file-options)
                         'block
                         (native-transcoder)))

(define (console-input-port)
  ((console-input-port-factory)))

(define (console-output-port)
  ((console-output-port-factory)))

(define (console-error-port)
  ((console-error-port-factory)))

(define console-input-port-factory
  (make-parameter "console-input-port-factory" 
                  console-io/console-input-port
                  procedure?))

(define console-output-port-factory
  (make-parameter "console-output-port-factory"
                  console-io/console-output-port
                  procedure?))

(define console-error-port-factory
  (make-parameter "console-error-port-factory"
                  console-io/console-error-port
                  procedure?))

(define (open-input-string s)
  (string-io/open-input-string s))

(define (open-output-string)
  (string-io/open-output-string))

(define (get-output-string port)
  (string-io/get-output-string port))

(define (reset-output-string port)
  (string-io/reset-output-string port))

(define (open-input-bytevector s)
  (bytevector-io/open-input-bytevector s))

(define (open-output-bytevector)
  (bytevector-io/open-output-bytevector))

(define (open-input/output-bytevector bv)
  (bytevector-io/open-input/output-bytevector bv))

(define (get-output-bytevector port)
  (bytevector-io/get-output-bytevector port))

(define (reset-output-bytevector port)
  (bytevector-io/reset-output-bytevector port))

(define (close-input-port p) 
  (cond ((input-port? p)
         (io/close-port p))
        ((not (output-port? p)) ; HACK: port is closed
         (unspecified))
        (else
         (error "close-input-port: not an input port: " p)
         #t)))

(define (close-output-port p)
  (cond ((output-port? p)
         (io/close-port p))
        ((not (input-port? p)) ; HACK: port is closed
         (unspecified))
        (else
         (error "close-output-port: not an output port: " p)
         #t)))

(define (flush-output-port . rest)
  (cond ((null? rest)
         (io/flush (current-output-port)))
        ((null? (cdr rest))
         (io/flush (car rest)))
        (else
         (error "flush-output-port: too many arguments.")
         #t)))

(define (call-with-input-file file proc)
  (call-with-port (open-input-file file) proc))

(define (call-with-output-file file proc)
  (call-with-port (open-output-file file) proc))

(define (call-with-input-string str proc)
  (let ((port (open-input-string str)))
    (let ((r (proc port)))
      (close-input-port port)
      r)))

(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (let ((str (get-output-string port)))
      (close-output-port port)
      str)))

(define (call-with-output-bytevector proc)
  (let ((port (open-output-bytevector)))
    (proc port)
    (let ((str (get-output-bytevector port)))
      (close-output-port port)
      str)))

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port))
    (thunk)))

(define (with-output-to-port port thunk)
  (parameterize ((current-output-port port))
    (thunk)))

(define (with-input-from-file fn thunk)
  (call-with-input-file fn
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-file fn thunk)
  (call-with-output-file fn
    (lambda (p)
      (with-output-to-port p thunk))))

(define (with-input-from-string str thunk)
  (call-with-input-string str
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (p)
      (with-output-to-port p thunk))))

; Close-open-files is useful for (interactive) error recovery.
; FIXME: this actually closes all ports, so it's pretty drastic.

(define (close-open-ports)
  (file-io/close-open-files))

(define (close-open-files)
  (close-open-ports))

; The following are useful extensions for dealing with the file system.

(define (file-modification-time filename)
  (file-io/file-modification-time filename))

; Maybe this should use >= instead?  Usually what we actually want is
; file-not-older?.
(define (file-newer? f1 f2)
  (let ((t1 (file-modification-time f1))
        (t2 (file-modification-time f2)))
    (let loop ((i 0))
      (cond ((= i (vector-length t1)) #f)
            ((= (vector-ref t1 i) (vector-ref t2 i))
             (loop (+ i 1)))
            (else
              (> (vector-ref t1 i) (vector-ref t2 i)))))))

(define (file-exists? filename)
  (file-io/file-exists? filename))

(define (relative-path-string? filename)
  (file-io/relative-path-string? filename))

(define (absolute-path-string? filename)
  (file-io/absolute-path-string? filename))

(define (rename-file from to)
  (file-io/rename-file from to))

(define (delete-file filename)
  (file-io/delete-file filename))

(define port-position io/port-position)

; eof
