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

(define current-input-port
  (make-parameter "current-input-port" #f (lambda (x) (input-port? x))))

(define current-output-port
  (make-parameter "current-output-port" #f (lambda (x) (output-port? x))))

(define (initialize-io-system)
  (io/initialize)
  (file-io/initialize)
  (console-io/initialize)
  (current-input-port (console-input-port))
  (current-output-port (console-output-port))
  (unspecified))

(define (shutdown-io-system)
  (close-open-files)
  (unspecified))

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
          (io/write-bytevector-like string (car rest))
          (begin (error "write-string: too many arguments.")
                 #t))
      (io/write-bytevector-like string (current-output-port))))

(define (input-port? p)
  (io/input-port? p))

(define (output-port? p)
  (io/output-port? p))

(define (port-name p)
  (io/port-name p))

(define (open-input-file filename)
  (file-io/open-file filename 'input 'text))

(define (open-output-file filename)
  (file-io/open-file filename 'output 'text))

(define (open-binary-input-file filename)
  (file-io/open-file filename 'input 'binary))

(define (open-binary-output-file filename)
  (file-io/open-file filename 'output 'binary))

(define (console-input-port)
  ((console-input-port-factory)))

(define (console-output-port)
  ((console-output-port-factory)))

(define console-input-port-factory
  (make-parameter "console-input-port-factory"
                  console-io/console-input-port
                  procedure?))

(define console-output-port-factory
  (make-parameter "console-output-port-factory"
                  console-io/console-output-port
                  procedure?))

(define (open-input-string s)
  (string-io/open-input-string s))

(define (open-output-string)
  (string-io/open-output-string))

(define (get-output-string port)
  (string-io/get-output-string port))

(define (reset-output-string port)
  (string-io/reset-output-string port))

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
  (let ((port (open-input-file file)))
    (let ((r (proc port)))
      (close-input-port port)
      r)))

(define (call-with-output-file file proc)
  (let ((port (open-output-file file)))
    (let ((r (proc port)))
      (close-output-port port)
      r)))

(define (call-with-binary-input-file file proc)
  (let ((port (open-binary-input-file file)))
    (let ((r (proc port)))
      (close-input-port port)
      r)))

(define (call-with-binary-output-file file proc)
  (let ((port (open-binary-output-file file)))
    (let ((r (proc port)))
      (close-output-port port)
      r)))

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

(define (with-input-from-binary-file fn thunk)
  (call-with-binary-input-file fn
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-binary-file fn thunk)
  (call-with-binary-output-file fn
    (lambda (p)
      (with-output-to-port p thunk))))

; Close-open-files is useful for (interactive) error recovery.

(define (close-open-files)
  (file-io/close-open-files))

; The following are useful extensions for dealing with the file system.

(define (file-modification-time filename)
  (file-io/file-modification-time filename))

(define (file-exists? filename)
  (file-io/file-exists? filename))

(define (rename-file from to)
  (file-io/rename-file from to))

(define (delete-file filename)
  (file-io/delete-file filename))

(define port-position io/port-position)

; eof
