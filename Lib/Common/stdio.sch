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
;
; Features we could add:
;   transcript ports

($$trace "stdio")

; First there are private variables and some procedures that depend on them.
; In a multi-threaded system, we must be more sophisticated than this, 
; because the current input and output ports are really dynamically bound
; (and bound separately on each processor).

(define *stdin* #f)
(define *stdout* #f)

(define (current-input-port . rest)
  (cond ((null? rest) *stdin*)
	((null? (cdr rest))
	 (let ((old *stdin*)
	       (new (car rest)))
	   (if (input-port? new)
	       (begin (set! *stdin* new)
		      old)
	       (begin (error "current-input-port: not an input port: " new)
		      #t))))
	(else
	 (error "current-input-port: too many arguments.")
	 #t)))

(define (current-output-port . rest)
  (cond ((null? rest) *stdout*)
	((null? (cdr rest))
	 (let ((old *stdout*)
	       (new (car rest)))
	   (if (output-port? new)
	       (begin (set! *stdout* new)
		      old)
	       (begin (error "current-output-port: not an output port: " new)
		      #t))))
	(else
	 (error "current-output-port: too many arguments.")
	 #t)))

; Then there is code that does not depend on *stdin* or *stdout*, but
; that use only current-input-port and current-output-port.

(define (initialize-io-system)
  (io/initialize)
  (file-io/initialize)
  (console-io/initialize)
  (current-input-port (open-input-console))
  (current-output-port (open-output-console))
  (unspecified))

(define (shutdown-io-system)
  (close-open-files)
  (unspecified))

; Read-char has been re-coded in MAL for performance; see Lib/malcode.mal.
;
;(define (read-char . rest)
;  (cond ((null? rest)
;	 (io/read-char (current-input-port)))
;	((null? (cdr rest))
;	 (io/read-char (car rest)))
;	(else
;	 (error "read-char: too many arguments.")
;	 #t)))

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
;	 (io/write-char c (current-output-port)))
;	((null? (cdr rest))
;	 (io/write-char c (car rest)))
;	(else
;	 (error "write-char: too many arguments.")
;	 #t)))

(define (write-bytevector-like bvl . rest)
  (cond ((null? rest)
	 (io/write-bytevector-like bvl (current-output-port)))
	((null? (cdr rest))
	 (io/write-bytevector-like bvl (car rest)))
	(else
	 (error "write-bytevector-like: too many arguments.")
	 #t)))

(define (input-port? p)
  (io/input-port? p))

(define (output-port? p)
  (io/output-port? p))

(define (port-name p)
  (io/port-name p))

(define (open-input-file filename)
  (file-io/open-input-file filename))

(define (open-output-file filename)
  (file-io/open-output-file filename))

(define (open-input-console)
  (console-io/open-input-console))

(define (open-output-console)
  (console-io/open-output-console))

(define (open-input-string s)
  (string-io/open-input-string s))

(define (open-output-string)
  (string-io/open-output-string))

(define (get-output-string port)
  (string-io/get-output-string port))

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

(define (with-input-from-port port thunk)
  (let ((old (current-input-port port)))
    (let ((r (thunk)))
      (current-input-port old)
      r)))

(define (with-output-to-port port thunk)
  (let ((old (current-output-port port)))
    (let ((r (thunk)))
      (current-output-port old)
      r)))

(define (with-input-from-file fn thunk)
  (call-with-input-file fn
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-file fn thunk)
  (call-with-output-file fn
    (lambda (p)
      (with-output-to-port p thunk))))

(define (transcript-on filename)
  (transcript-io/push filename)
  ((transcript-event-notifier))
  (unspecified))

(define (transcript-off)
  (transcript-io/pop)
  ((transcript-event-notifier))
  (unspecified))

(define transcript-event-notifier
  (system-parameter 'transcript-event-notifier (lambda () #t)))


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

; eof
