; Lib/stdio.sch
; Larceny -- standard Scheme I/O library
;
; $Id: stdio.sch,v 1.3 1997/05/15 00:42:10 lth Exp lth $
;
; Procedures we could add:
;   file-port?
;   console-port?
;   closed-port?
;
; Features we could add:
;   string ports
;
; Notes
;   eof-object? needs to be integrable for good performance.


; First there are private variables and some procedures that depend on them.
; In a multi-threaded system, we must be more sophisticated than this, because
; the current input and output ports are really dynamically bound.

(define *stdin* #f)
(define *stdout* #f)

(define (current-input-port . rest)
  (cond ((null? rest) *stdin*)
	((null? (cdr rest))
	 (let ((old *stdin*)
	       (p   (car rest)))
	   (if (input-port? p)
	       (begin (set! *stdin* p)
		      old)
	       (begin (error "current-input-port: not an input port: " p)
		      #t))))
	(else
	 (error "current-input-port: too many arguments.")
	 #t)))

(define (current-output-port . rest)
  (cond ((null? rest) *stdout*)
	((null? (cdr rest))
	 (let ((old *stdout*)
	       (p   (car rest)))
	   (if (output-port? p)
	       (begin (set! *stdout* p)
		      old)
	       (begin (error "current-output-port: not an output port: " p)
		      #t))))
	(else
	 (error "current-output-port: too many arguments.")
	 #t)))


; The following do not depend on *stdin* and *stdout*.

(define (initialize-io-system)
  (io/initialize)
  (file-io/initialize)
  (console-io/initialize)
  (current-input-port (open-input-console))
  (current-output-port (open-output-console))
  (unspecified))
  
(define (read-char . rest)
  (cond ((null? rest)
	 (io/read-char (current-input-port)))
	((null? (cdr rest))
	 (io/read-char (car rest)))
	(else
	 (error "read-char: too many arguments.")
	 #t)))

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

(define (write-char c . rest)
  (cond ((null? rest)
	 (io/write-char c (current-output-port)))
	((null? (cdr rest))
	 (io/write-char c (car rest)))
	(else
	 (error "write-char: too many arguments.")
	 #t)))

(define (eof-object? p)
  (eq? p (eof-object)))

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

(define (close-input-port p) 
  (if (input-port? p)
      (io/close-port p)
      (begin (error "close-input-port: not an input port: " p)
	     #t)))

(define (close-output-port p)
  (if (output-port? p)
      (io/close-port p)
      (begin (error "close-output-port: not an output port: " p)
	     #t)))

(define (flush-output-port . rest)
  (cond ((null? rest) (io/flush (current-output-port)))
	((null? (cdr rest)) (io/flush (car rest)))
	(else (error "flush-output-port: too many arguments.")
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


; Useful for (interactive) error recovery.

(define (close-open-files)
  (file-io/close-open-files))


; Useful extensions dealing with the file system.

(define (file-modification-time filename)
  (file-io/file-modification-time filename))

(define (file-exists? filename)
  (file-io/file-exists? filename))

(define (rename-file from to)
  (file-io/rename-file from to))

(define (delete-file filename)
  (file-io/delete-file filename))

; Backwards compatibility

(define (reset-iosystem)
  #t)

; eof
