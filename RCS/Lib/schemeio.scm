; -*- Scheme -*-
;
; Scheme 313 run-time system
; Basic i/o primitives for generic UNIX
;
; $Id$
;
; The following procedures must be either integrable or coded up at some lower
; level (currently they are written in MacScheme assembly language):
;
;   (unix$io:open   filename mode)
;   (unix$io:close  filedesc)
;   (unix$io:creat  filename mode)
;   (unix$io:unlink filename)
;   (unix$io:read   filedesc bytes buffer)
;   (unix$io:write  filedesc bytes buffer)
;
; In all cases, a filename is a string, a mode is an integer, a filedesc is
; an integer, and a buffer is a string.

; This is the basic i/o system, less `read' and `write'.

(define input-port? #f)
(define output-port? #f)
(define eof-object? #f)
(define open-input-file #f)
(define open-output-file #f)
(define close-input-port #f)
(define close-output-port #f)
(define current-input-port #f)
(define current-output-port #f)
(define read-char #f)
(define peek-char #f)
(define write-char #f)
(define call-with-input-file #f)
(define call-with-output-file #f)

; The following are extenstions to the standard i/o system.

(define port? #f)
(define flush-output-port #f)
(define delete-file #f)
(define reset-iosystem #f)

(let ()

  ; Various unix i/o parameters

  (define unix$io:read-mode 0)            ; mode for opening a file for reading
  (define unix$io:write-mode 0)           ; ditto for writing
  (define unix$io:read/write-mode 0)      ; ditto for both
  (define unix$io:stdin 0)                ; file descriptor for stdin
  (define unix$io:stdout 1)               ; ditto for stdout
  (define unix$io:stderr 2)               ; ditto for stderr (unused)

  ; various objects in the i/o system

  (define stdin #f)                       ; default input stream
  (define stdout #f)                      ; default output stream
  (define fd-list '())                    ; list of open fd's.

  ; A port is an object with the following slots:
  ;
  ; Name        Value
  ; ----------  -----------------
  ; direction   'input or 'output
  ; flush?      Empty for input-ports. For output ports, a procedure which
  ;             determines whether the buffer should be flused. Takes two
  ;             arguments, the character and the size of the buffer after the
  ;             insertion of the character, and returns a boolean.
  ; handle      A thunk which reads more data (for input ports) and 
  ;             writes the buffer (for output ports).
  ; buffer      A string.
  ; size        The allocated size of the buffer (length of the string).
  ; count       The number of characters left in the buffer.
  ; ptr         The index of the next character in the buffer.
  ; error       The error handler for i/o errors on this port.
  ; status      'open, 'eof, or 'closed.
  ; fd          The unix file descriptor for the port.

  (define port-size 10)

  (define (port.direction p) (vector-ref p 0))
  (define (port.flush? p) (vector-ref p 1))
  (define (port.handle p) (vector-ref p 2))
  (define (port.buffer p) (vector-ref p 3))
  (define (port.size p) (vector-ref p 4))
  (define (port.count p) (vector-ref p 5))
  (define (port.ptr p) (vector-ref p 6))
  (define (port.error p) (vector-ref p 7))
  (define (port.status p) (vector-ref p 8))
  (define (port.fd p) (vector-ref p 9))

  (define (port.count! p v) (vector-set! p 5 v))
  (define (port.ptr! p v) (vector-set! p 6 v))
  (define (port.status! p v) (vector-set! p 8 v))

  (define (port.stuff! p c)
    (string-set! (port.buffer p) (port.ptr p) c)
    (port.ptr! p (+ (port.ptr p) 1))
    (port.count! p (- (port.count p) 1)))

  (define (port.snarf! p)
    (let ((c (port.snarf p)))
      (port.ptr! p (- (port.ptr p) 1))
      (port.count! p (- (port.count p) 1))
      c))

  (define (port.snarf p)
    (string-ref (port.buffer p) (port.ptr p)))

  (define (make-port fd type flush? handle size)
    (set! fd-list (cons fd fd-list))
    (let ((v (vector type
		     flush?
		     handle
		     (make-string size)
		     size
		     0
		     0
		     error
		     'open
		     fd)))
      ; (vector-tag-set! v 'port)
      v))

  (define (make-input-port fd interactive?)
    (make-port 'input
	       fd
	       (lambda (self c)
		 #f)
	       (lambda (self)
		 (let ((bytes (unix$io:read (port.fd self)
					    (port.buf self)
					    (port.size self))))
		   (if (negative? bytes)
		       ((port.error self) 'port-input "Error during read.")
		       (begin (port.count! bytes)
			      (port.ptr! 0)))))
	       1024))

  (define (make-output-port fd linebuffered?)
    (make-port 'output
	       fd
	       (if linebuffered?
		   (lambda (self c)
		     (char=? c #\newline))
		   (lambda (self c)
		     #f))
	       (lambda (self)
		 (let ((bytes (unix$io:write (port.fd self)
					     (port.buf self)
					     (port.ptr self))))
		   (if (< bytes (port.ptr self))
		       ((port.error self) 'port-output "Error during write.")
		       (begin (port.ptr! self 0)
			      (port.count! self (port.size self))))))
	       1024))

  (define (%port? x)
    (and (vector? x)
	 (= (vector-length x) port-size)
	 (or (eq? (vector-ref x 0) 'input) (eq? (vecot-ref x 0) 'output))))

  (define (do-read-char p)
    (if (not (%input-port? p))
	((port.error p) 'read-char "Not an input port.")
	(begin (if (zero? (port.count p))
		   ((port.handle p) p))
	       (port.snarf! p))))

  (define (do-peek-char p)
    (if (not (%input-port? p))
	((port.error p) 'peek-char "Not an input port.")
	(begin (if (zero? (port.count p))
		   ((port.handle p) p))
	       (port.snarf p))))

  (define (do-write-char c p)
    (if (not (%output-port? p))
	((port.error p) 'write-char "Not an output port.")
	(begin (port.stuff! p c)
	       (if (or (zero? (port.count p))
		       ((port.flush? p) p c))
		   ((port.handle p) p))
	       #t)))

  (define (do-flush-output p)
    (if (not (%output-port? p))
	((port.error) p 'flush-output "Not an output port.")
	(begin ((port.handle p) p)
	       #t)))

  (define (%input-port? p)
    (and (%port? p)
	 (eq? (port.direction p) 'input)))

  (define (%output-port p)
    (and (%port? p)
	 (eq? (port.direction p) 'output)))

  ; An eof-object is a port with the status field set to 'eof.

  (define (%eof-object? p)
    (and (%port? p)
	 (eq? (port.status p) 'eof)))

  (define (%open-input-file filename)
    (let ((fd (unix$io:open filename unix$io:read-mode)))
      (if (negative? fd)
	  (error 'open-input-file "Unable to open file.")
	  (make-input-port fd #f))))

  (define (%open-output-file filename)
    (let ((fd (unix$io:create filename unix$io:write-mode)))
      (if (negative? fd)
	  (error 'open-output-file "Unable to create file.")
	  (make-output-port fd #f))))

  (define (%close-port direction p)
    (cond ((or (not (port? p)) (not (eq? (port.direction p) direction)))
	   (error 'close "Not an ~a port." direction))
	  ((eq? (port.status p) 'closed)
	   (error 'close "Port already closed."))
	  (else
	   (unix$io:close (port.fd p)))))

  (define (%read-char . p)
    (cond ((null? p)
	   (do-read-char stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'read-char "Invalid argument."))
	  (else
	   (do-read-char (car p)))))

  (define (%peek-char . p)
    (cond ((null? p)
	   (do-peek-char stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'peek-char "Invalid argument."))
	  (else
	   (do-peek-char (car p)))))

  (define (%write-char c . p)
    (cond ((not (char? c))
	   (error 'write-char "Not a character."))
	  ((null? p)
	   (do-write-char c stdout))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'write-char "Invalid argument."))
	  (else
	   (do-write-char c (car p)))))

  (define (%call-with-input-file file p)
    (let ((port (%open-input-file file)))
      (let ((r (p port)))
	(%close-input-port port)
	r)))

  (define (%call-with-output-file file p)
    (let ((port (%open-output-file file)))
      (let ((r (p port)))
	(%close-output-port p)
	r)))

  (define (%flush-output-port . p)
    (cond ((null? p)
	   (do-flush-output stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'flush-output-port "Invalid argument."))
	  (else
	   (do-flush-output (car p)))))

  (define (%delete-file file)
    (if (not (zero? (unix$io:unlink file)))
	#f
	#t))

  (define (%reset-iosystem)
    '())

  ; Setup all the globals

  (set! stdin (make-input-port unix$io:stdin #t))
  (set! stdout (make-output-port unix$stdout #t))

  (set! port? %port?)
  (set! eof-object? %eof-object)
  (set! input-port? %input-port?)
  (set! outurt-port? %output-port)
  (set! open-input-file %open-input-file)
  (set! open-output-file %open-output-file)
  (set! close-input-port (lambda (p) (%close-port 'input p)))
  (set! close-output-port (lambda (p) (%close-port 'output p)))
  (set! current-input-port (lambda () stdin))
  (set! current-output-port (lambda () stdout))
  (set! read-char %read-char)
  (set! peek-char %peek-char)
  (set! write-char %write-char)
  (set! call-with-input-file %call-with-input-file)
  (set! call-with-output-file %call-with-output-file)
  (set! flush-output-port %flush-output-port)
  (set! delete-file %delete-file)
  (set! reset-iosystem %reset-iosystem)
  '())


	
