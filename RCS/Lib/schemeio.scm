; -*- Scheme -*-
;
; Scheme 313 run-time system
; Basic i/o primitives for generic UNIX
;
; $Id: schemeio.scm,v 1.1 91/08/23 22:13:37 lth Exp Locker: lth $
;
; The following procedures must be either integrable or coded up at some lower
; level (currently they are written in MacScheme assembly language):
;
;   (unix$io:open   filename flags)
;   (unix$io:open   filename flags mode)
;   (unix$io:close  filedesc)
;   (unix$io:unlink filename)
;   (unix$io:read   filedesc bytes buffer)
;   (unix$io:write  filedesc bytes buffer)
;
; In all cases, a filename is a string, a mode is an integer, a filedesc is
; an integer, and a buffer is a string.
;
; Opening a file which already exits for mail will cause the old file
; to be silently truncated.
;
; Ports are represented using vectors. A port can be either for input or for
; output, and it can be in one of three states: open, closed, or at 
; end-of-file (output ports are never at eof). En eof object is represented
; as a port in the eof state.

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

(define port? #f)              ; (port? <obj>)                --> <bool>
(define flush-output-port #f)  ; (flush-ouptut-port <oport>)  --> <unspecified>
(define delete-file #f)        ; (delete-file <string>)       --> <bool>
(define reset-iosystem #f)     ; (reset-iosystem)             --> <unspecified>

(let ()

  ; Various UNIX i/o parameters. The values are taken from the header
  ; for SunOS 4.1.1; at some point we need to find a scheme for generating
  ; these values automatically.

  (define unix$io:O_RDONLY 0)             ; mode for opening a file for reading
  (define unix$io:O_WRONLY 1)             ; ditto for writing
  (define unix$io:O_RDWR   2)             ; ditto for both
  (define unix$io:O_CREAT  #x200)         ; create file if !existing (output)
  (define unix$io:O_TRUNC  #x400)         ; truncate if existing (output)
  (define unix$io:create-mode #o666)      ; default mode for new files

  ; These never change.

  (define unix$io:stdin 0)                ; file descriptor for stdin
  (define unix$io:stdout 1)               ; ditto for stdout
  (define unix$io:stderr 2)               ; ditto for stderr (unused)

  ; various objects in the i/o system

  (define stdin #f)                       ; default input stream
  (define stdout #f)                      ; default output stream
  (define port-list '())                  ; list of open ports.

  ; A port is an object with the following slots:
  ;
  ; Name        Value
  ; ----------  -----------------
  ; direction   'input or 'output
  ; flush?      A predicate which (for output streams) decides whether the
  ;             stream should be flushed or not. Takes two arguments: the
  ;             port object and the character which was just placed in the
  ;             output buffer.
  ; handle      A procedure which reads more data (for input ports) and 
  ;             writes the buffer (for output ports). Takes one argument:
  ;             the port object.
  ; buffer      A string.
  ; size        The allocated size of the buffer (length of the string).
  ; count       The number of characters left in the buffer.
  ; ptr         The index of the next character in the buffer.
  ; error       The error handler for i/o errors on this port.
  ; status      'open, 'eof, or 'closed.
  ; fd          The unix file descriptor for the port.
  ; filename    The name of the file, if any.

  (define port-size 11)

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
  (define (port.name p) (vector-ref p 10))

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

  (define (make-port type fd flush? handle size name)
    (let ((v (vector type
		     flush?
		     handle
		     (make-string size)
		     size
		     0
		     0
		     error
		     'open
		     fd
		     name)))
      ; (vector-tag-set! v 'port)
      (set! port-list (cons v port-list))
      v))

  (define (make-input-port fd linebuffered? name)
    (make-port 'input
	       fd
	       (lambda (self c)        ; dummy
		 #f)
	       (lambda (self)
		 (let ((bytes (unix$io:read (port.fd self)
					    (port.buf self)
					    (port.size self))))
		   (if (negative? bytes)
		       ((port.error self) 'port-input "Error during read.")
		       (begin (port.count! bytes)
			      (port.ptr! 0)))))
	       1024
	       name))

  (define (make-output-port fd linebuffered? name)
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
	       1024
	       name))

  ;

  (define (%port? x)
    (and (vector? x)
	 (= (vector-length x) port-size)
	 (or (eq? (vector-ref x 0) 'input) (eq? (vector-ref x 0) 'output))))

  (define (%input-port? p)
    (and (%port? p)
	 (eq? (port.direction p) 'input)))

  (define (%output-port p)
    (and (%port? p)
	 (eq? (port.direction p) 'output)))

  (define (%eof-object? p)
    (and (%port? p)
	 (eq? (port.status p) 'eof)))

  (define (%open-input-file filename)
    (let ((fd (unix$io:open filename unix$io:O_RDONLY)))
      (if (negative? fd)
	  (error 'open-input-file "Unable to open file ~a for input." filename)
	  (make-input-port fd #f filename))))

  (define (%open-output-file filename)
    (let ((fd (unix$io:open filename
			    (+ unix$io:O_WRONLY
			       unix$io:O_CREAT
			       unix$io:O_TRUNC)
			    unix$io:create-mode)))
      (if (negative? fd)
	  (error 'open-output-file 
		 "Unable to open file ~a for output." filename)
	  (make-output-port fd #f filename))))

  (define (%close-port direction p)
    (cond ((or (not (port? p)) (not (eq? (port.direction p) direction)))
	   (error 'close "Not an ~a port." direction))
	  ((eq? (port.status p) 'closed)
	   (error 'close "Port already closed."))
	  (else
	   (close-port! p))))

  (define (close-port! p)
    (if (and (not (memq p (list stdin stdout)))
	     (not (eq? (port.status p) 'closed)))
	(unix$io:close (port.fd p))))

  (define (%read-char . p)

    (define (do-read-char p)
      (cond ((not (%input-port? p))
	     ((port.error p) 'read-char "Not an input port."))
	    ((eq? (port.status p) 'eof)
	     p)
	    ((not (eq? (port.status p) 'open))
	     ((port.error p) 'read-char "Port is not open."))
	    (else
	     (if (zero? (port.count p))
		 ((port.handle p) p))
	     (if (zero? (port.count p))
		 (begin (port.status! p 'eof)
			p)
		 (port.snarf! p)))))

    (cond ((null? p)
	   (do-read-char stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'read-char "Invalid argument."))
	  (else
	   (do-read-char (car p)))))

  (define (%peek-char . p)

    (define (do-peek-char p)
      (cond ((not (%input-port? p))
	     ((port.error p) 'peek-char "Not an input port."))
	    ((eq? (port.status p) 'efo)
	     p)
	    ((not (eq? (port.status p) 'open))
	     ((port.error p) 'peek-char "Port is not open."))
	    (else
	     (if (zero? (port.count p))
		 ((port.handle p) p))
	     (if (zero? (port.count p))
		 (begin (port.status! p 'eof)
			p)
		 (port.snarf p)))))

    (cond ((null? p)
	   (do-peek-char stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'peek-char "Invalid argument."))
	  (else
	   (do-peek-char (car p)))))

  (define (%write-char c . p)

    (define (do-write-char c p)
      (cond ((not (%output-port? p))
	     ((port.error p) 'write-char "Not an output port."))
	    ((not (eq? (port.status p) 'open))
	     ((port.error p) 'write-char "Port is not open."))
	    (else
	     (port.stuff! p c)
	     (if (or (zero? (port.count p))
		     ((port.flush? p) p c))
		 ((port.handle p) p))
	     #t)))

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

    (define (do-flush-output p)
      (cond ((not (%output-port? p))
	     ((port.error p) 'flush-output "Not an output port."))
	    ((not (eq? (port.status p) 'open))
	     ((port.error p) 'flush-output "Not an output port."))
	    (else
	     ((port.handle p) p)
	     #t)))

    (cond ((null? p)
	   (do-flush-output stdin))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (%port? (car p)))))
	   (error 'flush-output-port "Invalid argument."))
	  (else
	   (do-flush-output (car p)))))

  (define (%delete-file filename)
    (if (not (zero? (unix$io:unlink filename)))
	#f
	#t))

  (define (%reset-iosystem)
    (for-each (lambda (p)
		(close-port! p))
	      port-list))

  ; Setup all the globals

  (set! stdin (make-input-port unix$io:stdin #t "(stdin)"))
  (set! stdout (make-output-port unix$io:stdout #t "(stdout)"))

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
