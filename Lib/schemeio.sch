; -*- Scheme -*-
; Larceny run-time system -- generic I/O system.
;
; $Id: schemeio.scm,v 1.5 1992/06/10 09:05:37 lth Exp $
;
; Ports are represented using vectors. A port can be either for input or for
; output, and it can be in one of three states: open, closed, or at 
; end-of-file (output ports are never at eof). En eof object is represented
; as a port in the eof state.

; This I/O system is not very fast; it should be replaced by something
; akin to the I/O system of Modula-3.

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
; count       The number of characters left in the buffer. For output 
;             streams this is the number of chars free; for input 
;             streams, the number of chars unread.
; ptr         The index of the next character in the buffer.
; error       The error handler for i/o errors on this port.
; status      'open, 'eof, or 'closed.
; fd          The unix file descriptor for the port.
; filename    The name of the file, if any.

(define port-size 11)

(define (port.direction p) (vector-like-ref p 0))
(define (port.flush? p) (vector-like-ref p 1))
(define (port.handle p) (vector-like-ref p 2))
(define (port.buffer p) (vector-like-ref p 3))
(define (port.size p) (vector-like-ref p 4))
(define (port.count p) (vector-like-ref p 5))
(define (port.ptr p) (vector-like-ref p 6))
(define (port.error p) (vector-like-ref p 7))
(define (port.status p) (vector-like-ref p 8))
(define (port.fd p) (vector-like-ref p 9))
(define (port.name p) (vector-like-ref p 10))

(define (port-name p)
  (if (port? p)
      (port.name p)
      (error "port-name: not a port: " p)))

(define (port.count! p v) (vector-like-set! p 5 v))
(define (port.ptr! p v) (vector-like-set! p 6 v))
(define (port.status! p v) (vector-like-set! p 8 v))

(define (port.stuff! p c)
  (let ((b (port.buffer p))
	(t (port.ptr p)))
    (string-set! b t c)
    (port.ptr! p (+ (port.ptr p) 1))
    (port.count! p (- (port.count p) 1))))

(define (port.snarf! p)
  (let ((c (port.snarf p)))
    (port.ptr! p (+ (port.ptr p) 1))
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
		   (if (eq? type 'output) size 0)
		   0
		   (lambda r (apply error r))
		   'open
		   fd
		   name)))
    (typetag-set! v sys$tag.port-typetag)
    (set! port-list (cons v port-list))
    v))

(define (make-input-port fd linebuffered? name)
  (make-port 'input
	     fd
	     (lambda (self c)		; dummy
	       #f)
	     (lambda (self)
	       (let ((bytes (sys$read-file (port.fd self)
					   (port.buffer self)
					   (port.size self))))
		 (if (negative? bytes)
		     ((port.error self) 'port-input "Error during read.")
		     (begin (port.count! self bytes)
			    (port.ptr! self 0)))))
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
	       (let ((bytes (sys$write-file (port.fd self)
					    (port.buffer self)
					    (port.ptr self))))
		 (if (< bytes (port.ptr self))
		     ((port.error self) 'port-output "Error during write.")
		     (begin (port.ptr! self 0)
			    (port.count! self (port.size self))))))
	     1024
	     name))

(define (port? x)
  (and (vector-like? x)
       (= (typetag x) sys$tag.port-typetag)))

(define (input-port? p)
  (and (port? p)
       (eq? (port.direction p) 'input)))

(define (output-port? p)
  (and (port? p)
       (eq? (port.direction p) 'output)))

(define (eof-object? p)
  (and (port? p)
       (eq? (port.status p) 'eof)))

(define (open-input-file filename)
  (let ((fd (sys$open-file filename 'input)))
    (if (negative? fd)
	(error 'open-input-file "Unable to open file ~a for input." filename)
	(make-input-port fd #f filename))))

(define (open-output-file filename)
  (let ((fd (sys$open-file filename 'output)))
    (if (negative? fd)
	(error 'open-output-file 
	       "Unable to open file ~a for output." filename)
	(make-output-port fd #f filename))))

(define (close-port direction p)
  (cond ((or (not (port? p)) (not (eq? (port.direction p) direction)))
	 (error 'close "Not an ~a port." direction))
	((eq? (port.status p) 'closed)
	 (error 'close "Port already closed."))
	(else
	 (if (eq? direction 'output)
	     (flush-output-port p))
	 (close-port! p))))

(define (close-port! p)
  (if (and (not (memq p (list stdin stdout)))
	   (not (eq? (port.status p) 'closed)))
      (sys$close-file (port.fd p))))

(define (close-input-port p) (close-port 'input p))
(define (close-output-port p) (close-port 'output p))

(define read-char
  (lambda p

    (define (do-read-char p)
      (cond ((not (input-port? p))
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
		    (not (port? (car p)))))
	   (error 'read-char "Invalid argument."))
	  (else
	   (do-read-char (car p))))))

(define peek-char
  (lambda p

    (define (do-peek-char p)
      (cond ((not (input-port? p))
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
		    (not (port? (car p)))))
	   (error 'peek-char "Invalid argument."))
	  (else
	   (do-peek-char (car p))))))

(define write-char
  (lambda (c . p)

    (define (do-write-char c p)
      (cond ((not (output-port? p))
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
		    (not (port? (car p)))))
	   (error 'write-char "Invalid argument."))
	  (else
	   (do-write-char c (car p))))))

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

(define flush-output-port
  (lambda p

    (define (do-flush-output p)
      (cond ((not (output-port? p))
	     ((port.error p) 'flush-output "Not an output port."))
	    ((not (eq? (port.status p) 'open))
	     ((port.error p) 'flush-output "Not an output port."))
	    (else
	     ((port.handle p) p)
	     #t)))

    (cond ((null? p)
	   (do-flush-output stdout))
	  ((and (not (null? p))
		(or (not (null? (cdr p)))
		    (not (port? (car p)))))
	   (error 'flush-output-port "Invalid argument."))
	  (else
	   (do-flush-output (car p))))))

(define (delete-file filename)
  (if (not (zero? (sys$delete-file filename)))
      #f
      #t))

; Uses 'close-port!' because the I/O system may be in a weird state.
(define (reset-iosystem)
  (for-each close-port! port-list))

(define (close-open-files)
  (flush-output-port stdout)
  (for-each (lambda (p)
	      (if (output-port? p)
		  (close-output-port p)
		  (close-input-port p)))
	    port-list))

(define (with-input-from-port port thunk)
  (let ((old-stdin stdin))
    (set! stdin port)
    (let ((r (thunk)))
      (set! stdin old-stdin)
      r)))

(define (with-output-to-port port thunk)
  (let ((old-stdout stdout))
    (set! stdout port)
    (let ((r (thunk)))
      (set! stdout old-stdout)
      r)))

(define (with-input-from-file fn thunk)
  (call-with-input-file fn
    (lambda (p)
      (with-input-from-port p thunk))))

(define (with-output-to-file fn thunk)
  (call-with-output-file fn
    (lambda (p)
      (with-output-to-port p thunk))))

(define (current-input-port) stdin)

(define (current-output-port) stdout)

(define **eof**
  (let ((eof (make-input-port -1 #f "(**eof**)")))
    (port.status! eof 'eof)
    eof))

(define stdin (make-input-port (sys$open-terminal 'input) #t "(stdin)"))
(define stdout (make-output-port (sys$open-terminal 'output) #t "(stdout)"))

(define (rename-file from to)
  (sys$rename-file from to))

(define (file-exists? fn)
  (sys$file-exists? fn))

; Returns a vector of six elements: #(year month day hour min sec).

(define (file-modification-time fn)
  (sys$file-modification-time fn))

(define (char-ready? port)
  (if (not (input-port? port))
      (error "char-ready?: not an input port: " port))
  (or (> (port.count port) 0)
      (sys$char-ready? (port.fd port))))

; eof
