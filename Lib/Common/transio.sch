; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Transcript I/O ports.

(define *transcript-stack* '())		; Stack of (echo in out)

(define (transcript-io/push filename)

  (define (flush-input-port in)		; A bit of a hack.
    (if (char-ready? in)
	(let ((x (peek-char in)))
	  (if (char-whitespace? x)
	      (begin (read-char in)
		     (flush-input-port in))))))

  (let* ((echo (open-output-file filename))
	 (in   (current-input-port))
	 (out  (current-output-port))
	 (transcript-input 
	  (make-transcript-input-port echo in filename))
	 (transcript-output 
	  (make-transcript-output-port echo out filename)))
    (flush-output-port out)
    (flush-input-port in)
    (current-input-port transcript-input)
    (current-output-port transcript-output)
    (call-without-interrupts
      (lambda ()
	(set! *transcript-stack*
	      (cons (list echo in out) *transcript-stack*))))))

(define (transcript-io/pop)
  (let ((x (call-without-interrupts
	     (lambda ()
	       (if (null? *transcript-stack*)
		   #f
		   (let ((x (car *transcript-stack*)))
		     (set! *transcript-stack* (cdr *transcript-stack*))
		     x))))))
    (if (not x)
	(error "transcript-off: no transcripts in progress."))
    (let ((echo (car x))
	  (in   (cadr x))
	  (out  (caddr x)))
      (close-output-port echo)
      (current-output-port out)
      (current-input-port in))))

; Port datum is #(tag echo filename input-or-output).

(define (make-transcript-input-port echo input fn)
  (io/make-port transcript-io/ioproc 
		(vector 'transcript-output-port echo fn input) 'input))

(define (make-transcript-output-port echo output fn)
  (io/make-port transcript-io/ioproc 
		(vector 'transcript-input-port echo fn output) 'output 'flush))

; BUG: The handling of `name' is technically wrong because it returns
; the name of the transcript echo file rather than the underlying file.

(define (transcript-io/ioproc op)
  (case op
    ((read)   transcript-io/fill-buffer)
    ((write)  transcript-io/flush-buffer)
    ((close)  transcript-io/close)
    ((ready?) (lambda (data) (char-ready? (vector-ref data 3))))
    ((name)   (lambda (data) (vector-ref data 2)))
    (else
     (error "transcript-io/ioproc: illegal operation: " op))))

(define (transcript-io/close data)
  (let ((in-or-out (vector-ref data 3)))
    (if (input-port? in-or-out)
	(close-input-port in-or-out)
	(close-output-port in-or-out))))

; Here we must read entire lines for the transcript to look anything like
; what the user sees.
;
; FIXME: if we run out of buffer space, then we can't read an entire line.
; The buffer is 1024 characters by default and the problem should appear
; rarely, if ever.
;
; FIXME: if the read _fails_ we should close the port.  For now we check
; the ports before each read-char.

(define (transcript-io/fill-buffer data buffer)
  (let ((echo (vector-ref data 1))
	(in   (vector-ref data 3)))
    (if (or (vector-like-ref echo port.error?)
	    (not (vector-like-ref in port.input?))
	    (vector-like-ref in port.error?))
	'error
	(let ((limit (string-length buffer)))
	  (let loop ((i 0))
	    (if (= i limit)
		i
		(let ((c (read-char in)))
		  (if (eof-object? c)
		      i
		      (begin
			(write-char c echo)
			(string-set! buffer i c)
			(if (char=? c #\newline)
			    (+ i 1)
			    (loop (+ i 1))))))))))))

; FIXME: if the write to echo _fails_ we should close the port.  For now
; we check the ports before we write.

(define (transcript-io/flush-buffer data buffer count)
  (let ((echo (vector-ref data 1))
	(out  (vector-ref data 3)))
    (if (or (vector-like-ref echo port.error?)
	    (not (vector-like-ref out port.output?))
	    (vector-like-ref out port.error?))
	'error
	(do ((i 0 (+ i 1)))
	    ((= i count)
	     (flush-output-port out)
	     'ok)
	  (write-char (string-ref buffer i) out)
	  (write-char (string-ref buffer i) echo)))))

; eof
