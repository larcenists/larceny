; Lib/stringio.sch
; Larceny -- string I/O ports
;
; $Id$
;
; Compatibility notes:
;
; Our procedures are largely compatible with Chez Scheme, except
; that Chez Scheme resets the string to empty after get-output-string,
; and I don't really see any reason for doing that.
;
; The MIT Scheme string output functions can mostly be implemented in
; terms of ours.


; Offsets in the string port data structure.

(define string-io.type 0)		; Symbol
(define string-io.s 1)			; Input: string; output: list of strings
(define string-io.i 2)			; Input: fixnum; output: garbage

(define (string-io/open-input-string s)
  (if (not (string? s))
      (error "open-input-string: " s " is not a string."))
  (io/make-port string-io/ioproc (vector 'string-input-port s 0) 'input))

(define (string-io/open-output-string)
  (io/make-port string-io/ioproc (vector 'string-output-port '() 0) 'output))

(define (string-io/get-output-string port)
  (if (not (string-output-port? port))
      (error "get-output-string: " port " is not a string output port."))
  (flush-output-port port)
  (let* ((data (vector-like-ref port port.iodata))
	 (s    (vector-ref data string-io.s)))
    (cond ((null? s)
	   "")
	  ((null? (cdr s))
	   (car s))
	  (else
	   (let ((x (apply string-append s)))
	     (vector-set! data string-io.s (list x))
	     x)))))

(define (string-io/ioproc op)
  (case op
    ((read)
     string-io/fill-buffer)
    ((write)
     string-io/flush-buffer)
    ((close)
     (lambda (data) #t))
    ((ready?)
     (lambda (data) #t))
    ((name)
     (lambda (data) "*string*"))
    (else
     (error "string-io/ioproc: illegal operation: " op))))

(define (string-io/fill-buffer data buffer)
  (let ((s (vector-ref data string-io.s))
	(i (vector-ref data string-io.i)))
    (let ((n (min (string-length buffer) (- (string-length s) i))))
      (if (= n 0)
	  'eof
	  (do ((j 0 (+ j 1))
	       (k i (+ k 1)))
	      ((= j n) 
	       (vector-set! data string-io.i (+ i n))
	       n)
	    (string-set! buffer j (string-ref s k)))))))

(define (string-io/flush-buffer data buffer count)
  (vector-set! data string-io.s
	       (append! (vector-ref data string-io.s)
			(list (substring buffer 0 count))))
  'ok)

(define (string-output-port? port)
  (let ((d (vector-like-ref port port.iodata)))
    (and (vector? d)
	 (> (vector-length d) 0)
	 (eq? (vector-ref d 0) 'string-output-port))))

; eof
