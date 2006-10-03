; Useful input/output procedures.
;
; $Id$

; READ-LINE does not allow the client to distinguish between lines 
; that are terminated by newline and lines that are terminated by EOF.
; This is a feature for most applications (but not all)
;
; FIXME: for long lines, using string buffers will be much more space
; efficient.

; (read-line) == (read-line (current-input-port))
; (read-line <input-port>)  =>  <next sequence of non-newline chars from port>

(define (read-line . rest)

  (define (finish l k)
    (let ((s (make-string k)))
      (do ((i (- k 1) (- i 1))
	   (l l (cdr l)))
	  ((< i 0) s)
	(string-set! s i (car l)))))

  (define (loop p l k)
    (let ((c (read-char p)))
      (cond ((eof-object? c)
	     (if (null? l)
		 c
		 (finish l k)))
	    ((char=? c #\newline)
	     (finish l k))
	    (else
	     (loop p (cons c l) (+ k 1))))))

  (if (null? rest)
      (loop (current-input-port) '() 0)
      (loop (car rest) '() 0)))

(define (read-file-as-lines file)
  (call-with-input-file file
    (lambda (in)
      (let loop ((l '()))
	(let ((line (read-line in)))
	  (if (eof-object? line)
	      (reverse l)
	      (loop (cons line l))))))))

(define (slurp fn-or-port)
  (define (slurp-port in)
    (do ((cs '()            (cons c cs))
	 (c  (read-char in) (read-char in)))
	((eof-object? c)
	 (list->string (reverse cs)))))
  (if (string? fn-or-port)
      (call-with-input-file fn-or-port slurp-port)
      (slurp-port fn-or-port)))

; FIXME, ought to return bytevector.
; FIXME, should accept a port too.
(define (slurp-binary-file fn)
  (call-with-binary-input-file fn
    (lambda (in)
      (do ((cs '()            (cons c cs))
	   (c  (read-char in) (read-char in)))
	  ((eof-object? c)
	   (reverse cs))))))

(define (with-input-from-string s p)
  (with-input-from-port (open-input-string s) p))

(define (with-output-to-string p)
  (let ((s (open-output-string)))
    (with-output-to-port s p)
    (get-output-string s)))

; eof
