; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Useful I/O procedures.

; Read the next line and return it as a string, with newline stripped.

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

; Read characters into the string 'buf' to fill [start..end) and return
; the number of characters read.  If eof was reached before any
; characters were read, return #<eof>.

(define (read-substring buf start end . rest)
  (let ((port (cond ((null? rest) (current-input-port))
		    ((null? (cdr rest)) (car rest))
		    (else (error "read-substring: too many arguments.")))))
    (let loop ((i start))
      (if (= i end)
	  (- end start)
	  (let ((c (read-char port)))
	    (if (eof-object? c)
		(if (= i start)
		    c
		    (- i start))
		(begin (string-set! buf i c)
		       (loop (+ i 1)))))))))

; Write characters from the string 'buf' in positions [start..end).
; Returns something unspecified.

(define (write-substring buf start end . rest)
  (let ((port (cond ((null? rest) (current-output-port))
		    ((null? (cdr rest)) (car rest))
		    (else (error "write-substring: too many arguments.")))))
    (do ((i start (+ i 1)))
	((= i end))
      (write-char (string-ref buf i) port))))

; Wrapper around the standard function that signals an error if the file
; does not exist.

(define file-modification-time
  (let ((file-modification-time file-modification-time))
    (lambda (fn)
      (or (file-modification-time fn)
	  (error "file-modification-time: \"" fn "\" does not exist.")))))

(define (file-newer? f1 f2)
  (let ((t1 (file-modification-time f1))
	(t2 (file-modification-time f2)))
    (let loop ((i 0))
      (cond ((= i (vector-length t1)) #f)
	    ((= (vector-ref t1 i) (vector-ref t2 i))
	     (loop (+ i 1)))
	    (else
	     (> (vector-ref t1 i) (vector-ref t2 i)))))))

; eof
