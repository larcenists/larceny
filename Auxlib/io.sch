; Auxlib/io.sch
; Extra I/O procedures
;
; $Id: io.sch,v 1.1.1.1 1998/11/19 21:52:17 lth Exp $

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
