; Lib/fileio.sch
; Larceny -- file I/O
;
; $Id$

($$trace "fileio")

(define *file-io/files* '())

(define (file-io/initialize)
  (set! *file-io/files* '()))

(define (file-io/remember-file data p)
  (set! *file-io/files* (cons (cons data p) *file-io/files*)))

(define (file-io/forget-file data)
  (define (loop l)
    (cond ((null? l) l)
	  ((eq? (caar l) data) (cdr l))
	  (else (loop (cdr l)))))
  (set! *file-io/files* (loop *file-io/files*)))

(define (file-io/ioproc op)
  (case op
    ((read)
     (lambda (data buffer)
       (file-io/read-bytes (file-io/fd data) buffer)))
    ((write)
     (lambda (data buffer count)
       (file-io/write-bytes (file-io/fd data) buffer count 0)))
    ((close)
     (lambda (data)
       (file-io/close-file data)))
    ((ready?)
     (lambda (data) #t))
    ((name) 
     (lambda (data)
       (file-io/name data)))
    (else 
     (error "file-io/ioproc: illegal operation: " op))))

(define (file-io/data fd name)
  (cons fd name))

(define (file-io/fd datum)
  (car datum))

(define (file-io/name datum)
  (cdr datum))

(define (file-io/read-bytes fd buffer)
  (let ((r (sys$read-file fd buffer (string-length buffer))))
    (cond ((< r 0) 'error)
	  ((= r 0) 'eof)
	  (else r))))

(define (file-io/write-bytes fd buffer n offset)
  (let ((k (sys$write-file4 fd buffer n offset)))
    (cond ((<= k 0) 'error)
	  ((= k n)  'ok)
	  (else (file-io/write-bytes fd buffer (- n k) (+ offset k))))))

(define (file-io/open-file filename mode)
  (let ((fd (sys$open-file filename mode)))
    (if (>= fd 0)
	(let* ((data (file-io/data fd filename))
	       (p    (io/make-port file-io/ioproc data mode)))
	  (file-io/remember-file data p)
	  p)
	#f)))

(define (file-io/open-input-file filename)
  (cond ((file-io/open-file filename 'input))
	(else
	 (error "Open-input-file: Unable to open file for input: "
		filename)
	 #t)))

(define (file-io/open-output-file filename)
  (cond ((file-io/open-file filename 'output))
	(else
	 (error "Open-output-file: unable to open file for output: "
		filename)
	 #t)))

(define (file-io/close-file data)
  (let ((r (sys$close-file (file-io/fd data))))
    (file-io/forget-file data)
    (if (< r 0) 
	'error
	'ok)))

(define (file-io/close-open-files)
  (do ((l *file-io/files* (cdr l)))
      ((null? l)
       (set! *file-io/files* '())
       (unspecified))
    (io/close-port (cdar l))))

(define (file-io/file-modification-time filename)
  (if (string? filename)
      (sys$file-modification-time filename)
      (begin (error "file-modification-time: not a filename: " filename)
	     #t)))

(define (file-io/file-exists? filename)
  (if (string? filename)
      (sys$file-exists? filename)
      (begin (error "file-exists?: not a filename: " filename)
	     #t)))

(define (file-io/rename-file from to)
  (if (and (string? from) (string? to))
      (sys$rename-file from to)
      (begin (error "rename-file: bad arguments: " from " " to)
	     #t)))

(define (file-io/delete-file filename)
  (if (string? filename)
      (if (not (zero? (sys$delete-file filename)))
	  #f
	  #t)
      (begin (error "delete-file: not a filename: " filename)
	     #t)))

; eof
