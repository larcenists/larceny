; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File I/O.

($$trace "fileio")

; List of open files: assoc list on (file-data . port)

(define *file-io/files* '())

(define (file-io/initialize)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* '()))))

(define (file-io/remember-file data p)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* 
           (cons (cons data p) *file-io/files*)))))

(define (file-io/forget-file data)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* 
           (remq! (assq data *file-io/files*) *file-io/files*)))))

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
  (let ((r (osdep/read-file fd buffer (bytevector-like-length buffer))))
    (cond ((< r 0) 'error)
          ((= r 0) 'eof)
          (else r))))

(define (file-io/write-bytes fd buffer n offset)
  (let ((k (osdep/write-file4 fd buffer n offset)))
    (cond ((<= k 0) 'error)
          ((= k n)  'ok)
          (else (file-io/write-bytes fd buffer (- n k) (+ offset k))))))

(define (file-io/open-file filename . modes)
  (let* ((io-mode (if (memq 'input modes) 'input 'output))
         (tx-mode (if (memq 'binary modes) 'binary 'text))
         (fd      (osdep/open-file filename io-mode tx-mode))) 
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (io/make-port file-io/ioproc data io-mode tx-mode)))
          (file-io/remember-file data p)
          p)
        (begin (error "Unable to open file " filename " for " io-mode)
               #t))))

(define (file-io/close-file data)
  (let ((r (osdep/close-file (file-io/fd data))))
    (file-io/forget-file data)
    (if (< r 0) 
        'error
        'ok)))

(define (file-io/close-open-files)
  (let ((files (call-without-interrupts    ; *file-io/files*
                (lambda ()
                  (let ((x *file-io/files*))
                    (set! *file-io/files* '())
                    x)))))
    (do ((l files (cdr l)))
        ((null? l) (unspecified))
      (io/close-port (cdar l)))))

(define (file-io/file-modification-time filename)
  (osdep/file-modification-time filename))

(define (file-io/file-exists? filename)
  (osdep/file-exists? filename))

(define (file-io/relative-path-string? filename)
  (osdep/relative-path-string? filename))

(define (file-io/absolute-path-string? filename)
  (osdep/absolute-path-string? filename))

(define (file-io/rename-file from to)
  (osdep/rename-file from to))

(define (file-io/delete-file filename)
  (osdep/delete-file filename))

; eof
