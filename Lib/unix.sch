;; Larceny library -- Some Unix primitives
;;
;; History
;;   July 19, 1994 / lth
;;     Created

(define (unix:open filename mode)
  (syscall sys$syscall:open filename mode))

(define (unix:close fd)
  (syscall sys$syscall:close fd))

(define (unix:read fd buffer nbytes)
  (syscall sys$syscall:read fd buffer nbytes))

(define (unix:write fd buffer nbytes)
  (syscall sys$syscall:write fd buffer nbytes))

(define (unix:unlink filename)
  (syscall sys$syscall:unlink filename))

(define (unix:rename oldname newname)
  (syscall sys$syscall:rename oldname newname))

;; Returns a vector of length 6, containing yr month day hr min sec

(define (unix:file-modification-time filename)
  (let ((v (make-vector 6)))
    (syscall sys$syscall:mtime filename v)
    v))

;; #t if file exists

(define (unix:file-exists? filename)
  (zero? (syscall syscall:access filename ...)))

