; Useful file system procedures.
; 2001-11-16 / lth
;
; $Id$

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

