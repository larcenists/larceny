; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Completely fundamental pathname manipulation.

; This takes zero or more directory components and a file name and
; constructs a filename relative to the current directory.

(define (make-relative-filename . components)

  (define (construct l)
    (if (null? (cdr l))
	l
	(cons (car l)
	      (cons "/" (construct (cdr l))))))

  (if (null? (cdr components))
      (car components)
      (apply string-append (construct components))))

; This takes one or more directory components and constructs a 
; directory name with proper termination (a crock -- we can finess 
; this later).

(define (pathname-append . components)

  (define (construct l)
    (cond ((null? (cdr l))
	   l)
	  ((string=? (car l) "")
	   (construct (cdr l)))
	  (else
	   (cons (car l)
		 (cons "/" (construct (cdr l)))))))

  (let ((n (if (null? (cdr components))
	       (car components)
	       (apply string-append (construct components)))))
    (if (not (char=? #\/ (string-ref n (- (string-length n) 1))))
	(string-append n "/")
	n)))

; eof
