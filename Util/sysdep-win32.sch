; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Completely fundamental pathname manipulation.

; Takes zero or more directories and a filename and appends them, inserting
; the necessary pathname separators.  The first directory in the list, if
; present, has special meaning.  If "" or #f, it denotes the current
; directory.  Otherwise it is taken literally and should already have a
; format that is meaningful on the system.

(define (make-filename first . components)
  (cond ((null? components)
	 first)
	((not first)
	 (apply make-relative-filename components))
	((string=? first "")
	 (apply make-relative-filename components))
	(else
	 (let ((rest (apply make-relative-filename components)))
	   (if (let ((c (string-ref first (- (string-length first) 1))))
		 (or (char=? c #\/) (char=? c #\\)))
	       (string-append first rest)
	       (string-append first "\\" rest))))))


; This takes zero or more directory components without directory
; separators and a file name and constructs a filename relative to the
; current directory.

(define (make-relative-filename . components)

  (define (construct l)
    (if (null? (cdr l))
	l
	(cons (car l)
	      (cons "\\" (construct (cdr l))))))

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
          ((let ((c (string-ref (car l) (- (string-length (car l)) 1))))
	     (or (char=? c #\/) (char=? c #\\)))
           (cons (car l) (construct (cdr l))))
	  (else
	   (cons (car l)
		 (cons "\\" (construct (cdr l)))))))

  (let ((n (if (null? (cdr components))
	       (car components)
	       (apply string-append (construct components)))))
    (if (not (let ((c (string-ref n (- (string-length n) 1))))
	       (or (char=? c #\/) (char=? c #\\))))
	(string-append n "\\")
	n)))

; eof
