; Copyright 1999 Lars T Hansen.
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
	 (let* ((rest (apply make-relative-filename components))
		(l    (string-length first))
		(c1   (char=? #\: (string-ref first (- l 1))))
		(c2   (char=? #\: (string-ref rest 0))))
	   (cond ((and c1 c2)
		  (string-append (substring first 0 (- l 1)) rest))
		 ((not (or c1 c2))
		  (string-append first ":" rest))
		 (else
		  (string-append first rest)))))))

; This takes zero or more directory components without directory
; separators and a file name and constructs a filename relative to the
; current directory.

(define (make-relative-filename . components)

  (define (construct l)
    (if (null? (cdr l))
        l
        (cons (car l)
              (cons ":" (construct (cdr l))))))

  (if (null? (cdr components))
      (car components)
      (apply string-append ":" (construct components))))



; This takes one or more directory components and constructs a 
; directory name with proper termination (a crock -- we can finess 
; this later).  Whether the pathname is relative or absolute depends
; on the first component.

(define (pathname-append . components)

  (define (construct l)
    (cond ((null? (cdr l))
           l)
          ((string=? (car l) "")
           (construct (cdr l)))
          ((char=? #\: (string-ref (car l) (- (string-length (car l)) 1)))
           (cons (car l) (construct (cdr l))))
          (else
           (cons (car l)
                 (cons ":" (construct (cdr l)))))))

  (let ((n (if (null? (cdr components))
               (car components)
               (apply string-append (construct components)))))
    (if (not (char=? #\: (string-ref n (- (string-length n) 1))))
        (string-append n ":")
        n)))

; eof
