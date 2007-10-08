; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Completely fundamental pathname manipulation.

; TODO doc

(define (make-directory . components)
  (system (string-append "mkdir " (apply make-filename components))))

; TODO doc

(define (catfiles input-files output-file)
  (delete-file output-file)
  (call-with-output-file output-file
    (lambda (out)
      (for-each (lambda (f)
		  (call-with-input-file f
		    (lambda (in)
		      (do ((c (read-char in) (read-char in)))
			  ((eof-object? c))
			(write-char c out)))))
		input-files))))

; TODO doc

(define (copy-file/regexp source-path pat target-path)
  (system (string-append "copy " (make-filename source-path pat)
			 "     " target-path)))

; TODO doc

(define (delete-file/regexp target-path pat)
  (system (string-append "del " (make-filename target-path pat))))

(define (execute-in-directory dir cmd)
  (with-current-directory dir
    (lambda ()
      (system cmd))))

(define (with-current-directory dir thunk)
  (let ((cdir #f))
    (dynamic-wind
	(lambda ()
	  (set! cdir (current-directory))
	  (current-directory dir))
	thunk
	(lambda ()
	  (set! dir (current-directory))
	  (current-directory cdir)))))

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

;; split-path-string : String -> (values String String)
;; Splits an input path into the container portion and the immediate target.
;; (The container is either a path itself or "")

(define (split-path-string path)
  (let loop ((chars (reverse (string->list path)))
	     (file '()))

    (cond ((null? chars)
	   (values "" (list->string file)))
	  ((char=? (car chars) #\\)
	   (values (list->string (reverse chars))
		   (list->string file)))
	  (else 
	   (loop (cdr chars)
		 (cons (car chars) file))))))

(define (relative-path-string? path)
  (or (< (string-length path) 2)
      (not (char=? #\: (string-ref path 1)))))

(define (absolute-path-string? path)
  (not (relative-path-string? path)))

; eof
