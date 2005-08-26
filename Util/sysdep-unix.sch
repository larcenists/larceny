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
  (system (string-append "cat " 
			 (apply string-append 
				(map (lambda (x) (string-append x " "))
				     input-files))
			 " > " 
			 output-file)))

; TODO doc
(define (copy-file/regexp source-path pat target-path)
  (system (string-append "cp " (make-filename source-path pat)
			 "   " target-path)))

; TODO doc
(define (delete-file/regexp target-path pat)
  (system (string-append "rm -f " (make-filename target-path pat))))

; TODO doc
(define (execute-in-directory dir cmd)
  (system (string-append "( cd " dir "; " cmd " )" )))

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
	   (if (char=? #\/ (string-ref first (- (string-length first) 1)))
	       (string-append first rest)
	       (string-append first "/" rest))))))


; This takes zero or more directory components without directory
; separators and a file name and constructs a filename relative to the
; current directory.

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
          ((char=? #\/ (string-ref (car l) (- (string-length (car l)) 1)))
           (cons (car l) (construct (cdr l))))
	  (else
	   (cons (car l)
		 (cons "/" (construct (cdr l)))))))

  (let ((n (if (null? (cdr components))
	       (car components)
	       (apply string-append (construct components)))))
    (if (not (char=? #\/ (string-ref n (- (string-length n) 1))))
	(string-append n "/")
	n)))

;; split-path-string : String -> (values String String)
;; Splits an input path into the container portion and the immediate target.
;; (The container is either a path itself or "")
(define (split-path-string path)
  (let loop ((chars (reverse (string->list path)))
	     (file '()))

    (cond ((null? chars)
	   (values "" (list->string file)))
	  ((char=? (car chars) #\/)
	   (values (list->string (reverse chars))
		   (list->string file)))
	  (else 
	   (loop (cdr chars)
		 (cons (car chars) file))))))

(define (relative-path-string? path)
  (not (char=? #\/ (string-ref path 0))))

; eof
