; These deal with some MacScheme/Chez Scheme incompatibilities.
;
; Chez will barf on a lot of these if optimization is turned on.

(define (unspecified) '***unspecified***)

(define **special-forms** '())

(define some? ormap)
(define every? andmap)
(define (benchmark-mode) #t)
(define (include-source-code) #t)
(define (include-procedure-names) #t)
(define (integrate-usual-procedures) #t)
(define (empty-list-is-true) #t)
(define proper-list? list?)                ; some ancient code in pass1.

(define sort
  (let ((old-sort sort))
    (lambda (list pred)
      (old-sort pred list))))

(define error
  (let ((old-error error))
    (lambda (msg . irritants)
      (display "error: ")
      (display msg)
      (for-each (lambda (x) (display " ") (display x)) irritants)
      (newline)
      (old-error '() ""))))

(define issue-warnings
  (let ((state #f))
    (lambda rest
      (if (not (null? rest))
	  (set! state (car rest)))
      state)))

(define gensym
  (let ((old-gensym gensym)
	(n 0))
    (lambda (x)
      (set! n (+ n 1))
      (string->uninterned-symbol (format "~a~a" x n)))))

(define write-codelist write)

; More or less fixes chez scheme v3.0 "write" bug.
;
;(define (write-codelist x p)
;  (if (list? x)
;      (begin (write-char #\( p)
;	     (for-each (lambda (x)
;			 (write x p)
;			 (display #\space p))
;		       x)
;	     (write-char #\) p))
;      (write x p)))

; Uncomment if using version 3
;
;(define (list? x)
;  (or (null? x) (and (pair? x) (list? (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNIX interface to support 'make'. Defines two procedures:
;
;  (file-exists? string)
;     returns #t if the file exists and #f if not
;
;  (file-modification-time string)
;     returns a timestamp for the file

(load-foreign (string-append chezdir "mtime.o"))

(define file-exists?
  (let ((access
	 ; Using 'access' system call.
	 (foreign-procedure "access" (string integer-32) integer-32)))
    (lambda (f)
      (not (= (access f 0) -1)))))

(define file-modification-time
  ; Using 'mtime' procedure defined in mtime.c.
  (foreign-procedure "mtime" (string) unsigned-32))

; eof
