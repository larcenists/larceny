; Simple 'text' data type -- an expandable string
;
; 2002-03-18 / lth
; Trivial implementation, pretty well tested.

; A text is just an expandable string with some convenient operations
; on it.  Nevertheless, the string has a definite length and must be
; extended either by inserting data into it or appending data to it,
; one cannot simply reference it beyond its current bounds.

(require 'string)

(define *text-tag* (list '#(text)))

(define (make-text . rest)
  (let ((init (if (null? rest) "" (string-copy (car rest)))))
    (list *text-tag* init)))

(define (text? obj)
  (and (pair? obj)
       (eq? (car obj) *text-tag*)))

(define (text-length text)
  (string-length (cadr text)))

(define (text-append! text x)
  (text-insert! text (text-length text) x))

(define (text-insert! text pos x)
  (cond ((char? x)
	 (text-insert! text pos (string x)))
	((text? x)
	 (text-insert! text pos (cadr text)))
	((string? x)
	 (let ((t (cadr text)))
	   (set-car! (cdr text)
		     (string-append (substring t 0 pos)
				    x
				    (substring t pos (string-length t))))
	   (unspecified)))
	(else
	 (error "text-insert! requires a text, a string, or a character."))))

(define (text-ref text loc)
  (string-ref (cadr text) loc))

(define (text-set! text loc c)
  (string-set! (cadr text) loc c))

(define (text-substring text from to)
  (substring (cadr text) from to))

(define (text-substring-set! text loc subs)
  (let ((s (cadr text))
	(l (string-length subs)))
    (do ((i 0   (+ i 1))
	 (j loc (+ j 1)))
	((= i l) (unspecified))
      (string-set! s j (string-ref subs i)))))

(define (text-substring-delete! text from to)
  (let ((t (cadr text)))
    (set-car! (cdr text)
	      (string-append (substring t 0 from)
			     (substring t to (string-length t))))
    (unspecified)))

(define (text-search text s . rest)
  (let ((start (if (null? rest) 0 (car rest))))
    (string-search (cadr text) s start)))

; The underlying text is represented as some number of strings.
; TEXT-FOR-EACH calls 'proc' once for each string, in order, passing
; a pointer to the string and the first and last valid character in
; the string.  This is potentially much more efficient than using
; text-substring when implementing eg searching or saving.

(define (text-for-each text proc)
  (proc (cadr text) 0 (text-length text)))

; eof


