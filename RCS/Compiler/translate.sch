; -*- Scheme -*-
;
; Scheme program which translates the #define lines on its standard input to
; define-const lines on its standard output, and which copies everything
; straight across.
;
; $Id$
;
; BUGS
;   Lines which define a name with no value will make this program break.

(define (main)
  (let loop ((x (readln)))
    (cond ((eof-object? x)
	   #t)
	  ((strncmp x "#define" 7)
	   (display (mangle x)) 
	   (newline)
	   (loop (readln)))
	  (else
	   (display x) 
	   (newline)
	   (loop (readln))))))

(define (string-tr s)
  (let ((v (make-string (string-length s)))
	(l (string-length s)))
    (let loop ((i 0))
      (if (< i l)
	  (begin (string-set! v i (char-trans (string-ref s i)))
		 (loop (+ i 1)))
	  v))))

(define (char-trans c)
  (if (char=? c #\_) #\- (char-downcase c)))

(define (mangle x)

  ; return non-blank string starting at start with the index of the
  ; next char, or #f.

  (define (getfield x start)
    (let loop ((i start) (l (string-length x)))
      (cond ((>= i l)
	     #f)
	    ((char-whitespace? (string-ref x i))
	     (loop (+ i 1) l))
	    (else
	     (let loop1 ((s i) (i i))
	       (cond ((>= i l)
		      (cons (substring x s l) i))
		     ((char-whitespace? (string-ref x i))
		      (cons (substring x s i) i))
		     (else
		      (loop1 s (+ i 1)))))))))

  (let* ((a (getfield x 7))
	 (name (car a))
	 (b (getfield x (cdr a)))
	 (value (car b)))
    (string-append "(define-const " 
		   (string-tr name)
		   " "
		   value
		   " \"" 
		   name
		   "\" #f #f)")))

(define (strncmp a b l)
  (let ((la (string-length a))
	(lb (string-length b)))
    (cond ((or (and (< la l) (>= lb l))
	       (and (< lb l) (>= la l)))
	   #f)
	  ((and (>= la l) (>= lb l))
	   (let ((x (min (string-length a) (string-length b) l)))
	     (string=? (substring a 0 x) (substring b 0 x))))
	  (else
	   (string=? a b)))))

(define (readln)
  (let loop ((c (read-char)) (l '()))
    (cond ((and (eof-object? c) (null? l))
	   c)
	  ((or (eof-object? c) (char=? c #\newline))
	   (list->string (reverse l)))
	  (else (loop (read-char) (cons c l))))))



