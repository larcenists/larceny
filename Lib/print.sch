; Lib/print.sch
; Larceny -- Print procedures 
;
; $Id$
;
; Copyright 1991 Lightship Software

($$trace "print")

(define (print x p slashify)
  
  (define write-char io/write-char)
     
  (define quoters '(quote quasiquote unquote unquote-splicing))
     
  (define quoter-strings '((quote . "'")
			   (quasiquote . "`")
			   (unquote . ",")
			   (unquote-splicing . ",@")))
     
  (define funny-characters (list #\" #\\ #\;))

  (define ctrl-B (integer->char 2))
  (define ctrl-C (integer->char 3))
  (define ctrl-F (integer->char 6))

  (define (print x p slashify)
    (cond ((not (pair? x)) (patom x p slashify))
	  ((and (memq (car x) quoters)
		(pair? (cdr x))
		(null? (cddr x)))
	   (print-quoted x p slashify))
	  (else (write-char (string-ref "(" 0) p)
		(print (car x) p slashify)
		(print-cdr (cdr x) p slashify))))
     
  (define (print-cdr x p slashify)
    (if (null? x)
	(write-char (string-ref ")" 0) p)
	(if (not (pair? x))
	    (begin (printstr " . " p)
		   (patom x p slashify)
		   (write-char (string-ref ")" 0) p))
	    (begin (write-char #\space p)
		   (print (car x) p slashify)
		   (print-cdr (cdr x) p slashify)))))
     
  (define (printstr s p)

    (define (loop x p i n)
      (if (< i n)
	  (begin (write-char (string-ref x i) p)
		 (loop x p (+ 1 i) n))))

    (loop s p 0 (string-length s)))
     
  (define (print-slashed-string s p)

    (define (loop x p i n)
      (if (< i n)
	  (let ((c (string-ref x i)))
	    (if (memq c funny-characters)
		(write-char #\\ p))
	    (write-char c p)
	    (loop x p (+ 1 i) n))))

    (loop s p 0 (string-length s)))

  (define (print-slashed-bytevector s p)

    (define (loop x p i n)
      (if (< i n)
	  (let ((c (integer->char (bytevector-ref x i))))
	    (if (memq c funny-characters)
		(write-char #\\ p))
	    (write-char c p)
	    (loop x p (+ 1 i) n))))

    (loop s p 0 (bytevector-length s)))
     
  (define (patom x p slashify)
    (cond ((eq? x '())              (printstr "()" p))
	  ((not x)                  (printstr "#f" p))
	  ((eq? x #t)               (printstr "#t" p))
	  ((symbol? x)              (printstr (symbol->string x) p))
	  ((number? x)              (printnumber x p slashify))
	  ((char? x)
	   (if slashify
	       (printcharacter x p)
	       (write-char x p)))
	  ((string? x)
	   (if slashify
	       (begin (write-char #\" p)
		      (print-slashed-string x p)
		      (write-char #\" p))
	       (printstr x p)))
	  ((structure? x)
	   ((structure-printer) x p slashify))
	  ((environment? x)
	   (printstr (string-append "#<ENVIRONMENT "
				    (environment-name x)
				    ">")
		     p))
	  ((vector? x)
	   (begin (write-char #\# p)
		  (print (vector->list x) p slashify)))
	  ((procedure? x)           (printprocedure x p slashify))
	  ((bytevector? x)          (printbytevector x p slashify))
	  ((eof-object? x)          (printeof x p slashify))
	  ((port? x)                (printport x p slashify))
	  ((eq? x (unspecified))    (printstr "#!unspecified" p))
	  ((eq? x (undefined))      (printstr "#!undefined" p))
	  (else                     (printweird x p slashify))))
     
  (define (printnumber n p slashify)
    (if (eq? slashify **lowlevel**)
	(cond ((flonum? n)
	       (write-char #\# p)
	       (write-char ctrl-F p)
	       (do ((i 4 (+ i 1)))
		   ((= i 12))
		 (write-char (integer->char (bytevector-like-ref n i)) p)))
	      ((compnum? n)
	       (write-char #\# p)
	       (write-char ctrl-C p)
	       (do ((i 4 (+ i 1)))
		   ((= i 20))
		 (write-char (integer->char (bytevector-like-ref n i)) p)))
	      (else
	       (printstr (number->string n) p)))
	(printstr (number->string n) p)))
     
  (define (printcharacter c p)
    (write-char #\# p)
    (write-char #\\ p)
    (let ((k (char->integer c)))
      (cond ((= k **space**)  (printstr "space" p))
	    ((= k **newline**) (printstr "newline" p))
	    ((= k **tab**) (printstr "tab" p))
	    ((= k **carriage-return**) (printstr "return" p))
	    ((= k **linefeed**) (printstr "linefeed" p))
	    ((= k **form-feed**) (printstr "page" p))
	    ((= k **backspace**) (printstr "backspace" p))
	    (else (write-char c p)))))
     
  (define (printprocedure x p slashify)
    (printstr (string-append "#<PROCEDURE"
			     (let ((doc (procedure-name x)))
			       (if doc
				   (string-append " " (symbol->string doc))
				   ""))
			     ">")
	      p))
     
  (define (printbytevector x p slashify)
    (if (eq? slashify **lowlevel**)
	(begin (write-char #\# p)
	       (write-char ctrl-B p)
	       (write-char #\" p)
	       (print-slashed-bytevector x p)
	       (write-char #\" p))
	(printstr "#<BYTEVECTOR>" p)))
     
  (define (printport x p slashify)
    (printstr (string-append "#<" (cond ((input-port? x) "INPUT PORT ")
					((output-port? x) "OUTPUT PORT ")
					(else "PORT "))
			     (port-name x)
			     ">")
	      p))
     
  (define (printeof x p slashify)
    (printstr "#<EOF>" p))
     
  (define (printweird x p slashify)
    (printstr "#<WEIRD OBJECT>" p))
     
  (define (print-quoted x p slashify)
    (printstr (cdr (assq (car x) quoter-strings)) p)
    (print (cadr x) p slashify))
    
  (print x p slashify))


(define **lowlevel** (list 0))   ; any unforgeable value

(define **nonprinting-value** (unspecified))
  
(define write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #t)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define display
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #f)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define lowlevel-write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p **lowlevel**)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define newline
  (lambda rest
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (write-char #\newline p)
      (io/discretionary-flush p)
      **nonprinting-value**)))

; eof
