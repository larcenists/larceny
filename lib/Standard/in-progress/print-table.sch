; Copyright 1991 Lightship Software, Incorporated.
;
; Experimental design for print tables:
;   print-table-print obj p slashify level
;   print-table-ref key
;   print-table-set! key v
;
; Each entry in the print table is a triple (key printer list-printer),
; just like for the read table.  Each dispatch procedure receives
; four arguments: the object, the port to print to, a flag whether to
; print as for WRITE or for as DISPLAY, and a print level: when descending
; into a substructure, the level must be decreased by 1.
;
; Print-table-print is like write/display, but it can be called on
; substructures to continue printing in the current context.

($$trace "print")

(define (print x p slashify)
  
  (define write-char io/write-char)
     
  (define (printstr s p)
    (let loop ((x s) p 0 (string-length s))
      (if (< i n)
	  (begin (write-char (string-ref x i) p)
		 (loop x p (+ 1 i) n)))))

  (define funny-characters (list #\" #\\ #\;))

  (define (print-string x p slashify level)
    (if slashify
        (let loop ((x x) (p p) (i 0) (n (string-length x)))
          (if (< i n)
              (let ((c (string-ref x i)))
                (if (memq c funny-characters)
                    (write-char #\\ p))
                (write-char c p)
                (loop x p (+ 1 i) n))))
        (printstr x p)))

  (define (print-string-list x p slashify level)
    (print-string x p slashify level))

  (define (print-null x p slashify level)
    (printstr "()"))

  (define (print-null-list x p slashify level)
    ...)

  (define (print-char c p slashify level)
    (if slashify
        (begin
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
        (write-char c p)))

  (define (print-char-list x p slashify level)
    (print-char x p slashify level))

  (define quoters '(quote quasiquote unquote unquote-splicing))
     
  (define quoter-strings '((quote . "'")
			   (quasiquote . "`")
			   (unquote . ",")
			   (unquote-splicing . ",@")))
     
  (define ctrl-B (integer->char 2))
  (define ctrl-C (integer->char 3))
  (define ctrl-F (integer->char 6))

  (define (print x p slashify level)
    (cond ((zero? level) (printstr "..." p))
          ((not (pair? x)) (patom x p slashify level))
	  ((and (memq (car x) quoters)
		(pair? (cdr x))
		(null? (cddr x)))
	   (print-quoted x p slashify level))
          ((zero? (- level 1))
           (printstr "(...)" p))
          ((eqv? 0 (print-length))
           (printstr "(...)" p))
	  (else
           (write-char (string-ref "(" 0) p)
           (print (car x) p slashify (- level 1))
           (print-cdr (cdr x) p slashify 
                      (- level 1) 
                      (- (or (print-length) 0) 1)))))
     
  (define (print-cdr x p slashify level length)
    (cond ((null? x)
           (write-char (string-ref ")" 0) p))
          ((zero? length)
           (printstr " ...)" p))
          ((not (pair? x))
	   (printstr " . " p)
           (patom x p slashify level)
           (write-char (string-ref ")" 0) p))
          (else
	   (write-char #\space p)
           (print (car x) p slashify level)
           (print-cdr (cdr x) p slashify level (- length 1)))))
     
  (define (print-slashed-bytevector s p)

    (define (loop x p i n)
      (if (< i n)
	  (let ((c (integer->char (bytevector-ref x i))))
	    (if (memq c funny-characters)
		(write-char #\\ p))
	    (write-char c p)
	    (loop x p (+ 1 i) n))))

    (loop s p 0 (bytevector-length s)))
     
  (define (patom x p slashify level)
    (cond ((eq? x '())              (print-null x p))
          ((boolean? x)             (print-boolean x p))
	  ((symbol? x)              (print-symbol x p))
	  ((number? x)              (print-number x p slashify))
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
		  (print (vector->list x) p slashify level)))
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
     
  (define (print-quoted x p slashify level)
    (printstr (cdr (assq (car x) quoter-strings)) p)
    (print (cadr x) p slashify (- level 1)))
    
  (set! print-table-ref
        (lambda (key)
          (case key
            ((char)   (list 'char print-char print-char-list))
            ((pair)   (list 'pair print-pair print-pair-list))
            ((number) ...)
            ((string) ...)
            ((null)   ...)
            ((boolean) ...)
            ((symbol) ...)
            ((structure) ...)
            ((environment) ...)
            ((vector) ...)
            ((procedure) ...)
            ((bytevector) ...)
            ((eof-object) ...)
            ((port) ...)
            ((unspecified) ...)
            ((undefined) ...)
            ((weird) ...)
            (else ???))))

  (set! print-table-set!
        (lambda (key v)
          (case key
            ((char) (set! print-char (cadr v))
                    (set! print-char-list (caddr v)))
            ((number) ...)
            ((string) ...)
            ((null)   ...)
            ((boolean) ...)
            ((symbol) ...)
            ((structure) ...)
            ((environment) ...)
            ((vector) ...)
            ((procedure) ...)
            ((bytevector) ...)
            ((eof-object) ...)
            ((port) ...)
            ((unspecified) ...)
            ((undefined) ...)
            ((weird) ...)
            (else ???))))

  (print x p slashify (+ (or (print-level) -2) 1)))

(define print-length
  (let ((*print-length* #f))
    (lambda rest
      (cond ((null? rest) *print-length*)
            ((null? (cdr rest))
             (let ((x (car rest)))
               (if (not (or (not x)
                            (and (fixnum? x) (>= x 0))))
                   (error "Bad argument " x " to print-length."))
               (set! *print-length* x)
               x))
            (else
             (error "Wrong number of arguments to print-length."))))))

(define print-level
  (let ((*print-level* #f))
    (lambda rest
      (cond ((null? rest) *print-level*)
            ((null? (cdr rest))
             (let ((x (car rest)))
               (if (not (or (not x)
                            (and (fixnum? x) (>= x 0))))
                   (error "Bad argument " x " to print-level."))
               (set! *print-level* x)
               x))
            (else
             (error "Wrong number of arguments to print-level."))))))

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
