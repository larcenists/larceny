; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny -- Print procedures.

($$trace "print")

;;; Parameterized hooks to customize the printer.
(define code-object-printer
  (make-parameter
   "code-object-printer"
   (lambda (co port slashify)
     (print (string-append "#<"
                           (car (vector-ref co 0))
                           ">")
            port
            #f))))

(define environment-printer
  (make-parameter
   "environment-printer"
   (lambda (environment port slashify)
     (print (string-append "#<ENVIRONMENT "
                           (environment-name environment)
                           ">")
            port
            #f))))

(define hashtable-printer
  (make-parameter
   "hashtable-printer"
   (lambda (hashtable port slashify)
     (print "#<HASHTABLE>" port #f))))

(define procedure-printer
  (make-parameter
   "procedure-printer"
   (lambda (procedure port slashify)
     (print (string-append "#<PROCEDURE"
                           (let ((doc (procedure-name procedure)))
                             (if doc
                                 (string-append " " (symbol->string doc))
                                 ""))
                           ">")
            port
            #f))))

(define weird-printer
  (make-parameter
   "weird-printer"
   (lambda (weirdo port slashify)
     (print "#<WEIRD OBJECT>" port #f))))

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

  ;; Don't print ellipsis when slashifying (that is, when
  ;; using WRITE rather than DISPLAY) because result is
  ;; being printed with intent to read it back in.
  (define (print x p slashify level)
    (cond ((and (not slashify)
                (zero? level))
           (printstr "..." p))
          ((not (pair? x)) (patom x p slashify level))
	  ((and (memq (car x) quoters)
		(pair? (cdr x))
		(null? (cddr x)))
	   (print-quoted x p slashify level))
          ((and (not slashify)
                (zero? (- level 1)))
           (printstr "(...)" p))
          ((and (not slashify)
                (eqv? 0 (print-length)))
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
          ((and (not slashify)
                (zero? length))
           (printstr " ...)" p))
          ((pair? x)
           (write-char #\space p)
           (print (car x) p slashify level)
           (print-cdr (cdr x) p slashify level (- length 1)))
          (else
	   (printstr " . " p)
           (patom x p slashify level)
           (write-char (string-ref ")" 0) p))))

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

  (define (patom x p slashify level)
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

	  ((vector? x) (cond ((environment? x) (printenvironment x p slashify))
                             ((code-object? x) (printcodeobject x p slashify))
                             ((hashtable? x)   (printhashtable x p slashify))
                             (else (write-char #\# p)
                                   (print (vector->list x) p slashify level))))

	  ((procedure? x)           (printprocedure x p slashify))
	  ((bytevector? x)          (printbytevector x p slashify))
	  ((eof-object? x)          (printeof x p slashify))
	  ((port? x)                (printport x p slashify))
	  ((eq? x (unspecified))    (printstr "#!unspecified" p))
	  ((eq? x (undefined))      (printstr "#!undefined" p))
	  ((structure? x)
	   ((structure-printer) x p slashify))
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

  (define (printcodeobject x p slashify)
    ((code-object-printer) x p slashify))

  (define (printenvironment x p slashify)
    ((environment-printer) x p slashify))

  (define (printhashtable x p slashify)
    ((hashtable-printer) x p slashify))

  (define (printprocedure x p slashify)
    ((procedure-printer) x p slashify))

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
    ((weird-printer) x p slashify))

  (define (print-quoted x p slashify level)
    (printstr (cdr (assq (car x) quoter-strings)) p)
    (print (cadr x) p slashify (- level 1)))

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
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #t)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define display
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #f)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define lowlevel-write
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p **lowlevel**)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define newline
  (lambda rest
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (write-char #\newline p)
      (io/discretionary-flush p)
      **nonprinting-value**)))

; eof
