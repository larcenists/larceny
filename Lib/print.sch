; Print procedures for Larceny, based on same for MacScheme.
; Copyright 1991 Lightship Software
;
; $Id: print.sch,v 1.4 1992/06/10 09:05:33 lth Exp $
;
; Differences from MacScheme version: 
;  * printslashed now accepts both bytevectors and strings; bytevector 
;    elements are just cast to characters before use.
;  * printbytevector does not cast its argument to a string before passing 
;    it to printslashed.

(define (print x p slashify)
  
  (letrec
    
    (
     
     (print
      (lambda (x p slashify)
        (cond ((not (pair? x)) (patom x p slashify))
              ((and (memq (car x) quoters)
                    (pair? (cdr x))
                    (null? (cddr x)))
               (print-quoted x p slashify))
              (else (write-char (string-ref "(" 0) p)
                    (print (car x) p slashify)
                    (print-cdr (cdr x) p slashify)))))
     
     (print-cdr
      (lambda (x p slashify)
        (if (null? x)
            (write-char (string-ref ")" 0) p)
            (if (not (pair? x))
                (begin (printstr " . " p)
                       (patom x p slashify)
                       (write-char (string-ref ")" 0) p))
                (begin (write-char #\space p)
                       (print (car x) p slashify)
                       (print-cdr (cdr x) p slashify))))))
     
     (printstr
      (lambda (s p)
        (letrec
          ((loop (lambda (x p i n)
                   (if (< i n)
                       (begin (write-char (string-ref x i) p)
                              (loop x p (+ 1 i) n))))))
          (loop s
                p
                0
                (string-length s)))))
     
     ; Works on strings as well as bytevectors.

     (printslashed
      (lambda (s p)
        (letrec
          ((loop (lambda (x p i n)
                   (if (< i n)
		       (let ((c (if (string? x)
				    (string-ref x i)
				    (integer->char (bytevector-like-ref x i)))))
			 (if (memq c funny-characters)
			     (write-char #\\ p))
			 (write-char c p)
			 (loop x p (+ 1 i) n))))))
          (loop s
                p
                0
                (string-length s)))))
     
     (patom
      (lambda (x p slashify)
        (cond ((eq? x '())              (printstr "()" p))
              ((not x)                  (printstr "#f" p))
              ((eq? x #t)               (printstr "#t" p))
              ((symbol? x)              (printstr (symbol->string x) p))
              ((number? x)              (printnumber x p))
              ((char? x)
               (if slashify
                   (printcharacter x p)
                   (write-char x p)))
              ((string? x)
               (if slashify
                   (begin (write-char #\" p)
                          (printslashed x p)
                          (write-char #\" p))
                   (printstr x p)))
              ((vector? x)
               (begin (write-char #\# p)
                      (print (vector->list x) p slashify)))
              ((procedure? x)           (printprocedure x p slashify))
              ((bytevector? x)          (printbytevector x p slashify))
              ((eof-object? x)          (printeof x p slashify))
              ((port? x)                (printport x p slashify))
              ((eq? x (unspecified))    (printstr "#!unspecified" p))
              (else                     (printweird x p slashify)))))
     
     (printnumber
      (lambda (n p)
        (printstr (number->string n) p)))
     
     (printcharacter
      (lambda (c p)
        (write-char #\# p)
        (write-char #\\ p)
        (cond ((char=? c #\space) (printstr "space" p))
              ((char=? c #\newline) (printstr "newline" p))
              (else (write-char c p)))))
     
     (printprocedure
      (lambda (x p slashify)
        (printstr (string-append "#<PROCEDURE"
                                 (let ((doc (proc-id x)))
                                   (if doc
                                       (string-append " " (symbol->string doc))
                                       ""))
                                 ">")
                  p)))
     
     (printbytevector
      (lambda (x p slashify)
        (if (eq? slashify **lowlevel**)
            (begin (write-char #\# p)
                   (write-char (integer->char 2) p)  ; control-B
                   (write-char #\" p)
                   (printslashed x p)        ; used to be (->string x)
                   (write-char #\" p))
            (printstr "#<BYTEVECTOR>" p))))
     
     (printport
      (lambda (x p slashify)
        (printstr (string-append "#<" (if (input-port? x)
					  "INPUT PORT "
					  "OUTPUT PORT ")
				 (port-name x)
				 ">")
		  p)))
     
     (printeof
      (lambda (x p slashify)
        (printstr "#<EOF>" p)))
     
     ; Probably not Larceny-compatible. FIXME.

     (printweird
      (lambda (x p slashify)
	(printstr "#<WEIRD OBJECT>" p)))
     
     (proc-id
      (lambda (x)
        (let ((x (vector-ref (procedure-ref x 1) 0)))
          (cond ((symbol? x) x)
                ((and (pair? x) (symbol? (car x)) (not (eq? (car x) 'lambda)))
                 (car x))
                (else #f)))))
     
     (print-quoted
      (lambda (x p slashify)
        (printstr (cdr (assq (car x) quoter-strings)) p)
        (print (cadr x) p slashify)))
     
     (quoters '(quote quasiquote unquote unquote-splicing))
     
     (quoter-strings '((quote . "'")
                       (quasiquote . "`")
                       (unquote . ",")
                       (unquote-splicing . ",@")))
     
     (funny-characters (list #\" #\\ #\;))
     
     )    ; end of letrec bindings
    
    (print x p slashify)))




(define **lowlevel** (list 0))   ; any unforgeable value

; This one is a problem due to circularity in the initialization.
;
; (define **nonprinting-value** (string->symbol ""))

(define **nonprinting-value** '())
  
(define write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #t)
      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define display
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #f)
      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define lowlevel-write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p **lowlevel**)
      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define newline
  (lambda rest
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (write-char #\newline p)
      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

; This is defined as "flush-output-port" in "schemeio.scm"; all references to
; "flush-output" have been removed from the printer.
;
; (define flush-output
;   (lambda (p)
;     (write-char #\page p)))
;  

; number->string is separate so it can be redefined when bignums and flonums
; are added later.
; The optional radix argument is ignored for now.
;
; (define number->string
;  (lambda (n . format)
;    (letrec ((loop (lambda (n)                ; nonnegative n only
;                     (if (< n 10)
;                         (list (integer->char (+ (char->integer #\0) n)))
;                         (cons (integer->char (+ (char->integer #\0) (remainder n 10)))
;                               (loop (quotient n 10)))))))
;      (if (negative? n)
;          (list->string (cons #\- (reverse (loop (-- n)))))
;          (list->string (reverse (loop n)))))))
