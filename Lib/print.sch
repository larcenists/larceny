; Lib/print.sch
; Larceny -- Print procedures 
;
; $Id: print.sch,v 1.7 1997/09/23 20:08:39 lth Exp lth $
;
; Copyright 1991 Lightship Software

($$trace "print")

(define (print x p slashify)
  
  (letrec
    
    (
     (write-char io/write-char)
     
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
     
     ; FIXME: Should use write-bytevector-like here, probably.

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
     ; FIXME: the use of memq is _not_ good.

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
                                 (let ((doc (procedure-name x)))
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
        (printstr (string-append "#<" (cond ((input-port? x) "INPUT PORT ")
					    ((output-port? x) "OUTPUT PORT ")
					    (else "PORT "))
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

(define **nonprinting-value** (unspecified))
  
(define write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #t)
      (io/discretionary-flush p)
;      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define display
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p #f)
      (io/discretionary-flush p)
;      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define lowlevel-write
  (lambda (x . rest)
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (print x p **lowlevel**)
      (io/discretionary-flush p)
;      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

(define newline
  (lambda rest
    (let ((p (if (null? rest) (current-output-port) (car rest))))
      (write-char #\newline p)
      (io/discretionary-flush p)
;      (if (eq? p (current-output-port)) (flush-output-port p))
      **nonprinting-value**)))

; eof
