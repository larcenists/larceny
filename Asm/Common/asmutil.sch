; Asm/Common/asmutil.sch
; Larceny assembler -- miscellaneous utility procedures.
;
; $Id$

; Given any Scheme object, return its printable representation as a string.
; This code is largely portable (see comments).

(define (format-object x)

  (define (format-list x)
    (define (loop x)
      (cond ((null? x)
	     '(")"))
	    ((null? (cdr x))
	     (list (format-object (car x)) ")"))
	    (else
	     (cons (format-object (car x))
		   (cons " " 
			 (loop (cdr x)))))))
    (apply string-append (cons "(" (loop x))))

  (define (format-improper-list x)
    (define (loop x)
      (if (pair? (cdr x))
	  (cons (format-object (car x))
		(cons " "
		      (loop (cdr x))))
	  (list (format-object (car x))
		" . "
		(format-object (cdr x))
		")")))
    (apply string-append (cons "(" (loop x))))

  (cond ((null? x)             "()")
	((not x)               "#f")
	((eq? x #t)            "#t")
	((symbol? x)           (symbol->string x))
	((number? x)           (number->string x))
	((char? x)             (string x))
	((string? x)           x)
	((procedure? x)        "#<procedure>")
	((bytevector? x)       "#<bytevector>")     ; Larceny
	((eof-object? x)       "#<eof>")
	((port? x)             "#<port>")
	((eq? x (unspecified)) "#!unspecified")     ; Larceny
	((eq? x (undefined))   "#!undefined")       ; Larceny
	((vector? x)
	 (string-append "#" (format-list (vector->list x))))
	((list? x)
	 (format-list x))
	((pair? x)
	 (format-improper-list x))
	(else                  "#<weird>")))

; eof
