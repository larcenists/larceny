; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- simple help system for the development environment

(define *help-topics*)
(define *help-mode* 'full)

(define help.keyword car)
(define help.description cadr)
(define help.texts cddr)
(define help.text.modes car)
(define help.text.text cadr)

(define (help . rest)

  (define (display-texts texts)
    (for-each (lambda (x) (display (help.text.text x))) texts))

  (define (select-texts topic)
    (let ((texts (help.texts topic)))
      (filter (lambda (text)
		(memq *help-mode* (help.text.modes text)))
	      texts)))

  (define (display-help-for keyword)
    (let ((probe (find (lambda (topic)
			 (eq? (help.keyword topic) keyword))
		       *help-topics*)))
      (if probe
	  (display-texts (select-texts probe))
	  (writeln "There is no topic " keyword))))
  
  (define (display-topics)
    (for-each (lambda (topic)
		(writeln (help.description topic)))
	      *help-topics*))

  (define (writeln . x)
    (display "  ")
    (for-each display x)
    (newline))

  (if (null? rest)
      (begin (writeln "Help is available on the following topics: ")
	     (display-topics)
	     (writeln "For help on any topic, type (help '<topic>)"))
      (display-help-for (car rest))))

(define (initialize-help dir . rest)
  (if (not (null? rest))
      (if (memq (car rest) '(brief full))
	  (set! *help-mode* (car rest))
	  (error "Invalid help mode " (car rest))))
  (set! *help-topics*
	(call-with-input-file (string-append dir "help-topics.txt")
	  (lambda (p)
	    (let loop ((topics '()) (i (read p)))
	      (if (not (eof-object? i))
		  (loop (cons i topics) (read p))
		  (compat:sort 
		   (filter (lambda (topic)
			     (some? (lambda (t)
				      (memq *help-mode* (help.text.modes t)))
				    (help.texts topic)))
			   topics)
		   (lambda (a b)
		     (string<? (symbol->string (help.keyword a))
			       (symbol->string (help.keyword b))))))))))
  (unspecified))

; eof
