; Compiler/help.sch
; Larceny -- simple help system for the development environment
;
; $Id: help.sch,v 1.1.1.1 1998/11/19 21:52:21 lth Exp $

(define *help-topics*)

(define (help . rest)

  (define (display-help-for topic)
    (let ((probe (assq topic *help-topics*)))
      (if probe
	  (display (caddr probe))
	  (writeln "There is no topic " topic))))
  
  (define (display-topics)
    (for-each (lambda (topic)
		(writeln (cadr topic)))
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

(define (initialize-help dir)
  (set! *help-topics*
	(call-with-input-file (string-append dir "help-topics.txt")
	  (lambda (p)
	    (let loop ((l '()) (i (read p)))
	      (if (eof-object? i)
		  (compat:sort l (lambda (a b)
				   (string<? (symbol->string (car a))
					     (symbol->string (car b)))))
		  (loop (cons i l) (read p))))))))

; eof
