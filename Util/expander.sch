; Util/expander.sch
; Simple string substitution facility used to convert .mac to .c (for now).
;
; $Id$

(define *templates* '())
(define *input* #f)
(define *output* #f)

(define (expand-file in . rest)
  (call-with-input-file in
    (lambda (input)
      (if (null? rest)
	  (expand-loop input (current-output-port))
	  (begin
	    (delete-file (car rest))
	    (call-with-output-file (car rest)
	      (lambda (output)
		(expand-loop input output))))))))

(define (expand-loop input output)
  (set! *templates* '())
  (set! *input* input)
  (set! *output* output)
  (do ((x (read *input*) (read *input*)))
      ((eof-object? x))
    (if (string? x)
	(display x *output*)
	(eval x))))

(define (define-template name formals template)
  (set! *templates* (cons (list name formals template) *templates*)))

(define (expand-template name actuals)
  (let ((probe (assq name *templates*)))
    (if probe
	(display (perform-expansion (cadr probe) actuals (caddr probe))
		 *output*)
	(expand-error "Template " name " not found."))))

(define (perform-expansion formals actuals template)
  (substitute
   (map (lambda (x)
	  (cons (symbol->string (car x))
		(let* ((t (cadr x))
		       (l (string-length t)))
		  (cond ((= l 0) t)
			((and (char=? #\newline (string-ref t 0))
			      (char=? #\newline (string-ref t (- l 1))))
			 (substring t 1 (- l 1)))
			((char=? #\newline (string-ref t 0))
			 (substring t 1 l))
			((char=? #\newline (string-ref t (- l 1)))
			 (substring t 0 (- l 1)))
			(else
			 t)))))
	actuals)
   template))


; To be really spiffy, we should indent a substitution to the level of
; its reference if the reference is the first non-blank token on the line.

(define (substitute bindings template)

  (define l (string-length template))

  (define (find-next loc)
    (cond ((= loc l) #f)
	  ((char=? (string-ref template loc) #\@) loc)
	  (else (find-next (+ loc 1)))))

  (define (finish substrings)
    (apply string-append (reverse substrings)))

  (define (substitute loc substrings)
    (let ((start (find-next loc)))
      (if (not start)
	  (finish (cons (substring template loc l) substrings))
	  (let ((end (find-next (+ start 1))))
	    (if (not end)
		(expand-error "Missing ending macro character; loc=" loc))
	    (let* ((varname (substring template (+ start 1) end))
		   (binding (assoc varname bindings)))
	      (if (not binding)
		  (expand-error "No binding for variable " varname " in "
				bindings))
	      (substitute (+ end 1)
			  (cons (cdr binding)
				(cons (substring template loc start)
				      substrings))))))))

  (substitute 0 '()))

(define (expand-error . rest)
  (display "Error: ")
  (for-each display rest)
  (newline)
  (reset))

; eof
