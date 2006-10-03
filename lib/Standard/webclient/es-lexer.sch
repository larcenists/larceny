; Copyright 2004 Lars T Hansen <lth@acm.org>
;
; Lexical analyzer for something that is roughly ECMAScript 3rd
; edition, without unicode support.
;
; 2004-02-07 / lth
;
; Status: Believed complete; barely tested.

; ES-LEX in operand? => token | eof
;
;   Return the next token from the input stream IN.  Tokens are
;   symbols representing keywords, punctuation, operators, and
;   newline, or lists representing numbers, strings, identifiers, and
;   regex literals.
;
;   If OPERAND? is true then a '/' is taken to be the start of a
;   regular expression literal rather than the division operator.

(define es-lex-temp (open-output-string))

(define (es-lex in operand?)

  (define (operator . specs)
    (let loop ((specs specs))
      (cond ((symbol? (car specs))
	     (car specs))
	    ((eqv? (caar specs) (peek-char in))
	     (read-char in)
	     (loop (cdar specs)))
	    (else
	     (loop (cdr specs))))))

  (let ((x (read-char in)))
    (cond ((eof-object? x)
	   x)
	  ((char=? x #\newline)
	   'newline)
	  ((char-whitespace? x)
	   (es-lex in))
	  ((or (char-alphabetic? x) (char=? x #\_) (char=? x #\$))
	   (es-lex-ident in x))
	  ((char-numeric? x)
	   (es-lex-number in x))
	  (else
	   (case x
	     ((#\+) (operator '(#\+ increment) '(#\= add-assign) 'add))
	     ((#\-) (operator '(#\- decrement) '(#\= sub-assign) 'sub))
	     ((#\=) (operator '(#\= (#\= eq) eqv) 'assign))
	     ((#\>) (operator '(#\= ge) '(#\> (#\> bitshru) bitshr) 'gt))
	     ((#\<) (operator '(#\= le) '(#\< bitshl) 'lt))
	     ((#\*) (operator '(#\= mul-assign) 'mul))
	     ((#\%) (operator '(#\= mod-assign) 'mod))
	     ((#\/) (cond ((eqv? (peek-char in) #\/)
			   (es-lex-single-line-comment in))
			  ((eqv? (peek-char in) #\*)
			   (read-char in)
			   (es-lex-multiline-comment in))
			  (operand?
			   (es-lex-regex in))
			  (else
			   (operator '(#\= div-assign) 'div))))
	     ((#\{) 'lbrace)
	     ((#\}) 'rbrace)
	     ((#\() 'lparen)
	     ((#\)) 'rparen)
	     ((#\[) 'lbracket)
	     ((#\]) 'rbracket)
	     ((#\.) 'dot)
	     ((#\;) 'semi)
	     ((#\,) 'comma)
	     ((#\!) (operator '(#\= (#\= noteq) noteqv) 'not))
	     ((#\&) (operator '(#\& and) '(#\= 'bitand-assign) 'bitand))
	     ((#\|) (operator '(#\| or) '(#\= 'bitor-assign) 'bitor))
	     ((#\^) (operator '(#\= bitxor-assign) 'bitxor))
	     ((#\~) 'bitnot)
	     ((#\?) 'quest)
	     ((#\:) 'colon)
	     ((#\" #\') (es-lex-string in x))
	     (else ???))))))

(define (es-lex-ident in c)
  (reset-output-string es-lex-temp)
  (display c es-lex-temp)
  (let loop ()
    (let ((c (peek-char in)))
      (if (and (char? c)
	       (or (char-alphabetic? c)
		   (char-numeric? c)
		   (char=? c #\_)
		   (char=? c #\$)))
	  (begin
	    (read-char in)
	    (display c es-lex-temp)
	    (loop))
	  (let ((s (string->symbol (get-output-string es-lex-temp))))
	    (if (es-lex-keyword? s)
		s
		(list 'ident s)))))))

(define (es-lex-number in c)

  (define (finish)
    (let ((x (string->number (get-output-string es-lex-temp))))
      (if x
	  (list 'number (exact->inexact x))
	  (es-syntax-error "Invalid number"))))

  (define (lex-number state)
    (let ((c (peek-char in)))
      (cond ((eof-object? c)
	     (finish))
	    ((char-numeric? c)
	     (display (read-char in) es-lex-temp)
	     (lex-number (if (= state 2) 3 state)))
	    ((and (char=? c #\.) (= state 0))
	     (display (read-char in) es-lex-temp)
	     (lex-number 1))
	    ((and (char-ci=? c #\E) (or (= state 0) (= state 1)))
	     (display (read-char in) es-lex-temp)
	     (lex-number 2))
	    ((and (or (char=? c #\+) (char=? c #\-)) (= state 2))
	     (display (read-char in) es-lex-temp)
	     (lex-number 3))
	    (else
	     (finish)))))

  (reset-output-string es-lex-temp)
  (display c es-lex-temp)
  (if (char=? c #\0)
      (let ((d (peek-char in)))
	(if (and (char? d) (char-ci=? d #\X))
	    (begin (read-char in)
		   (es-lex-hex in -1))
	    (lex-number 0)))
      (lex-number 0)))

(define (es-lex-hex in limit)
  (let loop ((n limit) (sum 0))
    (if (= n 0)
	sum
	(let ((c (peek-char in)))
	  (if (eof-object? c) 
	      (if (= n limit)
		  (es-syntax-error "Invalid hex constant")
		  sum)
	      (let ((c (char-upcase c)))
		(cond ((char-numeric? c)
		       (read-char in)
		       (loop (- n 1) (+ (* sum 16) (- (char->integer c)
						      (char->integer #\0)))))
		      ((and (char<=? #\A c) (char<=? c #\F))
		       (read-char in)
		       (loop (- n 1) (+ (* sum 16) 
					(- (char->integer c)
					   (char->integer #\A))
					10)))
		      ((= n limit)
		       (es-syntax-error "Invalid hex constant"))
		      (else
		       sum))))))))

(define (es-lex-string in quote-char)
  (reset-output-string es-lex-temp)
  (let loop ()
    (let ((c (read-char in)))
      (cond ((eof-object? c) 
	     (es-syntax-error "EOF in string constant"))
	    ((char=? c quote-char)
	     (list 'string (get-output-string es-lex-temp)))
	    ((char=? c #\\)
	     (let ((d (read-char in)))
	       (if (eof-object? d)
		   (es-syntax-error "EOF in string constant")
		   (begin
		     (case d
		       ((#\b) (display (integer->char 8) es-lex-temp))
		       ((#\f) (display (integer->char 12) es-lex-temp))
		       ((#\n) (display (integer->char 10) es-lex-temp))
		       ((#\r) (display (integer->char 13) es-lex-temp))
		       ((#\t) (display (integer->char 9) es-lex-temp))
		       ((#\v) (display (integer->char 11) es-lex-temp))
		       ((#\x) (display (integer->char (es-lex-hex in 2)) 
				       es-lex-temp))
		       ((#\u) (display (integer->char 
					(remainder (es-lex-hex in 4) 256))
				       es-lex-temp))
		       ((#\0) (display (integer->char 0) es-lex-temp))
		       (else
			(display d es-lex-temp)))
		     (loop)))))
	    (else
	     (display c es-lex-temp)
	     (loop))))))

; Regex is terminated by a / but not inside parens or brackets, and
; not if it is escaped.

(define (es-lex-regex in)

  (define (scan term)
    (let ((c (read-char in)))
      (if (eof-object? c)
	  (es-syntax-error "EOF in regex literal")
	  (begin
	    (display c es-lex-temp)
	    (cond ((char=? c term))
		  ((char=? c #\\)
		   (let ((d (read-char in)))
		     (if (not (eof-object? c))
			 (display d es-lex-temp))
		     (scan term)))
		  ((char=? c #\() 
		   (scan #\))
		   (scan term))
		  ((char=? c #\[)
		   (let ((d (peek-char in)))
		     (cond ((eof-object? d)
			    (scan term))
			   ((char=? d #\])
			    (display (read-char in) es-lex-temp))
			   ((char=? d #\^)
			    (display (read-char in) es-lex-temp)
			    (let ((e (peek-char in)))
			      (cond ((eof-object? e)
				     (scan term))
				    ((char=? e #\])
				     (display (read-char in) es-lex-temp))))))
		     (scan #\])
		     (scan term)))
		  (else
		   (scan term)))))))

  (reset-output-string es-lex-temp)
  (scan #\/)
  (let loop ((flags '()))
    (let ((c (peek-char in)))
      (if (and (char? c)
	       (or (char=? c #\i)
		   (char=? c #\g)
		   (char=? c #\m)))
	  (loop (cons (read-char in) flags))
	  (let ((s (get-output-string es-lex-temp)))
	    (list 'regex 
		  (substring s 0 (- (string-length s) 1))
		  (list->string flags)))))))

(define (es-lex-single-line-comment in)
  (let ((c (peek-char in)))
    (cond ((eof-object? c) (es-lex in))
	  ((char=? c #\newline) (es-lex in))
	  (else
	   (read-char in)
	   (es-lex-single-line-comment in)))))

(define (es-lex-multi-line-comment in)
  (let ((c (peek-char in)))
    (cond ((eof-object? c) 
	   (es-syntax-error "EOF in multi-line comment"))
	  ((char? c #\*)
	   (read-char in)
	   (if (eqv? (peek-char in) #\/)
	       (begin
		 (read-char in)
		 (es-lex in))
	       (es-lex-multi-line-comment in)))
	  (else
	   (read-char in)
	   (es-lex-multi-line-comment in)))))

(define (es-lex-keyword? s)
  (memq s '(break case catch continue default delete do else false 
	    finally for function if in instanceof new null return 
	    switch this throw true try typeof var void while with)))

; eof
