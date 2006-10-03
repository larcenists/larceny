; Infix expression parser and evaluator (operates on strings)
; 2001-12-13 / lth
;
; $Id$

; Works, but if the operator specs are wrong then it silently drops
; things on the floor (can't have infix and prefix at the same precedence
; level).  Either fix this or (easier) introduce a detector for it.
;
; It would be good to support non-integer numbers, after all it's Scheme.
; It would be easy to support xfy, yfy, and yf.

(require 'list)
(require 'trie)

; operators ::= (operator ...)
; operator  ::= (string precedence fixity (operation))   
;   where string describes the lexeme
;         precedence is a nonnegative integer, 1 is lowest
;         fixity is one of the symbols yfx, fy
;         operation is a procedure of the appropriate arity

(define (infix-expr-parser operators)

  (define op-name car)
  (define op-precedence cadr)
  (define op-fixity caddr)
  (define op-operation cadddr)
  (define op-opname (lambda (x) (car (cddddr x))))

  (define LEFTPAREN 'lp)
  (define RIGHTPAREN 'rp)
  (define NUL (integer->char 0))

  (let* ((operators
	  (map (lambda (x)
		 (append x (list (string->symbol (op-name x)))))
	       operators))
	 (ident-trie 
	  (make-trie char=? NUL (map (lambda (x)
				       (cons (string->list (op-name x))
					     (op-opname x)))
				     operators))))

    (define (tokenize input fail)
      (let ((lim (string-length input)))
	(let loop ((i 0) (ts '()))
	  (if (= i lim)
	      (reverse ts)
	      (let ((c (string-ref input i)))
		(cond ((char-whitespace? c)     
		       (loop (+ i 1) ts))

		      ((char-numeric? c)
		       (let ((start i))
			 (let numloop ((i (+ i 1)))
			   (if (or (= i lim)
				   (not (char-numeric? (string-ref input i))))
			       (loop i (cons (string->number (substring input start i)) ts))
			       (numloop (+ i 1))))))
		      
		      ((char=? c #\()
		       (loop (+ i 1) (cons LEFTPAREN ts)))

		      ((char=? c #\))
		       (loop (+ i 1) (cons RIGHTPAREN ts)))

		      ((assq c ident-trie)
		       =>
		       (lambda (subtrie)
			 (let oploop ((subtrie (cdr subtrie)) (i (+ i 1)))
			   (cond ((= i lim)
				  (let ((probe (assq NUL subtrie)))
				    (if probe
					(loop i (cons (cdr probe) ts))
					(fail))))
				 ((assq (string-ref input i) subtrie)
				  =>
				  (lambda (subtrie2)
				    (oploop (cdr subtrie2) (+ i 1))))
				 ((assq NUL subtrie)
				  =>
				  (lambda (probe)
				    (loop i (cons (cdr probe) ts))))
				 (else
				  (fail))))))
		      
		      (else  
		       (fail))))))))

    (define (token-op op class)
      (cdr (assq op class)))

    (define (infix-parser-left operators higher-prec op-class)
      (lambda (tokens fail)
	(call-with-values
	    (lambda () (higher-prec tokens fail))
	  (lambda (x tokens)
	    (let loop ((x x) (tokens tokens))
	      (if (or (null? tokens) (not (memq (car tokens) operators)))
		  (values x tokens)
		  (let ((op (car tokens)))
		    (call-with-values
			(lambda () (higher-prec (cdr tokens) fail))
		      (lambda (y tokens)
			(loop ((token-op op op-class) x y) tokens))))))))))

    (define (prefix-parser-right operators higher-prec op-class)
      (lambda (tokens fail)
	(if (or (null? tokens) (not (memq (car tokens) operators)))
	    (higher-prec tokens fail)
	    (let ((op (car tokens)))
	      (call-with-values
		  (lambda () (higher-prec (cdr tokens) fail))
		(lambda (x tokens)
		  (values ((token-op op op-class) x) tokens)))))))

    (define (atom-parser tokens fail)
      (cond ((null? tokens) 
	     (fail tokens))
	    ((eq? (car tokens) LEFTPAREN)
	     (call-with-values 
		 (lambda () (expr-parser (cdr tokens) fail))
	       (lambda (x tokens)
		 (if (or (null? tokens) (not (eq? (car tokens) RIGHTPAREN)))
		     (fail))
		 (values x (cdr tokens)))))
	    ((number? (car tokens))
	     (values (car tokens) (cdr tokens)))
	    (else
	     (fail))))

    (define expr-parser #f)

    (define (evaluate tokens fail)
      (call-with-values
	  (lambda () (expr-parser tokens fail))
	(lambda (result tokens)
	  (if (null? tokens)
	      result
	      (fail)))))

    (set! expr-parser
	  (let loop ((operators 
		      (sort (accumulate-unordered
			     (lambda (x y) (= (op-precedence x) (op-precedence y)))
			     operators)
			    (lambda (x y)
			      (> (op-precedence (car x)) (op-precedence (car y))))))
		     (parser    atom-parser))
	    (cond ((null? operators)
		   parser)
		  ((eq? 'yfx (op-fixity (caar operators)))
		   (loop (cdr operators) 
			 (infix-parser-left (map op-opname (car operators)) 
					    parser
					    (map (lambda (op)
						   (cons (op-opname op) (op-operation op)))
						 (car operators)))))
		  ((eq? 'fy (op-fixity (caar operators)))
		   (loop (cdr operators) 
			 (prefix-parser-right (map op-opname (car operators)) 
					      parser
					      (map (lambda (op)
						     (cons (op-opname op) (op-operation op)))
						   (car operators)))))
		  (else ???))))

    (lambda (input . errval)
      (let ((errval (if (null? errval) #f (car errval))))
	(call-with-current-continuation
	 (lambda (c)
	   (let ((fail (lambda rest 
			 ; (display "FAILED: ") (write rest) (newline) 
			 (c errval))))
	     (evaluate (tokenize input fail) fail))))))))


(define eval-infix-expr 
  (let ((parser (infix-expr-parser `(("+" 1 yfx ,+)
				     ("-" 1 yfx ,-)
				     ("*" 2 yfx ,*)
				     ("/" 2 yfx ,quotient)
				     ("%" 2 yfx ,remainder)
				     ("-" 3 fy ,-)
				     ("+" 3 fy ,+)
				     ("^" 5 yfx ,expt)
				     ("sqrt" 4 fy ,sqrt)
				     ("!" 4 fy ,(lambda (x)
						  (letrec ((fact 
							    (lambda (n)
							      (if (= n 1)
								  1
								  (* n (fact (- n 1)))))))
						    (fact x))))
				     ("++" 4 fy ,(lambda (x) (+ x 1)))))))
    (lambda (input . errval)
      (apply parser input errval))))

(define parse-infix-expr 
  (let ((parser (infix-expr-parser `(("+" 1 yfx ,(lambda (x y) `(+ ,x ,y)))
				     ("-" 1 yfx ,(lambda (x y) `(- ,x ,y)))
				     ("*" 2 yfx ,(lambda (x y) `(* ,x ,y)))
				     ("/" 2 yfx ,(lambda (x y) `(quotient ,x ,y)))
				     ("%" 2 yfx ,(lambda (x y) `(remainder ,x ,y)))
				     ("-" 3 fy  ,(lambda (x) `(- ,x)))
				     ("+" 3 fy ,(lambda (x) `(+ ,x)))
				     ("^" 5 yfx ,(lambda (x y) `(expt ,x ,y)))
				     ("sqrt" 4 fy ,(lambda (x) `(sqrt ,x)))
				     ("!" 4 fy ,(lambda (x) `(fact ,x)))
				     ("++" 4 fy ,(lambda (x) `(+ ,x 1)))))))
    (lambda (input . errval)
      (apply parser input errval))))

; eof
