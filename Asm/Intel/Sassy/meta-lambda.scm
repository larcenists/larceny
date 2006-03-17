; meta-lambda.scm - A simple parser generator
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; module meta-lambda
; export-syntax meta-lambda case-meta-lambda memoize

; Meta-lambda
; Another Henry Baker-inspired hack. see:
; http://home.pipeline.com/~hbaker1/Prag-Parse.html

; See after the code for documentation


(define-syntax meta-expand
  (syntax-rules (or and lambda begin quote unquote
		    unquote-splicing __ ? + * ?* else)
    ((_ p i r (quote a)) (and (not (null? i))
			      (pair? i)
			      (equal? 'a (car i))
			      (begin (set! i (cdr i)) #t)))
    ((_ p i r (unquote a)) (and (not (null? i))
				(pair? i)
				(equal? a (car i))
				(begin (set! i (cdr i)) #t)))
    ((_ p i r (unquote-splicing a)) (begin (set! i (list i))
					   (meta-expand p i r a)))
    ((_ p i r (or a ...)) (let ((ti i) (tp p))
			    (or (or (meta-expand p i r a)
				    (begin (set! i ti)
					   (set-cdr! tp '())
					   (set! p tp)
					   #f))
				...)))
    ((_ p i r (and a ...)) (and (meta-expand p i r a) ...))
    ((_ p i r (lambda a b ...)) (and (null? i)
				     (apply (lambda a b ...) (cdr r))))
    ((_ p i r (begin a b ...))  (and (null? i) (begin a b ...)))
    ((_ p i r (else a)) (let ((tmp (a i)))
			  (set! i '())
			  tmp))
    ((_ p i r (+ a)) (let* ((nr (list #t))
			    (np nr))
		       (and (meta-expand np i nr a)
			    (do () ((not (meta-expand np i nr a))
				    (set! nr (list (cdr nr)))
				    (set-cdr! p nr)
				    (set! p nr)
				    #t)))))
    ((_ p i r (* a)) (let* ((nr (list #t))
			    (np nr))
		       (do () ((not (meta-expand np i nr a))
			       (set! nr (list (cdr nr)))
			       (set-cdr! p nr)
			       (set! p nr)
			       #t))))
    ((_ p i r (?* a)) (or (meta-expand p i r a)
			  #t))
    ((_ p i r ()) (null? i))
    ((_ p i r (a)) (and (not (null? i))
			(pair? i)
			(cond (((meta-lambda a) (car i)) =>
			       (lambda (res)
				 (if (not (eq? #t res))
				     (begin (set! res (list res))
					    (set-cdr! p res)
					    (set! p res)))
				 (set! i (cdr i))
				 #t))
			      (else #f))))
    ((_ p i r __) (if (or (pair? i) (null? i))
		      (begin (set-cdr! p i) (set! p i) (set! i '()) '__tail)
		      #f))
    ((_ p i r ?) (and (not (null? i))
		      (pair? i)
		      (let ((t (list (car i))))
			(set-cdr! p t)
			(set! p t)
			(set! i (cdr i))
			#t)))
    ((_ p i r x) (let-syntax ((test (syntax-rules ()
				    ((test x w l) w)
				    ((test y w l) l))))
		 (test __fubar__
		       (and (not (null? i))
			    (pair? i)
			    (cond ((x (car i)) =>
				   (lambda (res)
				     (let ((tmp (if (eq? res #t)
						    (list (car i))
						    (list res))))
				       (set-cdr! p tmp)
				       (set! p tmp)
				       (set! i (cdr i)) #t)))
				  (else #f)))
		       (and (not (null? i))
			    (pair? i)
			    (equal? x (car i))
			    (begin (set! i (cdr i)) #t)))))))

(define-syntax meta-lambda
  (syntax-rules ()
    ((meta-lambda grammar)
     (lambda (i)
       (let* ((r (list #t))
	      (p r))
	 (cond ((meta-expand p i r grammar)
		=> (lambda (res)
		     (if (null? i)
			 (if (eq? res #t)
			     (cond ((null? (cdr r)) #t)
				   ((null? (cddr r)) (cadr r))
				   (else (cdr r)))
			     (if (eq? res '__tail) (cdr r) res))
			 #f)))
	       (else #f)))))))

; var-arity meta-lambda
(define-syntax meta-lambda-dot
  (syntax-rules ()
    ((_ x y ...) (lambda args
		   (let ((tmp (meta-lambda x y ...)))
		     (tmp args))))))

; Something useful to wrap meta-lambda in to hurry things along.
; Of course only use this when not using side-effects.
(define-syntax memoize
  (syntax-rules ()
    ((_ proc)
     (let ((the-proc proc))
       (let ((last-in  '%#$%#%#$%)
	     (last-out #f))
	 (lambda (arg2)
	   (if (eq? arg2 last-in)
	       last-out
	       (begin (set! last-in arg2)
		      (set! last-out (the-proc arg2))
		      last-out))))))))

; |===========|
; |Meta-lambda|
; |===========|

; Meta-lambda is a macro for building parsers and pattern matchers
; over lists or single items. You can also specify "actions" to be
; performed when a list has been successfully parsed, so it can also
; function as a very rudimentary compiler-generator or
; attribute-grammar-generator (using synthesized attributes).

; It's really for constructing simple embedded langauges, and it has its
; limitations if your're not willing to factor out tougher grammars by
; hand. But I've found it useful.

; Here's a simple example so you can see where this is going:

; |=====|
; |Usage|
; |=====|

; meta-lambda grammer -> procedure

; Grammars are described below. The procedure generated is a procedure
; of one argument. When applied to an item (usually a list), it attempts
; to match the grammar with the list and perform any actions specified
; if it was able to completely match all the items in the list (to the
; end of the list). If the list or item can not be matched completely,
; the procedure returns #f.

; |==============|
; |The Basic Idea|
; |==============|

; Meta-lambda distinguishes between literals, and identifiers it expects
; to be bound to "predicate-like" procedures. These are procedures of one
; argument that return either #t or #f (like the usual scheme
; predicates like symbol? or number?), or another value. 

; As it processes each input-item and the accompanying grammar-item, if
; the grammar-item is a literal that is equal? to the input-item, then
; meta-lambda accepts the match but discards the input item.

; If the grammar-item is a predicate-procedure, then meta-lambda applies
; that procedure to the input-item. If the result is #f, the match
; fails. If the result is #t, meta-lambda saves the input item in an
; internal accumulator-stack. If the result is any other value,
; meta-lambda saves that value in the stack, instead of the input item.

; Then, when and if the list is empty and meta-lambda encounters an
; action (expressed as a lambda expression in the grammar), meta-lambda
; applies that lambda expression to the items in the stack, and returns
; the result. (The "stack" is a list). Thus if a lambda-expression is
; supplied as an action it must contain as many arguments as there were
; predicate-procedures preceeding it.

; Since lambda-expression's denote actions to be taken at the end of a
; match (when the input-list is null), predicate procedures must be
; expressed by writing the identifier they are bound to. (No anonymous
; predicates!)

; You don't have to supply an action. In that case, if the stack is
; empty, meta-lambda returns true. If there is one item on the stack,
; meta-lambda returns that item. Otherwise, it returns the whole stack
; (as a list).

; There are other options, but that's the gist of it.

; (define match-foo-bar
;   (meta-lambda
;    (and 'foo 'bar (lambda () 'tada))))

; (match-foo-bar '(foo bar)) => 'tada
; (match-foo-bar '(3 cat dog)) => #f

; (define match-symbol-number-foo
;   (meta-lambda
;    (and symbol? number? 'foo (lambda (sym num)
; 			       (string-append (symbol->string sym)
; 					      (number->string num))))))

; (match-symbol-number-foo '(cat 3 foo)) => "cat3"
; (match-symbol-number-foo '(cat foo foo)) => #f

; (define both-of-em
;   (meta-lambda
;    (and match-foo-bar match-symbol-number-foo)))

; (both-of-em '((foo bar) (cat 3 foo))) => '(tada "cat3")

; |========|
; |Grammars|
; |========|

; grammar = (or  <grammar> ...)              ;choice
;         | (and <grammer> ...)              ;sequence
;         | (+ <grammar>)                    ;kleene+
;         | (* <grammar>)                    ;kleene*
;         | (?* <grammar>)                   ;kleene?
;         | <literal>                        ;literals
;         | <identifier>                     ;predicate-binding
;         | ()                               ;end-of-list
;         | ?                                ;anything
;         | __                               ;rest-of-list
;         | (<grammar>)                      ;sublist
;         | (unquote <identifier>)           ;location
;         | (unquote-splicing <grammer>)     ;not-a-list
;         | <action>                         ;result action
;         | (else <procedure>)               ;else-clause

; action = (lambda <formals> <body>)
;        | (begin  <sequence>)

; literal = (quote <scheme datum>)
;         | <char>
;         | <number>
;         | <string>

; |==================|
; |The usual suspects|
; |==================|

; choice
; ======

; (or <grammar> ...)

; Try to match each grammar against the input in order. If a match
; fails, backtrack on the input and revert the stack.

; sequence
; ========

; (and <grammer> ...)

; Match each grammar against an item in the input, failing as soon as a
; match fails

; literals
; ========

; 'cat 'dog "three" 34 #\a '(a b c) etc.

; Compare the input item with the literal using equal?, and discard the
; input and proceed if the result is #t, otherwise fail

; identifier
; ==========

; symbol? number? boolean? match-and-do-something

; The identifier should be bound to a procedure of one argument that
; returns one value. If the result of applying the procedure to the next
; input item is #f, then fail. If the result is #t, then save the
; input-item on the stack and proceed. If the result is any other value,
; save that value on the stack in place of the input item, and proceed.

; action
; ======

; (lambda (x y) <stuff>)
; (begin (display "foo") (narfle! garthaks))

; If there is any input remaining, these immediately fail. Otherwise, if
; a "lambda", apply the lambda to the accumulated stack of
; predicate-matched items and return the result. If a "begin", ignore
; the stack and perform the sequence, returning the result.

; |================|
; |Useful additions|
; |================|

; kleene-star
; ===========

; (* <grammar>)

; Match zero or more occurrences of the grammar, and place the list of
; the results on the stack.

; kleene-plus
; ===========

; (* <grammar>)

; Match one or more occurrences of the grammar, and place the list of
; the results on the stack. (If no results than '() is placed on the
; stack).

; kleene?
; ===========

; (?* <grammar>)

; Match zero or one occurrences of the grammar, and place the list of
; the results on the stack, or do nothing.

; anything
; ========

; ?

; Automatically match anything and put it on the input stack.

; rest-of-list
; ============

; __

; Automatically match the rest of a list and place it on the input stack.
; If followed by a lambda-action, it should be a variable arity lambda in order to bind the result of the match of __.

; (define number-and-rest
;   (meta-lambda
;    (and number? __ (lambda (num . rest)
; 		     (cons num (cadr rest))))))

; (number-and-rest '(3 cat dog foo)) => '(3 . dog)

; |=============|
; |Weirder stuff|
; |=============|

; end-of-list
; ===========

; ()

; Explicitly match the end of list and proceed. 

; sub-lists
; =========

; (<grammar>)

; Ah, trees. Wrapping a parens around a grammar causes meta-lambda to
; expect a sublist. It itself can contain actions that return
; values. The sublist is matched and returns results as if you had
; written a separte meta-lambda for the sublist, and whatever it returns
; is placed on the stack as a single item.

; (define match-lambda-one
;   (meta-lambda
;    (and 'lambda (symbol?) ? (lambda (formals body)
; 			      `(forms ,@formals)))))

; (match-lambda-one '(lambda (a) (foo a (bar b c)))) => '(forms . a)

; (define match-lambda
;   (meta-lambda
;    (and 'lambda ((* symbol?)) ? (lambda (formals body)
; 				  `(forms ,@formals)))))

; (match-lambda '(lambda (a b c) (foo a (bar b c)))) => '(forms  a b c)

; location
; ========

; (unquote <identifier>)

; This means match the literal that is bound to the identifier against
; the next input. Useful for parameterizing.

; (define (make-foo-matcher x)
;   (meta-lambda
;    (and 'foo ,x)))

; (define foo-3   (make-foo-matcher 3))
; (define foo-cat (make-foo-matcher 'cat))

; (foo-3 '(foo 3)) => #t
; (foo-3 '(foo 4)) => #f

; (foo-cat '(foo cat)) => #t
; (foo-cat '(foo 3))   => #f

; not-a-list
; ==========

; (unquote-splicing <grammar>)

; Wrap the input (or the next item in the input) in a list, and then
; match. This way meta-lambda can match lists or single items.

; (define infix
;   (let ((op? (meta-lambda   ;doing this for demo purposes. (case ...)
; 			    ;is better here
; 	      (or (and ,@'+ (begin +))
; 		  (and ,@'- (begin -))
; 		  (and ,@'* (begin *))))))
;     (meta-lambda
;      (or ,@integer?
; 	 (and infix op? infix (lambda (a op b) (op a b)))))))

; (infix '((3 + 4) * ((6 - 3) + 4))) => 49

; else
; ====

; (else <procedure>)

; If an else-clause is encountered, the rest of the input is immediately
; accepted, but instead of being accepted on the stack, it is
; immediately passed to <procedure>, which should be variable arity. The
; proedure's result, if it returns at all, becomes the result of the
; whole meta-lambda.

; (define infix2
;   (let ((op? (lambda (y)
; 	       (case y
; 		 ((+) +)
; 		 ((-) -)
; 		 ((*) *)))))
;     (meta-lambda
;      (or ,@integer?
; 	 (and infix op? infix (lambda (a op b) (op a b)))
; 	 (else (lambda x (error "bad input" x)))))))

; (infix2 '((3 + 4) * ((foo - 3) + 4))) => &error bad input (foo)

; |======|
; |Extras|
; |======|

; meta-lambda-dot grammer -> procedure

; Like meta-lambda, but the procedure returned is variable arity as in:

; (lambda x ...)

; The match procedure is applied to the list "x"
