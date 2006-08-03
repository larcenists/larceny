; macros.scm - Sassy's macro system
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

; Alex Shinn contributed several of these macros, including the "link"
; set for interacting with sassy's "unit" system (which he also
; wrote).


; module macros
; import srfi-69 intern
; export sassy-expand

(define-record-type sassy-lazy-macro
  (make-sassy-lazy-macro itm)
  sassy-lazy-macro?
  (itm sassy-lazy-macro-itm))

(define sassy-expand
  (let
      ((sassy-internal-macros ; permanent macros
	(alist->hash-table
	 `((<       . ,(lambda (a b) `(seq (cmp ,a ,b)  l!)))
	   (<=      . ,(lambda (a b) `(seq (cmp ,a ,b) le!)))
	   (>       . ,(lambda (a b) `(seq (cmp ,a ,b)  g!)))
	   (>=      . ,(lambda (a b) `(seq (cmp ,a ,b) ge!)))
	   (=       . ,(lambda (a b) `(seq (cmp ,a ,b)  e!)))
	   (!=      . ,(lambda (a b) `(seq (cmp ,a ,b) ne!)))
	   (zero?   . ,(lambda (x)   `(seq (test ,x ,x) z!)))
	   (asciiz  . ,(lambda (x)   `(bytes ,x 0)))
	   (alt     . ,(lambda x `(inv (seq ,@(map (lambda (y)
						     `(inv ,y))
						   x)))))
	   (times   . ,(lambda (num e)
			 (cons 'begin (make-list num e))))
	   (until   . ,(lambda (test body) `(while (inv ,test) ,body)))

	   (cs:     . ,(lambda rst `(cs (& ,@rst))))
	   (ds:     . ,(lambda rst `(ds (& ,@rst))))
	   (ss:     . ,(lambda rst `(ss (& ,@rst))))
	   (es:     . ,(lambda rst `(es (& ,@rst))))
	   (fs:     . ,(lambda rst `(fs (& ,@rst))))
	   (gs:     . ,(lambda rst `(gs (& ,@rst))))
	   
	   (_global_offset_table_ . ,(string->symbol "_GLOBAL_OFFSET_TABLE_"))
	   (get-got  . (seq (call $eip)
			    (pop ebx)
			    (add ebx (reloc gotpc _global_offset_table_ 3))))
	   (got-offset   . ,(lambda (sym . vals)
			      `(reloc gotoff ,sym ,(if (null? vals)
						       0
						       (apply + vals)))))
	   (got      . ,(lambda (symbol) `(reloc got32 ,symbol)))
	   (plt      . ,(lambda (symbol) `(reloc plt32 ,symbol)))
	   (sym      . ,(lambda (symbol) `(reloc sym32 ,symbol)))


	   ; All of the following are all thanks to Alex Shinn -Jon
	   (even?    . ,(lambda (x) `(seq (test ,x 1) z!)))
	   (odd?     . ,(lambda (x) `(seq (test ,x 1) nz!)))
	   (positive? . ,(lambda (x) `(seq (cmp ,x 0) g!)))
	   (negative? . ,(lambda (x) `(seq (cmp ,x 0) l!)))
	   
	   ;; Common-Lisp fashion
	   (when . ,(lambda (test . body)
		      `(if ,test (begin ,@body) (begin))))
	   (unless . ,(lambda (test . body)
			`(if ,test (begin) (begin ,@body))))
	   
	   ;; somehow disturbing that on a CISC architecture as
	   ;; baroque as the x86 there isn't a faster way to do this.
	   ;; still better than making two comparisons though.
	   (if3 . ,(lambda (comparator < = >)
		     `(begin ,comparator (if l! ,< (if g! ,> ,=)))))
	   
	   ;; doesn't use WHEN if there's an ELSE clause
	   (cond . ,(lambda (first . rest)
		      (cond
		       ((null? rest)
			(if (eq? 'else (car first))
			    `(begin ,@(cdr first))
			    `(when ,(car first) ,@(cdr first))))
		       ((null? (cdr rest))
			(if (eq? 'else (caar rest))
			    `(if ,(car first)
				 (begin ,@(cdr first))
				 (begin ,@(cdar rest)))
			    `(if ,(car first)
				 (begin ,@(cdr first))
				 (when ,(caar rest)
				       ,@(cdar rest)))))
		       (else
			`(if ,(car first)
			     (begin ,@(cdr first))
			     (cond ,@rest))))))


	   (load . ,(lambda (scheme-file)
		      (if (not (hash-table-exists?
				sassy-loaded-sources scheme-file))
			  (begin
			    (hash-table-walk
			     sassy-dependencies
			     (lambda (f t)
			       (hash-table-set!
				sassy-dependencies f
				(cons scheme-file
				      (hash-table-ref sassy-dependencies f)))))
			    `(! (begin (load ,scheme-file)
				       (hash-table-set!
					sassy-loaded-sources ,scheme-file #t)
				       '(begin))))
			  '(begin))))

	   (link-expand
	    . ,(lambda (unit table)
		 (define (take-up-to n ls proc)
		   (let lp ((n n) (ls ls) (res '()))
		     (if (or (zero? n) (null? ls))
			 (proc (reverse res) ls)
			 (lp (- n 1) (cdr ls) (cons (car ls) res)))))
		 (define (splice-list/begin ls)
		   (let lp ((ls ls) (res '()))
		     (if (null? ls)
			 (cons 'begin (reverse res))
			 (take-up-to 32 ls
				     (lambda (left right)
				       (lp right (cons (cons 'begin left)
						       res)))))))
		 (if (not (hash-table-exists? sassy-linked-units unit))
		     (let ((file (sassy-find-file unit)))
		       (if (hash-table-exists? sassy-linking-files file)
			   (error "sassy: circular linking reference" file))
		       (hash-table-walk
			sassy-linking-files
			(lambda (f t)
			  (hash-table-set!
			   sassy-dependencies f
			   (cons file (hash-table-ref sassy-dependencies f)))))
		       (hash-table-set! sassy-linking-files file #t)
		       (hash-table-set! sassy-dependencies file '())
		       (let* ((sexps (file->sexp-list file))
			      (sexps (if (and (pair? sexps)
					      (equal? '(include "units.sassy")
						      (car sexps)))
					 (cdr sexps)
					 sexps)))
			 `(let-macro ((text (lambda args
					      (hash-table-set! ,table ,file #t)
					      '(begin)))
				      (data (lambda args
					      (hash-table-set! ,table ,file #t)
					      '(begin)))
				      (heap (lambda args '(begin)))
				      (export (lambda args `(import ,@args))))
				     ,(splice-list/begin sexps)
				     (! (begin (hash-table-set!
						sassy-linked-units ,unit ,file)
					       (hash-table-delete!
						sassy-linking-files ,file)
					       '(begin))))))
		     '(begin))))

	   (link . ,(lambda (unit)
		      `(link-expand ,unit sassy-code-sources)))

	   (dynamic-link . ,(lambda (unit)
			      `(link-expand ,unit sassy-dynamic-sources)))

	   (auto-link . ,(lambda (unit . macros)
			   (define (make-loader x)
			     (list 'lambda '_args
				   (list 'list ''begin
					 (list 'list ''link unit)
					 (list 'cons `',x '_args))))
			   (cons
			    'begin
			    (map
			     (lambda (x)
			       (list 'macro x (make-loader x)))
			     macros))))

	   ))))

    ;sassy-user-macros is updated with a new hash-table every time
    ;sassy is called, but since it keeps the last table around,
    ;you can call sassy-expand yourself to see how something got
    ;expanded
    (define sassy-user-macros (make-hash-table))

    (define (make-sassy-macro form)
      (if (and (pair? form) (eq? 'lambda (car form)))
	  (eval form (interaction-environment))
	  form))

    (define (macro? x)
      (and (symbol? x)
	   (let iter ((env sassy-user-macros))
	     (cond
	      ((hash-table? env)
	       (or (hash-table-ref env x (lambda () #f))
		   (hash-table-ref sassy-internal-macros x (lambda () #f))))
	      ((eq? x (caar env)) (cdar env))
	      (else (iter (cdr env)))))))

    (define (call-macro macro-call args)
      (if (sassy-lazy-macro? macro-call)
	  (let ((res (apply (sassy-lazy-macro-itm macro-call) args)))
	    (if (not (pair? res))
		(expand res)
		(let ((maybe (macro? (car res))))
		  (cond ((procedure? maybe)
			 (call-macro maybe (map expand (cdr res))))
			((sassy-lazy-macro? maybe)
			 (call-macro maybe (cdr res)))
			(else
			 (case (car res)
			   ((macro lazy-macro let-macro let-lazy-macro)
			    (expand res))
			   (else res)))))))
	  (expand (if (procedure? macro-call)
		      (apply macro-call args)
		      (cons macro-call args)))))

    (define (do-scheme-call scheme-call)
      (expand (eval scheme-call (interaction-environment))))

    (define (symbol-or-expand  x) (if (pair? x) (expand x) x))

    (define (atom? x) (not (pair? x)))

    (define (include-helper next)
      (let ((t (expand next)))
	(if (or (not t) (eq? t 'void) (null? t))
	    #f
	    t)))

    (define (expand-seq rst)
      (filter (lambda (x) x) (map-in-order include-helper rst)))

    (define (process-macro-includes lst)
      (if (null? lst)
	  '()
	  (let ((itms (cond ((string? (car lst)) (read-file (car lst)))
			    ((symbol? (car lst))
			     (eval (car lst) (interaction-environment)))
			    (else (error "sassy: bad include"
					 (car lst))))))
	    (let ((res (expand-seq itms)))
	      (if (null? res)
		  (process-macro-includes (cdr lst))
		  (append res (process-macro-includes (cdr lst))))))))

    (define (escape x) (and (pair? x) (eq? '! (car x))
			    (not (null? (cdr x)))
			    (null? (cddr x))
			    (cadr x)))

    (define (macro-def kind x) (and (pair? x) (eq? kind (car x))
				    (not (null? (cdr x)))
				    (symbol? (cadr x))
				    (not (null? (cddr x)))
				    (null? (cdddr x))))

    (define (let-macro-form kind x) (and (pair? x) (eq? kind (car x))
					 (not (null? (cdr x)))
					 (pair? (cadr x))))

    (define (expand itm)
      (cond
       ((macro? itm) =>
	(lambda (constant)
	  (if (sassy-lazy-macro? constant)
	      (expand (sassy-lazy-macro-itm constant))
	      (expand constant))))
       
       ((atom? itm) itm)

       
       ((sassy-label-form? itm)
	(let ((label (symbol-or-expand (cadr itm)))
	      (rest  (map-in-order expand (cddr itm))))
	  `(label ,label ,@rest)))
       
       ((sassy-locals-form? itm)
	(let ((locs (map symbol-or-expand (cadr itm)))
	      (rest (map-in-order expand (cddr itm))))
	  `(locals ,locs ,@rest)))
       
       ((escape itm) => do-scheme-call)
       
       ((macro-def 'macro itm)
	(let* ((macro-name (cadr itm))
	       (macro-body (caddr itm))
	       (table (let iter ((t sassy-user-macros))
			(if (hash-table? t)
			    t
			    (iter (cdr t)))))
	       (res (make-sassy-macro macro-body)))
	  (hash-table-set! table macro-name res)
	  'void))

       ((let-macro-form 'let-macro itm)
	(let ((binds (cadr itm))
	      (body  (cddr itm))
	      (old-env sassy-user-macros))
	  (set! sassy-user-macros
		(append
		 (map (lambda (bind)
			(cons (car bind) (make-sassy-macro (cadr bind))))
		      binds)
		 sassy-user-macros))
	  (let ((result (expand-seq body)))
	    (set! sassy-user-macros old-env)
	    (cons 'begin result))))

       ((let-macro-form 'let-lazy-macro itm)
	(let ((binds (cadr itm))
	      (body  (cddr itm))
	      (old-env sassy-user-macros))
	  (set! sassy-user-macros
		(append (map
			 (lambda (bind)
			   (cons (car bind)
				 (make-sassy-lazy-macro
				  (make-sassy-macro (cadr bind)))))
			 binds)
			sassy-user-macros))
	  (let ((result (expand-seq body)))
	    (set! sassy-user-macros old-env)
	    (cons 'begin result))))

       ((macro-def 'lazy-macro itm)
	(let ((macro-name (cadr itm))
	      (macro-body (caddr itm))
	      (table (let iter ((t sassy-user-macros))
		       (if (hash-table? t)
			   t
			   (iter (cdr t))))))
	  (hash-table-set! table macro-name
			   (make-sassy-lazy-macro
			    (make-sassy-macro macro-body)))
	  'void))

       ((and (pair? itm) (macro? (car itm))) =>
	(lambda (macro-call)
	  (let ((args (cdr itm)))
	    (if (sassy-lazy-macro? macro-call)
		(call-macro macro-call args)
		(call-macro macro-call (map expand args))))))

       ((and (pair? itm) (escape (car itm))) =>
	(lambda (scheme-call)
	  (let ((tail (cdr itm))
		(new-head (do-scheme-call scheme-call)))
	    (cond ((macro? new-head) =>
		   (lambda (mac)
		     (if (sassy-lazy-macro? new-head)
			 (call-macro mac tail)
			 (call-macro mac (map expand tail)))))
		  ((procedure? new-head)
		   (call-macro new-head (map expand tail)))
		  (else (cons new-head tail))))))

       ((and (pair? itm) (eq? 'include (car itm)))
	(cons 'begin (process-macro-includes (cdr itm))))

       ((pair? itm)
	(cons (car itm) (map-in-order expand (cdr itm))))

       (else #f)))

    (lambda (list-or-hashtable)
      (if (hash-table? list-or-hashtable)
	  (set! sassy-user-macros list-or-hashtable)
	  (expand list-or-hashtable)))))

