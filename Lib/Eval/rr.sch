; Rewrites quasiquotations. Painfully.
; lth@cs.uoregon.edu / April 29, 1992
;
; Notation:
;
;   0 <= m <= infty
;   1 <= n <= infty
;   v is a vector
;   Caps indicate literals. Lower case indicates actual code.
;   Aliases: QQ = quasiquote
;            UNQ = unquote
;            UNQS = unquote-splicing
;            L->V = list->vector
;            v->l = vector->list
;
; Rewrite Rules (apply matching top-down, from the outside and in):
;
;   (r (QQ v) 0) => (L->V (r (QQ (v->l v)) 0)))
;   (r (QQ v) n) => (LIST (QUOTE QQ) (L->V (r (QQ (v->l v)) n))))
;   (r (QQ (UNQ x) 0)) => x
;   (r (QQ (UNQ x) n)) => (LIST (QUOTE UNQ) (r (QQ x) (- n 1)))
;   (r (QQ (QQ x) m)) => (LIST (QUOTE QQ) (r (QQ x) (+ m 1)))
;   (r (QQ ((UNQS x) . y)) 0) => (APPEND x (r (QQ y) 0))
;   (r (QQ ((UNQS x) . y)) n) => (LIST (QUOTE QQ)
;                                      (LIST (LIST (QUOTE UNQS) 
;                                            (r (QQ x) (- n 1)))
;                                      (r (QQ y) n)))
;   (r (QQ (QUOTE x)) m) => (QUOTE x)
;   (r (QQ (x . y) m)) => (CONS (r (QQ x) m) (r (QQ y) m))
;   (r (QQ x) m) => (QUOTE x)
;
; Notes:
;
;   Not terribly robust.
;   One could write a set of rules which would expand to more efficient code,
;   using literals whereever possible. Here, we always expand to runnable
;   code in almost all cases, resulting in less efficiency. A decent source-
;   level peephole optimizer can recover many of the literals, however.

; Rewrite procedure.

(define (rewrite-quasiquotation expr)

  ; "Safe" names of procedures we use. Needs improvement, but good 
  ; enough for now.

  (define %vector '%vector)
  (define %list '%list)
  (define %cons '%cons)
  (define %append '%append)
  (define %list->vector '%list->vector)

  (define (r e l)
    (cond ((vector? (cadr e))
	   (let ((v (cadr e)))
	     (if (zero? l)
		 (list %list->vector (r (list 'QUASIQUOTE (vector->list v)) l))
		 (list %list
		       'QUASIQUOTE
		       (list %list->vector 
			     (r (list 'QUASIQUOTE (vector->list v)) l))))))
	  ((pair? (cadr e))
	   (cond ((eq? (car (cadr e)) 'UNQUOTE)
		  (let ((x (cadr (cadr e))))
		    (if (zero? l)
			x
			(list %list '(QUOTE UNQUOTE)
			      (r (list 'QUASIQUOTE x) (- l 1))))))
		 ((eq? (car (cadr e)) 'QUASIQUOTE)
		  (let ((x (cadr (cadr e))))
		    (list %list 
			  '(QUOTE QUASIQUOTE)
			  (r (list 'QUASIQUOTE x) (+ l 1)))))
		 ((and (pair? (car (cadr e)))
		       (eq? (caar (cadr e)) 'UNQUOTE-SPLICING))
		  (let ((x (cadr (car (cadr e))))
			(y (cdr (cadr e))))
		    (if (zero? l)
			(list %append x (r (list 'QUASIQUOTE y) 0))
			(list %list
			      '(QUOTE quasiquote)
			      (list %list
				    (list %list 
					  '(QUOTE UNQUOTE-SPLICING)
					  (r (list 'QUASIQUOTE x) (- l 1)))
				    (r (list 'QUASIQUOTE y) l))))))
		 ((eq? (car (cadr e)) 'QUOTE)
		  (cadr e))
		 (else
		  (let ((x (car (cadr e)))
			(y (cdr (cadr e))))
		    (list %cons 
			  (r (list 'QUASIQUOTE x) l) 
			  (r (list 'QUASIQUOTE y) l))))))
	  (else
	   (let ((x (cadr e)))
	     (list 'QUOTE x)))))

  (r expr 0))

