
; Quasiquotations.
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
;   (r (QQ (x . y) m)) => (CONS (r (QQ x) m) (r (QQ y) m))
;   (r (QQ x) m) => (QUOTE x)

(define (stcov-quasiquote expr)

  (define (r e l)
    (let ((opd (cadr e)))
      (cond ((vector? opd)
	     (list->vector (r `(QUASIQUOTE ,(vector->list opd)) l)))
	    ((pair? opd)
	     (cond ((eq? (car opd) 'UNQUOTE)
		    `(UNQUOTE
		      ,(let ((x (cadr opd)))
			 (if (zero? l)
			     (stcov-expand x)
			     ,(r `(QUASIQUOTE ,x) (- l 1))))))
		   ((eq? (car opd) 'QUASIQUOTE)
		    `(QUASIQUOTE 
		      ,(let ((x (cadr opd)))
			 (r `(QUASIQUOTE ,x) (+ l 1)))))
		   ((and (pair? (car opd))
			 (eq? (caar opd) 'UNQUOTE-SPLICING))
		    (let ((x (cadr (car opd)))
			  (y (cdr opd)))
		      (if (zero? l)
			  `(,hyg-append ,x ,(r `(QUASIQUOTE ,y) 0))
			  `(,hyg-list
			    (QUOTE quasiquote)
			    ,`(,hyg-list
			       ,`(,hyg-list 
				  (QUOTE UNQUOTE-SPLICING)
				  ,(r `(QUASIQUOTE ,x) (- l 1)))
			       ,(r `(QUASIQUOTE ,y) l))))))
		   (else
		    (let ((x (car opd))
			  (y (cdr opd)))
		      `(,hyg-cons 
			,(r `(QUASIQUOTE ,x) l) 
			,(r `(QUASIQUOTE ,y) l))))))
	    (else
	     `(QUOTE ,opd)))))

  `(QUASIQUOTE ,(r expr 0)))
