; Lib/procinfo.sch
; Larceny library -- heuristic procedure information
;
; $Id: procinfo.sch,v 1.1.1.1 1998/11/19 21:52:10 lth Exp $
;
; Based on a proposal by Will Clinger to the rrrs-authors mailing list,
; with one addition proposed by Aubrey Jaffer.
;
; Old documentation format (pre-v0.31)
;   The documentation slot contains either
;     a symbol -- the procedure name.
;     a list beginning with 'lambda' -- the procedure source code.
;     a list beginning with a symbol -- the procedure name followed by
;       the source code without 'lambda'.
;     a list beginning with something else -- don't know.
;     something else -- don't know.
;
; New documentation format (v0.31)
;   a vector --  #(procedure-name source-code arity file-name file-position)
;     Any of the entries can be #f.
;   something else -- either old-format (still supported) or don't know.

($$trace "procinfo")

(define (procedure-arity proc)
  (let ((doc (vector-ref (procedure-ref proc 1) 0)))
    (cond ((and (pair? doc) (symbol? (car doc)))
	   (let loop ((i 0) (p (cadr doc)))
	     (cond ((pair? p)
		    (loop (+ i 1) (cdr p)))
		   ((symbol? p)
		    (exact->inexact i))
		   (else
		    #f))))
	  ((vector? doc) (vector-ref doc 2))
	  (else
	   #f))))

(define (procedure-formals proc)
  (let ((doc (vector-ref (procedure-ref proc 1) 0)))
    (cond ((and (pair? doc) (symbol? (car doc)))
	   (cadr doc))
	  ((and (vector? doc) (vector-ref doc 1))
	   (cadr (vector-ref doc 1)))
	  (else
	   #f))))

(define (procedure-documentation-string proc)
  #f)

(define (procedure-name proc)
  (let ((x (vector-ref (procedure-ref proc 1) 0)))
    (cond ((symbol? x) x)
	  ((and (pair? x) (symbol? (car x)) (not (eq? (car x) 'lambda)))
	   (car x))
	  ((vector? x)
	   (vector-ref x 0))
	  (else #f))))
     
(define (procedure-source-file proc)
  (let ((doc (vector-ref (procedure-ref proc 1) 0)))
    (cond ((vector? doc)
	   (vector-ref doc 3))
	  (else
	   #f))))

(define (procedure-source-position proc)
  (let ((doc (vector-ref (procedure-ref proc 1) 0)))
    (cond ((vector? doc)
	   (vector-ref doc 4))
	  (else
	   #f))))

(define (procedure-expression proc)
  (let ((doc (vector-ref (procedure-ref proc 1) 0)))
    (cond ((and (pair? doc) (symbol? (car doc)))
	   (if (eq? (car doc) 'lambda)
	       doc
	       (cons 'lambda (cdr doc))))
	  ((vector? doc)
	   (vector-ref doc 1))
	  (else
	   #f))))

(define (procedure-environment proc)
  #f)

; eof
