; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- heuristic procedure information.
; Based on a proposal by Will Clinger to the rrrs-authors mailing list.
;
; Documentation format is documented in Larceny Note #12.
; Summary:  documentation structure is a vector:
;
;   #(procedure-name source-code arity file-name file-position formals)
;
; Any of the entries can be #f, and if the tail of the vector is all #f,
; then it may be omitted.
;
; The file-position may be #f, or an exact non-negative integer,
; or a vector of three exact non-negative integers specifying
; (0) the zero-origin offset in characters, (2) the zero-origin
; line number, and (3) the zero-origin column number.
;
; FIXME: source positions should specify both start and end.

($$trace "procinfo")

(define doc.procedure-name 0)
(define doc.source-code 1)
(define doc.arity 2)
(define doc.file-name 3)
(define doc.file-position 4)
(define doc.formals 5)

; PROC[1] is the constant vector.
; CV[0] is the documentation slot.
;
; The documentation slot contains either a single vector, a list 
; of pairs, or #f.  If a single vector or #f, that is the documentation.
; Otherwise, the list is sorted in ascending order on the car of the 
; pairs, which represent starting code vector addresses for internal
; procedures; each cdr is the documentation for one internal procedure.
;
; Not well-documented: the entire constant vector may sometimes be
; missing, in some system thunks.  So be careful.

(define (procedure-documentation p . rest)
  (let* ((pc (if (null? rest) 0 (car rest)))
	 (cv (procedure-ref p 1))
	 (ds (if (vector? cv) (vector-ref cv 0) #f)))
    (if (pair? ds)
	(let loop ((dsl ds) (this (car ds)))
	  (if (null? (cdr dsl))
	      (if (<= (car this) pc)
		  (cdr this)
		  #f)
	      (let ((next (cadr dsl)))
		(cond ((and (<= (car this) pc) (< pc (car next)))
		       (cdr this))
		      ((>= pc (car next))
		       (loop (cdr dsl) next))
		      (else
		       (cdr this))))))
	ds)))

(define (doc-accessor x)
  (lambda (proc)
    (cond ((procedure? proc)
	   (let ((doc (procedure-documentation proc)))
	     (if (and (vector? doc) (< x (vector-length doc)))
		 (vector-ref doc x)
		 #f)))
	  ((not proc) proc)
	  (else (error "doc-accessor: " proc " is not a procedure or #f.")))))

(define procedure-arity (doc-accessor doc.arity))
(define procedure-name (doc-accessor doc.procedure-name))
(define procedure-source-file (doc-accessor doc.file-name))
(define procedure-source-position-larceny (doc-accessor doc.file-position))
(define procedure-expression (doc-accessor doc.source-code))
(define procedure-formals (doc-accessor doc.formals))

(define (procedure-source-position proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((fixnum? k) k)
          ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 0))
          (else #f))))

(define (procedure-source-line proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 1))
          (else #f))))

(define (procedure-source-column proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 2))
          (else #f))))

(define (procedure-documentation-string proc)
  #f)

(define (procedure-environment proc)
  #f)

; eof
