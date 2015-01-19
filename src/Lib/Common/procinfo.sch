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
; The file-position may be one of several things:
;     #f
;     an exact non-negative integer specifying
;         the zero-origin offset in characters
;     a vector of three exact non-negative integers specifying
;         (0) the zero-origin offset in characters
;         (1) the zero-origin line number
;         (2) the zero-origin column number
;     a pair (not a list) of two such vectors specifying both
;         the beginning (car) and end (cdr)

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

; For safety and simplicity, this procedure mutates a copy of the
; constant vector, and will only replace a vector with a vector.

(define (procedure-documentation-set! p newdoc)
  (let* ((cv (procedure-ref p 1))
	 (ds (if (vector? cv) (vector-ref cv 0) #f)))
    (cond ((and (vector? ds)
                (vector? newdoc)
                (= (vector-length ds) (vector-length newdoc)))
           (let ((newcv (vector-copy cv)))
             (vector-set! newcv 0 newdoc)
             (procedure-set! p 1 newcv)))
          ((and (list? ds)
                (vector? newdoc)
                (for-all pair? ds)
                (for-all fixnum? (map car ds))
                (for-all vector? (map cdr ds))
                (= (vector-length (cdr (car ds)))
                   (vector-length newdoc))
                (= 0 (car (car ds)))
                #t)
           (let ((newcv (vector-copy cv)))
             (vector-set! newcv 0 (cons (cons 0 newdoc) (cdr ds)))
             (procedure-set! p 1 newcv))))
    #f))

(define (doc-accessor x)
  (lambda (proc)
    (cond ((procedure? proc)
	   (let ((doc (procedure-documentation proc)))
	     (if (and (vector? doc) (< x (vector-length doc)))
		 (vector-ref doc x)
		 #f)))
	  ((not proc) proc)
	  (else
           (assertion-violation 'doc-accessor (errmsg 'msg:notproc) proc)))))

; It's dangerous to mutate a possibly shared vector of documentation,
; so this is no longer used.  See ticket #643.

;(define (doc-mutator x)
;  (lambda (proc newval)
;    (cond ((procedure? proc)
;           (let ((doc (procedure-documentation proc)))
;             (if (and (vector? doc) (< x (vector-length doc)))
;                 (vector-set! doc x newval)
;                 #f)))
;          (else
;           (assertion-violation 'doc-mutator (errmsg 'msg:notproc) proc)))))

(define procedure-arity (doc-accessor doc.arity))
(define procedure-name (doc-accessor doc.procedure-name))
(define procedure-source-file-larceny (doc-accessor doc.file-name))
(define procedure-source-position-larceny (doc-accessor doc.file-position))
(define procedure-expression (doc-accessor doc.source-code))
(define procedure-formals (doc-accessor doc.formals))

(define (procedure-name-set! proc newval)
  (cond ((procedure? proc)
         (let ((doc (procedure-documentation proc)))
           (if (and (vector? doc)
                    (< doc.procedure-name (vector-length doc)))
               (procedure-documentation-set! proc
                                             (vector-set! (vector-copy doc)
                                                          doc.procedure-name
                                                          newval))
               #f)))
        (else
         (assertion-violation 'procedure-name-set!
                              (errmsg 'msg:notproc)
                              proc))))


(define (procedure-source-file proc)
  (let ((sf (procedure-source-file-larceny proc)))
    (cond ((symbol? sf)
           (symbol->string sf))
          ((string? sf)
           sf)
          (else
           #f))))

(define (procedure-source-position proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((fixnum? k) k)
          ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 0))
          ((and (pair? k)
                (let ((k (car k)))
                  (and (vector? k) (= 3 (vector-length k)))))
           (vector-ref (car k) 0))
          (else #f))))

(define (procedure-source-line proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 1))
          ((and (pair? k)
                (let ((k (car k)))
                  (and (vector? k) (= 3 (vector-length k)))))
           (vector-ref (car k) 1))
          (else #f))))

(define (procedure-source-column proc)
  (let ((k (procedure-source-position-larceny proc)))
    (cond ((and (vector? k) (= 3 (vector-length k)))
           (vector-ref k 2))
          ((and (pair? k)
                (let ((k (car k)))
                  (and (vector? k) (= 3 (vector-length k)))))
           (vector-ref (car k) 2))
          (else #f))))

(define (procedure-documentation-string proc)
  #f)

(define (procedure-environment proc)
  #f)

; The name of the current source file is a parameter.

(define current-source-file
  (make-parameter "current-source-file"
                  #f
                  (lambda (x)
                    (or (eq? x #f)
                        (string? x)
                        (symbol? x)))))

; Source locations are recorded by calling the current value
; of the source-location-recorder parameter, whose argument
; is a position table as returned by get-datum-with-source-locations
;
; The macro expander or compiler can assign a useful value
; to that parameter; its initial value does nothing.

(define source-location-recorder
  (make-parameter "source-location-recorder"
                  (lambda (position-table) #t)
                  procedure?))

; eof
