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
;   #(procedure-name source-code arity file-name file-position)
;
; Any of the entries can be #f, and if the tail of the vector is all #f,
; then it may be omitted.

($$trace "procinfo")

(define doc.procedure-name 0)
(define doc.source-code 1)
(define doc.arity 2)
(define doc.file-name 3)
(define doc.file-position 4)

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
                  this
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
(define procedure-source-position (doc-accessor doc.file-position))
(define procedure-expression (doc-accessor doc.source-code))

(define (procedure-documentation-string proc)
  #f)

(define (procedure-environment proc)
  #f)

; eof
