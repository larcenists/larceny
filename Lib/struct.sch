; Lib/struct.sch
; Larceny library -- structures.
;
; $Id: struct.sch,v 1.1 1997/09/23 20:08:39 lth Exp lth $

(define (make-structure size)
  (let ((s (make-vector size #f)))
    (typetag-set! s sys$tag.structure-typetag)
    s))

; structure? needs to be integrable / a primop.

(define (structure? obj)
  (and (vector-like? obj)
       (eq? (typetag obj) sys$tag.structure-typetag)))

; When the printer receives a structure, it calls `structure-printer' which
; returns the current structure printer procedure.  The programmer may
; install a new structure printer procedure by calling structure-printer
; with an appropriate procedure.  An appropriate procedure takes three
; arguments: the object to be printed, the output port, and whether the
; representation should be quoted (as for `write') or not (as for `display').

(define *default-structure-printer*
  (lambda (obj port quote?)
    (display "#<STRUCTURE>" port)))

(define *structure-printer* *default-structure-printer*)

(define (structure-printer . rest)
  (cond ((null? rest)
	 *structure-printer*)
	((null? (cdr rest))
	 (set! *structure-printer* (car rest))
	 *structure-printer*)
	(else
	 (error "structure-printer: too many arguments.")
	 #t)))

; eof
