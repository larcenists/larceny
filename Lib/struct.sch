; Lib/struct.sch
; Larceny library -- structures.
;
; $Id: struct.sch,v 1.1 1997/09/23 20:08:39 lth Exp lth $

($$trace "struct")

(define (make-structure size)
  (let ((s (make-vector size #f)))
    (typetag-set! s sys$tag.structure-typetag)
    s))

; structure? is now integrable.

;(define (structure? obj)
;  (and (vector-like? obj)
;       (eq? (typetag obj) sys$tag.structure-typetag)))

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

; When equal? sees two structure arguments, it calls `structure-comparator'
; which returns the current structure comparator procedure.  The programmer
; may install a new structure comparator procedure by calling 
; structure-comparator with an appropriate procedure.  An appropriate procedure
; takes two arguments: the structures to be compared, and returns a boolean:
; #t if they are equal, #f if they are not.  The default is that two
; structures are equal if they are of the same length and all their fields
; are equal?.

(define *default-structure-comparator*
  (lambda (obj1 obj2)
    (let ((l1 (vector-like-length obj1))
	  (l2 (vector-like-length obj2)))
      (if (not (= l1 l2))
	  #f
	  (let loop ((i 0))
	    (cond ((= i l1))
		  ((not (equal? (vector-like-ref obj1 i)
				(vector-like-ref obj2 i)))
		   #f)
		  (else
		   (loop (+ i 1)))))))))

(define *structure-comparator* *default-structure-comparator*)

(define (structure-comparator . rest)
  (cond ((null? rest)
	 *structure-comparator*)
	((null? (cdr rest))
	 (set! *structure-comparator* (car rest))
	 *structure-comparator*)
	(else
	 (error "structure-comparator: too many arguments.")
	 #t)))

; eof
