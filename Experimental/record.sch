; Experimental/record.sch
; Larceny library -- Record Package
;
; $Id$
;
; Record package as proposed by RRRS authors but never made part of the
; report.  Based on a proposal posted to rrrs-authors on 01 Sep 89 by 
; Pavel Curtis, reposted to comp.lang.scheme by Norman Adams on 5 Feb, 1992.
;
; While that proposal is preserved in its full form, I have added some
; useful features.  See the on-line documentation.
;
; Features we have
;   Record printing procedure
;   Optimized access with `record-type-field-offset'
;
; Features we want
;   Inheritance
;   Name hiding
;   If record-type-descriptors are records, then some operations can be
;     simplified -- the print procedure is just stored in a slot, for 
;     example.  And there seems to be no performance reasons this should
;     not be done
;   Better performance -- should in-line some type tests.

; Records.
;
; Records with n fields are represented as structures of length n+1,
; where element 0 contains the record-type-descriptor, and the other
; elements hold the field values.

(define *record-overhead* 1)   ; number of overhead slots.

; True if the object is a record of any type.
; This needs to be inlined everywhere for good performance.

(define (record? obj)
  (and (structure? obj)
       (> (vector-like-length obj) 0)
       (record-type-descriptor? (vector-like-ref obj 0))))

; True if the object is a record and its rtd is the given rtd.
; This needs to be inlined everywhere for good performance.

(define (record/record-with-type? obj rtd)
  (and (record? obj)
       (eq? (record-type-descriptor obj) rtd)))

(define (record-constructor rtd . rest)
  (let* ((fields  (if (null? rest)
		      (record-type-field-names rtd)
		      (car rest)))
	 (indices (map (lambda (name)
			 (record-type-field-offset rtd name))
		       fields)))
    (lambda values
      (let ((record (make-structure (rtd/record-size rtd))))
	(vector-like-set! record 0 rtd)
	(for-each (lambda (i v)
		    (vector-like-set! record i v))
		  indices
		  values)
	record))))

(define (record-predicate rtd)
  (if (record-type-descriptor? rtd)
      (lambda (obj)
	(record/record-with-type? obj rtd))
      (error 'record-predicate "Not a record type descriptor")))

(define (record-accessor rtd field-name)
  (let ((i (record-type-field-offset rtd field-name)))
    (lambda (obj)
      (if (record/record-with-type? obj rtd)
	  (vector-like-ref obj i)
	  (error "record-accessor: " obj " is not of type " rtd)))))

(define (record-updater rtd field-name)
  (let ((i (record-type-field-offset rtd field-name)))
    (lambda (obj val)
      (if (record/record-with-type? obj rtd)
	  (vector-like-set! obj i val)
	  (error "record-updater: " obj " is not of type " rtd)))))

(define (record-type-descriptor record)
  (if (record? record)
      (vector-like-ref record 0)
      (error "record-type-descriptor: " obj " is not a record.")))


; Record type descriptors.
;
; A record-type-descriptor is a structure where the first element is a
; distinguished object.
;
; Make-record-type takes additional keyword parameters, where the keywords
; are just symbols.  Currently recognized are:
;
;   keyword   value
;   -------   -----
;   printer   printing procedure  (takes object and output port)

(define (make-record-type type-name field-names . rest)

  (define (make-field-info names index)
    (if (null? names)
	'()
	(cons (cons (car names) index)
	      (make-field-info (cdr names) (+ index 1)))))

  (define printer #f)
  (define parent #f)

  ; Parse rest arguments.

  (let loop ((rest rest))
    (cond ((null? rest))
	  ((eq? (car rest) 'printer)
	   (set! printer (cadr rest))
	   (loop (cddr rest)))
	  ((eq? (car rest) 'parent)
	   (set! parent (cadr rest))
	   (loop (cddr rest)))
	  (else
	   (error "make-record-type: invalid keyword " (car rest)))))

  ; Create descriptor.

  (let ((rtd (make-structure 5)))
    (vector-like-set! rtd 0 rtd/tag)
    (vector-like-set! rtd 1 type-name)                         ; name
    (vector-like-set! rtd 2 (make-field-info field-names       ; field info
					     *record-overhead*))
    (vector-like-set! rtd 3 printer)                           ; print proc
    (vector-like-set! rtd 4 (+ (length field-names)            ; # fields
			       *record-overhead*))
    rtd))

(define rtd/tag (list 'record-type-descriptor))

(define (rtd/field-info rtd) (vector-like-ref rtd 2))
(define (rtd/printer rtd) (vector-like-ref rtd 3))
(define (rtd/record-size rtd) (vector-like-ref rtd 4))

(define (record-type-descriptor? obj) 
    (and (structure? obj) 
	 (> (vector-like-length obj) 0)
	 (eq? (vector-like-ref obj 0) rtd/tag)))

(define (record-type-name rtd)
  (if (record-type-descriptor? rtd)
      (vector-like-ref rtd 1)
      (error "record-type-name: " rtd " is not a record-type-descriptor.")))

(define (record-type-field-names rtd)
  (if (record-type-descriptor? rtd)
      (map car (rtd/field-info rtd))
      (error 'record-type-field-names "Not a record type descriptor")))

(define (record-type-field-offset rtd name)
  (let ((probe (assq name (rtd/field-info rtd))))
    (if probe
	(cdr probe)
	(error "record-type-field-offset: " name
	       " is not a valid field for " rtd))))

; #t iff r1 extends r2

(define (record-type-extends? r1 r2)
  #f)

; Install a printing procedure for records.

(let ((previous-printer (structure-printer)))
  (structure-printer
   (lambda (obj port quote?)
     (cond ((record-type-descriptor? obj)
	    (display "#<record-type-descriptor " port)
	    (display (record-type-name obj) port)
	    (display ">" port))
	   ((record? obj)
	    (let ((p (rtd/printer (record-type-descriptor obj))))
	      (if p
		  (p obj port)
		  (begin (display "#<record " port)
			 (display (record-type-name
				   (record-type-descriptor obj))
				  port)
			 (display ">" port)))))
	   (else
	    (previous-printer obj port quote?))))))

; eof

