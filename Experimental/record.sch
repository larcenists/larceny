; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Record Package.
;
; Record package as proposed by RRRS authors but never made part 
; of the report.  Based on a proposal posted to rrrs-authors on 
; 1 Sep 89 by Pavel Curtis, reposted to comp.lang.scheme by Norman 
; Adams on 5 Feb, 1992.
;
; I have added some useful features:
;   * Single inheritance of record types (type extension a la Oberon).
;   * Record-type-descriptors are records; this work is not yet
;     complete (*rtd-type* cannot yet be extended).
;
; There are some Larceny-specific features also:
;   * Control over record printing.
;   * The procedure record-type-field-offset can be used with
;     vector-like-ref to optimize field access; this is an
;     experimental facility (may be removed if of little use).
;
; Public interface:
;   (make-record-type type-name field-names &key :parent :printer)  =>  rtd
;   (record-type-descriptor? obj)  =>  boolean
;   (record-type-field-names rtd)  =>  field-names
;   (record-type-name rtd)  =>  type-name
;   (record-type-field-offset rtd field-name)  =>  fixnum
;   (record-type-extends? rtd1 rtd2)  =>  boolean
;
;   (record? obj)  =>  boolean
;   (record-constructor rtd)  =>  procedure
;   (record-constructor rtd field-names)  =>  procedure
;   (record-predicate rtd)  =>  procedure
;   (record-accessor rtd field-name)  =>  procedure
;   (record-updater rtd field-name)  =>  procedure
;   (record-type-descriptor record)  =>  rtd
;
; Features we want
;   Name hiding
;   Better performance -- should in-line some type tests.


; Records.
;
; Records with n fields are represented as structures of length n+1,
; where element 0 contains the record-type-descriptor, and the other
; elements hold the field values.

(define *record-overhead* 1)   ; number of overhead slots.

; True if the object is a record of any type.

(define (record? obj)
  (and (structure? obj)
       (> (vector-like-length obj) 0)
       (record-type-descriptor? (vector-like-ref obj 0))))

; True if the object is a record and its rtd is the given rtd.
;
; Two cases: either the object is of the given type, or it is of
; a subtype of the given type.
;
; ASSUME: rtd is a record type descriptor.
;
; FIXME: Subtype case can be made faster by inlining and combining 
;        tests (note that we assume that record-type-extends? checks
;        that slot0 is an rtd).
; FIXME: This needs to be inlined everywhere for good performance.

(define (record/record-with-type? obj rtd)
  (and (structure? obj)
       (> (vector-like-length obj) 0)
       (let ((slot0 (vector-like-ref obj 0)))
	 (or (eq? rtd slot0)
	     (record-type-extends? slot0 rtd)))))

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
	(let loop ((indices indices) (values values))
	  (cond ((null? indices)
		 (if (not (null? values))
		     (error "Constructor for " rtd
			    ": too many arguments."))
		 record)
		(else
		 (vector-like-set! record (car indices) (car values))
		 (loop (cdr indices) (cdr values)))))))))

(define (record-predicate rtd)
  (if (record-type-descriptor? rtd)
      (lambda (obj)
	(record/record-with-type? obj rtd))
      (error "record-predicate: " rtd " is not a record type descriptor.")))

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


; Record types
;
; A record type descriptor is a record, which requires a little magic.
; Every record type descriptor is of type *rtd-type*.

; Helper procedure.

(define (rtd/make-slot-mapping fields)
  (let loop ((j 0) (r '()) (f fields))
    (if (= j (length fields))
	(reverse r)
	(loop (+ j 1)
	      (cons (cons (car f)
			  (+ j *record-overhead*))
		    r)
	      (cdr f)))))

; BEGIN MAGIC

; (define *rtd-type* 
;   (make-record-type "record-type-descriptor"
;                     '(type-name slot-mapping printer size
;                       hierarchy-vector hierarchy-depth)))
;
; (define record-type-descriptor?
;   (record-predicate *rtd-type*))

(define *rtd-type*
  (let* ((fields '(type-name slot-mapping printer size 
			     hierarchy-vector hierarchy-depth))
	 (x      (make-structure (+ *record-overhead* (length fields)))))
    (vector-like-set! x 0 x)
    (vector-like-set! x 1 "record-type-descriptor-type")
    (vector-like-set! x 2 (rtd/make-slot-mapping fields))
    (vector-like-set! x 3 #f)
    (vector-like-set! x 4 (vector-like-length x))
    (vector-like-set! x 5 (vector x))
    (vector-like-set! x 6 0)
    x))

; FIXME: This won't work if *rtd-type* is extended.
; If *rtd-type* is exported (and its extensions can be used as types) then
; the test must be that obj is an extension of *rtd-type*.

(define (record-type-descriptor? obj)
  (and (structure? obj)
       (> (vector-like-length obj) 0)
       (eq? (vector-like-ref obj 0) *rtd-type*)))

(define (record-type-field-offset rtd name)
  (cdr (assq name (vector-like-ref rtd (+ *record-overhead* 1)))))

; END MAGIC

; The ordering of the procedures is not accidental!

(define rtd/slot-mapping
  (record-accessor *rtd-type* 'slot-mapping))

(define rtd/record-size
  (record-accessor *rtd-type* 'size))

(define rtd/hierarchy-depth
  (record-accessor *rtd-type* 'hierarchy-depth))

(define rtd/hierarchy-vector
  (record-accessor *rtd-type* 'hierarchy-vector))

(define (record-type-field-names rtd)
  (map car (rtd/slot-mapping rtd)))

(define rtd/make-record-type
  (record-constructor *rtd-type*))

; At this point, things are stable.

(define (make-record-type type-name field-names . rest)
  (let* ((printer (cond ((memq 'printer rest) => cadr)
			(else #f)))
	 (parent  (cond ((memq 'parent rest) => cadr)
			(else #f)))
	 (field-names (if parent
			  (append (record-type-field-names parent)
				  field-names)
			  field-names))
	 (hierdep (if parent
		      (+ 1 (rtd/hierarchy-depth parent))
		      0))
	 (hiervec (let ((v (make-vector (+ hierdep 1))))
		    (if parent
			(let ((pv (rtd/hierarchy-vector parent)))
			  (do ((i 0 (+ i 1)))
			      ((= i hierdep))
			    (vector-set! v i (vector-ref pv i)))))
		    v)))
    (let ((rtd (rtd/make-record-type
		type-name
		(rtd/make-slot-mapping field-names)
		printer
		(+ (length field-names) *record-overhead*)
		hiervec
		hierdep)))
      (vector-set! hiervec hierdep rtd)
      rtd)))

(define record-type-name
  (record-accessor *rtd-type* 'type-name))

; Error-checking version.

(define (record-type-field-offset rtd name)
  (let ((probe (assq name (rtd/slot-mapping rtd))))
    (if probe
	(cdr probe)
	(error "record-type-field-offset: " name
	       " is not a valid field for " rtd))))

; Does r1 extend r2, ie, is r1 a subtype of r2?
; r1 extends r2 iff r1.type-hierarcy-vector[ r2.type-hierarchy.depth ] = r2

(define (record-type-extends? r1 r2)
  (let ((r1-vector (rtd/hierarchy-vector r1))
	(r2-depth  (rtd/hierarchy-depth r2)))
    (and (< r2-depth (vector-length r1-vector))
	 (eq? (vector-ref r1-vector r2-depth) r2))))


; Pretty printer for record types.

((record-updater *rtd-type* 'printer)
 *rtd-type*
 (lambda (obj port)
   (display "#<record-type-descriptor " port)
   (display (record-type-name obj) port)
   (display ">" port)))


; Install a printing procedure for records.

(let ((previous-printer (structure-printer))
      (get-printer (record-accessor *rtd-type* 'printer)))
  (structure-printer
   (lambda (obj port quote?)
     (cond ((record? obj)
	    (let ((p (get-printer (record-type-descriptor obj))))
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

