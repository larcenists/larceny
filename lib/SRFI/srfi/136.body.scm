;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Syntactic sentinel

(define-syntax <secret>
  (syntax-rules ()
    ((_) (syntax-error "invalid use of auxiliary syntax" <secret>))))

;;; Syntax

(define-syntax define-record-type-descriptor
  (syntax-rules (root)
    ((_ type-name rtd size indices root (field-spec ...))
     (define-record-type-descriptor type-name rtd size indices #f (field-spec ...)))    
    ((_ type-name rtd size indices parent (field-spec ...))
     (define-syntax type-name
       (syntax-rules (<secret>)
	 ((_ (keyword datum (... ...)))
	  (keyword datum (... ...) parent field-spec ...))
	 ((_ <secret> (k (... ...)))
	  (k (... ...) rtd size indices))
	 ((_) rtd)
	 ((_ . _)
	  (syntax-error "invalid use of record type descriptor" type-name)))))))

(define-record-type-descriptor root
  #f 0 () #f ())

(define-syntax define-record-type
  (syntax-rules ()
    ((_ (type-name #f) . rest)
     (define-record-type (type-name root) . rest))
    ((_ (type-name parent) spec ...)
     (parent <secret> (define-record-type-helper1 type-name spec ... parent)))
    ((_ type-name . rest)
     (define-record-type (type-name root) . rest))))

(define-syntax define-record-type-helper1
  (syntax-rules ()
    ((_ type-name constructor #f . rest)
     (define-record-type-helper1
       type-name constructor predicate-name . rest))
    ((_ type-name (constructor-name . args) predicate-name field-spec ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper2 parent-indices
       type-name (constructor-name . args) predicate-name field-spec ...
       parent parent-rtd parent-size parent-indices))
    ((_ type-name #f predicate-name
	(field-name accessor . mutator*) ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper3 type-name (constructor-name)
       predicate-name
       (field-name accessor . mutator*) ...
       parent parent-rtd parent-size parent-indices))
    ((_ type-name constructor-name predicate-name
	(field-name accessor . mutator*) ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper3 type-name (constructor-name field-name ...)
       predicate-name
       (field-name accessor . mutator*) ...
       parent parent-rtd parent-size parent-indices))))

(define-syntax define-record-type-helper2
  (syntax-rules ()
    ((_ () type-name (constructor-name . args) . rest)
     (define-record-type-helper3
       type-name (constructor-name . args) . rest))
    ((_ (index . index*)
	type-name (constructor-name arg . args) . rest)
     (define-record-type-helper2
       index* type-name (constructor-name . args) . rest))))

(define-syntax define-record-type-helper3
  (syntax-rules ()
    ((_ type-name constructor predicate-name field-spec ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper4 () parent-size
       type-name constructor predicate-name field-spec ...
       parent parent-rtd parent-size parent-indices (field-spec ...)))))

(define-syntax define-record-type-helper4
  (syntax-rules ()
    ((_ (field ...) index
	type-name constructor predicate-name
	parent parent-rtd parent-size parent-indices original-fields)
     (define-record-type-helper5 ()
       type-name index constructor predicate-name field ...
       parent parent-rtd parent-size parent-indices original-fields))
    ((_ (field ...) index
	type-name constructor predicate-name
	(field-name accessor-name) . rest)
     (define-record-type-helper4
       (field ... (field-name accessor-name mutator-name index)) (+ 1 index)
       type-name constructor predicate-name . rest))
    ((_ (field ...) index
	type-name constructor predicate-name
	(field-name accessor-name mutator-name) . rest)
     (define-record-type-helper4
       (field ... (field-name accessor-name mutator-name index)) (+ 1 index)
       type-name constructor predicate-name . rest))
    ((_ . _)
     (syntax-error "bad field specs"))))

(define-syntax define-record-type-helper5
  (syntax-rules ()
    ((_ indices
	type-name size (constructor-name) predicate-name (field-name accessor mutator index) ...
	parent parent-rtd parent-size (parent-index ...) original-fields)
     (begin
       (define-values (type-metadata make-type type? type-ref make-type-subtype)
	 (make-subtype parent-rtd #f))
       (define accessor
	 (make-accessor type-ref index))
       ...
       (define mutator
	 (make-mutator type-ref index))
       ...
       (define rtd
	 (make-rtd 'type-name 'original-fields
		   make-type type? type-ref (list accessor ...) (list mutator ...)
		   make-type-subtype parent-rtd))
       (define predicate-name type?)
       (define constructor-name
	 (make-constructor rtd size (list parent-index ... . indices)))
       (define-record-type-descriptor type-name rtd size (parent-index ... . indices)
	 parent original-fields)))
    ((_ indices
	type-name size (constructor-name . args) predicate-name
	(field-name accessor-name mutator-name index) ...
	parent parent-rtd parent-size parent-indices original-fields)
     (define-record-type-helper6
       ((field-name accessor-name mutator-name index) ...
       (accessor-name field-name mutator-name index) ...)
       indices type-name size (constructor-name . args) predicate-name
       (field-name accessor-name mutator-name index) ...
       parent parent-rtd parent-size parent-indices original-fields))))

(define-syntax define-record-type-helper6
  (syntax-rules ()
    ((_ ((field-name accessor mutator field-index) . fields) (index ...)
	type-name size (constructor-name arg . args) . rest)
     (begin
       (define-syntax m
	 (syntax-rules ()
	   ((m . params)
	    (begin
	      (define-syntax field-name
		(syntax-rules *... ()
		  ((_ *field-index *fields (*index *...)
		       *type-name *size (*constructor-name . *args) . *rest)
		   (define-record-type-helper6 *fields (*index *...)
		     *type-name *size (*constructor-name . *args) . *rest))))
	      (define-syntax ok
		(syntax-rules *... ()		 
		  ((_ *field-index *fields (*index *...)
		       *type-name *size (*constructor-name *arg . *args) . *rest)
		   (define-record-type-helper5
		     (*index *... *field-index)
		     *type-name *size (*constructor-name . *args) . *rest))))
	      (define-syntax test
		(syntax-rules ()
		  ((_ arg . c) (field-name . c))))
	      (test ok . params)))))
       (m field-index fields (index ...) type-name size
	  (constructor-name arg . args) . rest)))
    ((_ fields indices
	type-name size (constructor-name arg . args) . rest)
     (syntax-error "invalid field name in constructor" arg))))

;;; Foundation

(scheme-define-record-type <record-type-descriptor>
  (make-rtd name fieldspecs constructor predicate ref accessors mutators subtyper parent)
  record-type-descriptor?
  (name rtd-name)
  (fieldspecs rtd-fieldspecs)
  (accessors rtd-accessors)
  (mutators rtd-mutators)
  (constructor rtd-constructor)
  (predicate rtd-predicate)
  (ref rtd-ref)
  (subtyper rtd-subtyper)
  (parent rtd-parent))

(define-values (type-metadata %make-type record? type-ref make-type-subtype)
  (make-type #f))

(define (make-subtype rtd payload)
  (if rtd
      ((rtd-subtyper rtd) payload)
      (make-type-subtype payload)))

(scheme-define-record-type <record>
  (%make-record rtd fields)
  %record?
  (rtd record-rtd)
  (fields record-fields))
  
(define (make-constructor rtd size indices)
  (define constructor (rtd-constructor rtd))
  (lambda args
    (unless (= (length args) (length indices))
      (error "unsupported number of arguments in constructor call"))
    (let* ((fields (make-vector size))
	   (record (%make-record rtd fields)))
      (for-each
       (lambda (index arg)
	 (vector-set! fields index arg))
       indices args)
      (constructor record))))

(define (make-accessor ref index)
  (lambda (record)
    (vector-ref (record-fields (ref record)) index)))

(define (make-mutator ref index)
  (lambda (record value)
    (vector-set! (record-fields (ref record)) index value)))

;;; Procedural interface

(define (record-type-descriptor record)
  (record-rtd (type-ref record)))

(define (record-type-predicate rtd)
  (rtd-predicate rtd))

(define (record-type-name rtd)
  (rtd-name rtd))

(define (record-type-parent rtd)
  (rtd-parent rtd))

(define (record-type-fields rtd)
  (let loop ((fieldspecs (rtd-fieldspecs rtd))
	     (accessors (rtd-accessors rtd))
	     (mutators (rtd-mutators rtd)))
    (cond
     ((null? fieldspecs)
      '())
     ((= (length (car fieldspecs)) 3)
      (cons (list (caar fieldspecs) (car accessors) (car mutators))
	    (loop (cdr fieldspecs) (cdr accessors) (cdr mutators))))
     (else
      (cons (list (caar fieldspecs) (car accessors) #f)
	    (loop (cdr fieldspecs) (cdr accessors) (cdr mutators)))))))

(define (make-record-type-descriptor name fieldspecs . parent*)
  (let
      ((fieldspecs (map (lambda (fieldspec)
			  (cond
			   ((symbol? fieldspec)
			    (list fieldspec #f #f))
			   ((not (and (list? fieldspec)
				      (= (length fieldspec) 2)))
			    (error "make-record-type-descriptor: invalid fieldspec" fieldspec))
			   ((eq? (car fieldspec) 'immutable)
			    (list (cadr fieldspec) #f))
			   ((eq? (car fieldspec) 'mutable)
			    (list (cadr fieldspec) #f #f))
			   (else
			    (error "make-record-type-descriptor: invalid fieldspec" fieldspec))))
			fieldspecs))
       (parent-rtd
	(and (not (null? parent*)) (car parent*))))
    (let-values (((type-metadata make-type type? type-ref make-type-subtype)
		  (make-subtype parent-rtd #f)))
      (let*
	  ((parent-size
	    (if parent-rtd (length (rtd-fieldspecs parent-rtd)) 0))	 
	   (accessors 
	    (let loop ((fieldspecs fieldspecs) (index parent-size))
	      (if (null? fieldspecs)
		  '()
		  (cons (make-accessor type-ref index)
			(loop (cdr fieldspecs) (+ 1 index))))))
	   (mutators
	    (let loop ((fieldspecs fieldspecs) (index parent-size))
	      (if (null? fieldspecs)
		  '()
		  (cons (make-accessor type-ref index)
			(loop (cdr fieldspecs) (+ 1 index)))))))
	(make-rtd name fieldspecs make-type type? type-ref
		  accessors mutators
		  make-type-subtype parent-rtd)))))

(define (make-record rtd field-vector)
  ((rtd-constructor rtd) (%make-record rtd field-vector)))
