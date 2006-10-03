;;;;"array.scm" Arrays for Scheme
;
; $Id$
; From slib 2a7, slightly adapted to Larceny.  
;
; To do:
; - Replace array-1d-ref etc by a general array-fast-ref that 
;   uses CASE-LAMBDA, ditto for setters.  Requires CASE-LAMBDA.
; - Actually hide the array: names.
;
; Copyright (C) 1993 Alan Bawden
;
; Permission to copy this software, to redistribute it, and to use it
; for any purpose is granted, subject to the following restrictions and
; understandings.
;
; 1.  Any copy made of this software must include this copyright notice
; in full.
;
; 2.  Users of this software agree to make their best efforts (a) to
; return to me any improvements or extensions that they make, so that
; these may be included in future releases; and (b) to inform me of
; noteworthy uses of this software.
;
; 3.  I have made no warrantee or representation that the operation of
; this software will be error-free, and I am under no obligation to
; provide any services, by way of maintenance, update, or otherwise.
;
; 4.  In conjunction with products arising from the use of this material,
; there shall be no use of my name in any advertising, promotional, or
; sales literature without prior written consent in each case.
;
; Alan Bawden
; MIT Room NE43-510
; 545 Tech. Sq.
; Cambridge, MA 02139
; Alan@LCS.MIT.EDU
;
; The user interface consists of the following 9 functions
; 
;   (ARRAY? <object>)  =>  <boolean>
;   (MAKE-ARRAY <initial-value> <bound> <bound> ...)  =>  <array>
;   (ARRAY-REF <array> <index> <index> ...)  =>  <value>
;   (ARRAY-SET! <array> <new-value> <index> <index> ...)
;   (MAKE-SHARED-ARRAY <array> <mapper> <bound> <bound> ...)  =>  <array>
;   (ARRAY-SHAPE <array>)  => ((<lo> <hi>) (<lo> <hi>) ...)
;   (ARRAY-RANK <array>) => <number>
;   (ARRAY-DIMENSIONS <array>) => (<bound> <bound> ...)
;   (ARRAY-IN-BOUNDS? <array> <index> <index> ...)
;
; When constructing an array, <bound> is either an inclusive range of
; indices expressed as a two element list, or an upper bound expressed as a
; single integer.  So
;
;   (make-array 'foo 3 3)
;
; and
;
;   (make-array 'foo '(0 2) '(0 2))
;
; are equivalent.
;
; MAKE-SHARED-ARRAY can be used to create shared subarrays of other arrays.
; The <mapper> is a function that translates coordinates in the new array
; into coordinates in the old array.  A <mapper> must be linear, and its
; range must stay within the bounds of the old array, but it can be
; otherwise arbitrary.  A simple example:
;
;   (define fred (make-array #F 8 8))
;   (define freds-diagonal
;     (make-shared-array fred (lambda (i) (list i i)) 8))
;   (array-set! freds-diagonal 'foo 3)
;   (array-ref fred 3 3)  =>  FOO
;   (define freds-center
;     (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j))) 2 2))
;   (array-ref freds-center 0 0)  =>  FOO
;
; End of manual.

(require 'record)

;(declare (usual-integrations))
;(eval-when (compile load) (compiler-switches 'fast-safe))

(define array:rtd
  (make-record-type "Array"
    '(indexer		; Must be a -linear- function!
      shape		; Inclusive bounds: ((lower upper) ...)
      vector		; The actual contents
      )))

(define array:indexer (record-accessor array:rtd 'indexer))
(define array-shape (record-accessor array:rtd 'shape))
(define array:vector (record-accessor array:rtd 'vector))

(define array? (record-predicate array:rtd))

(define (array-rank obj)
  (if (array? obj) (length (array-shape obj)) 0))

(define (array-dimensions ra)
  (map (lambda (ind) (if (zero? (car ind)) (cadr ind) ind))
       (array-shape ra)))

(define array:construct
  (record-constructor array:rtd '(shape vector indexer)))

(define (array:compute-shape specs)
  (map (lambda (spec)
	 (cond ((and (integer? spec)
		     (< 0 spec))
		(list 0 (- spec 1)))
	       ((and (pair? spec)
		     (pair? (cdr spec))
		     (null? (cddr spec))
		     (integer? (car spec))
		     (integer? (cadr spec))
		     (<= (car spec) (cadr spec)))
		spec)
	       (else (error "array: Bad array dimension: " spec))))
       specs))

(define (make-array initial-value . specs)
  (let ((shape (array:compute-shape specs)))
    (let loop ((size 1)
	       (indexer (lambda () 0))
	       (l (reverse shape)))
      (if (null? l)
	  (array:construct shape
			   (make-vector size initial-value)
			   (array:optimize-linear-function indexer shape))
	  (loop (* size (+ 1 (- (cadar l) (caar l))))
		(lambda (first-index . rest-of-indices)
		  (+ (* size (- first-index (caar l)))
		     (apply indexer rest-of-indices)))
		(cdr l))))))

(define (make-shared-array array mapping . specs)
  (let ((new-shape (array:compute-shape specs))
	(old-indexer (array:indexer array)))
    (let check ((indices '())
		(bounds (reverse new-shape)))
      (cond ((null? bounds)
	     (array:check-bounds array (apply mapping indices)))
	    (else
	     (check (cons (caar bounds) indices) (cdr bounds))
	     (check (cons (cadar bounds) indices) (cdr bounds)))))
    (array:construct new-shape
		     (array:vector array)
		     (array:optimize-linear-function
		       (lambda indices
			 (apply old-indexer (apply mapping indices)))
		       new-shape))))

(define (array:in-bounds? array indices)
  (let loop ((indices indices)
	     (shape (array-shape array)))
    (if (null? indices)
	(null? shape)
	(let ((index (car indices)))
	  (and (not (null? shape))
	       (integer? index)
	       (<= (caar shape) index (cadar shape))
	       (loop (cdr indices) (cdr shape)))))))

(define (array:check-bounds array indices)
  (or (array:in-bounds? array indices)
      (error "array: Bad indices for " array indices)))

(define (array-ref array . indices)
  (array:check-bounds array indices)
  (vector-ref (array:vector array)
	      (apply (array:indexer array) indices)))

(define (array-set! array new-value . indices)
  (array:check-bounds array indices)
  (vector-set! (array:vector array)
	       (apply (array:indexer array) indices)
	       new-value))

(define (array-in-bounds? array . indices)
  (array:in-bounds? array indices))

; Fast versions of ARRAY-REF and ARRAY-SET! that do no error checking,
; and don't cons intermediate lists of indices:

(define (array-1d-ref a i0)
  (vector-ref (array:vector a) ((array:indexer a) i0)))

(define (array-2d-ref a i0 i1)
  (vector-ref (array:vector a) ((array:indexer a) i0 i1)))

(define (array-3d-ref a i0 i1 i2)
  (vector-ref (array:vector a) ((array:indexer a) i0 i1 i2)))

(define (array-1d-set! a v i0)
  (vector-set! (array:vector a) ((array:indexer a) i0) v))

(define (array-2d-set! a v i0 i1)
  (vector-set! (array:vector a) ((array:indexer a) i0 i1) v))

(define (array-3d-set! a v i0 i1 i2)
  (vector-set! (array:vector a) ((array:indexer a) i0 i1 i2) v))

; STOP!  Do not read beyond this point on your first reading of
; this code -- you should simply assume that the rest of this file
; contains only the following single definition:
;
;   (define (array:optimize-linear-function f l) f)
;
; Of course everything would be pretty inefficient if this were really the
; case, but it isn't.  The following code takes advantage of the fact that
; you can learn everything there is to know from a linear function by
; simply probing around in its domain and observing its values -- then a
; more efficient equivalent can be constructed.

(define (array:optimize-linear-function f l)
  (let ((d (length l)))
    (cond
     ((= d 0)
      (array:0d-c (f)))
     ((= d 1)
      (let ((c (f 0)))
	(array:1d-c0 c (- (f 1) c))))
     ((= d 2)
      (let ((c (f 0 0)))
	(array:2d-c01 c (- (f 1 0) c) (- (f 0 1) c))))
     ((= d 3)
      (let ((c (f 0 0 0)))
	(array:3d-c012 c (- (f 1 0 0) c) (- (f 0 1 0) c) (- (f 0 0 1) c))))
     (else
      (let* ((v (map (lambda (x) 0) l))
	     (c (apply f v)))
	(let loop ((p v)
		   (old-val c)
		   (coefs '()))
	  (cond ((null? p)
		 (array:Nd-c* c (reverse coefs)))
		(else
		 (set-car! p 1)
		 (let ((new-val (apply f v)))
		   (loop (cdr p)
			 new-val
			 (cons (- new-val old-val) coefs)))))))))))

; 0D cases:

(define (array:0d-c c)
  (lambda () c))

; 1D cases:

(define (array:1d-c c)
  (lambda (i0) (+ c i0)))

(define (array:1d-0 n0)
  (cond ((= 1 n0) (lambda (i0) i0))
	(else (lambda (i0) (* n0 i0)))))

(define (array:1d-c0 c n0)
  (cond ((= 0 c) (array:1d-0 n0))
	((= 1 n0) (array:1d-c c))
	(else (lambda (i0) (+ c (* n0 i0))))))

; 2D cases:

(define (array:2d-0 n0)
  (lambda (i0 i1) (+ (* n0 i0) i1)))

(define (array:2d-1 n1)
  (lambda (i0 i1) (+ i0 (* n1 i1))))

(define (array:2d-c0 c n0)
  (lambda (i0 i1) (+ c (* n0 i0) i1)))

(define (array:2d-c1 c n1)
  (lambda (i0 i1) (+ c i0 (* n1 i1))))

(define (array:2d-01 n0 n1)
  (cond ((= 1 n0) (array:2d-1 n1))
	((= 1 n1) (array:2d-0 n0))
	(else (lambda (i0 i1) (+ (* n0 i0) (* n1 i1))))))

(define (array:2d-c01 c n0 n1)
  (cond ((= 0 c) (array:2d-01 n0 n1))
	((= 1 n0) (array:2d-c1 c n1))
	((= 1 n1) (array:2d-c0 c n0))
	(else (lambda (i0 i1) (+ c (* n0 i0) (* n1 i1))))))

; 3D cases:

(define (array:3d-01 n0 n1)
  (lambda (i0 i1 i2) (+ (* n0 i0) (* n1 i1) i2)))

(define (array:3d-02 n0 n2)
  (lambda (i0 i1 i2) (+ (* n0 i0) i1 (* n2 i2))))

(define (array:3d-12 n1 n2)
  (lambda (i0 i1 i2) (+ i0 (* n1 i1) (* n2 i2))))

(define (array:3d-c12 c n1 n2)
  (lambda (i0 i1 i2) (+ c i0 (* n1 i1) (* n2 i2))))

(define (array:3d-c02 c n0 n2)
  (lambda (i0 i1 i2) (+ c (* n0 i0) i1 (* n2 i2))))

(define (array:3d-c01 c n0 n1)
  (lambda (i0 i1 i2) (+ c (* n0 i0) (* n1 i1) i2)))

(define (array:3d-012 n0 n1 n2)
  (cond ((= 1 n0) (array:3d-12 n1 n2))
	((= 1 n1) (array:3d-02 n0 n2))
	((= 1 n2) (array:3d-01 n0 n1))
	(else (lambda (i0 i1 i2) (+ (* n0 i0) (* n1 i1) (* n2 i2))))))

(define (array:3d-c012 c n0 n1 n2)
  (cond ((= 0 c) (array:3d-012 n0 n1 n2))
	((= 1 n0) (array:3d-c12 c n1 n2))
	((= 1 n1) (array:3d-c02 c n0 n2))
	((= 1 n2) (array:3d-c01 c n0 n1))
	(else (lambda (i0 i1 i2) (+ c (* n0 i0) (* n1 i1) (* n2 i2))))))

; ND cases:

(define (array:Nd-* coefs)
  (lambda indices (apply + (map * coefs indices))))

(define (array:Nd-c* c coefs)
  (cond ((= 0 c) (array:Nd-* coefs))
	(else (lambda indices (apply + c (map * coefs indices))))))

; eof
