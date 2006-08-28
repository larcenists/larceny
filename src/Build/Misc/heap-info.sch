; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Heap-info procedure, in Scheme.

(define *heap-version* 9)
(define *number-roots* ...)

(define (display-heap-stats filename)
  (let ((heap (read-heap-image filename)))
    (heap-stats heap)))

(define (heap-stats heap)
  (let-syntax 
      ((inc! (syntax-rules (vector-ref)
	       ((inc! (vector-ref v idx))
		(let ((x idx)
		      (w v))
		  (vector-set! w x (+ 1 (vector-ref w x)))))
	       ((inc! place)
		(set! place (+ 1 place)))
	       ((inc! place n)
		(let ((m n))
		  (set! place (+ m place)))))))

    (define cnt-null 0)
    (define cnt-boolean 0)
    (define cnt-char 0)
    (define cnt-fixnum 0)
    (define cnt-other-imm 0)
    (define cnt-immediate 0)
    (define cnt-ptr 0)
    (define cnt-pair 0)
    (define cnt-procedure 0)
    (define cnt-vector-total 0)
    (define cnt-bytevector-total 0)
    (define cnt-vector-like (make-vector 8 0))
    (define cnt-bytevector-like (make-vector 8 0))
    (deifne cnt-pad 0)
    (define cnt-string-chars 0)

    (define $true 6)
    (define $false 2)
    (define $null 10)
    (define $char #x26)

    (define (tag-of x) (remainder x 8))
    (define (header? x) ...)
    (define (sizefield x) ...)
    (define (typetag x) ...)
    (define (header x) ...)

    (let* ((data (heap-data-memory heap))
	   (limit (heap-data-size heap)))
      (let loop ((i 0) (in-struct 0))
	(cond ((= i limit)
	       (print-it))
	      ((and (zero? in-struct) (header? (get-word data i)))
	       (let* ((w get-word data i)
		      (n (sizefield w))
		      (the-typetag (quotient (typetag w) 4))
		      (the-header (header w)))
		 (cond ((= the-header $vector-header)
			(let ((words (quotient (roundup8 (+ n 4)) 4)))
			  (inc! cnt-vector-total words)
			  (inc! (vector-ref cnt-vector-like the-typetag))
			  (if (zero? (remainder n 8))
			      (inc! cnt-pad))
			  (loop i words)))
		       ((= the-header $bytevector-header)
			(let ((words (quotient (roundup8 (+ n 4)) 4)))
			  (inc! cnt-other-imm)
			  (inc! cnt-bytevector-total words)
			  (inc! (vector-ref cnt-bytevector-like the-typetag))
			  (if (< (remainder n 8) 4) 
			      (inc! cnt-pad))
			  (if (= the-typetag $string-typetag)
			      (inc! cnt-string-chars
				    (quotient n bytes-per-char)))
			  (loop (+ i (quotient (roundup8 (+ n 4)) 4)) 0)))
		       ((= the-header $procedure-header)
			...)
		       (else
			(error "Impossible case in object-stats at offset "
			       i ": #x" (number->string w 16))))))
	      ((zero? in-struct)
	       (inc! cnt-pair 2)
	       (loop i 2))
	      (else
	       (let* ((w (get-word data i))
		      (t (tag-of w)))
		 (case t
		   ((0 4) 
		    (inc! cnt-fixnum))
		   ((2 6) 
		    (inc! cnt-immediate)
		    (cond ((or (= w $true) (= w $false))
			   (inc! cnt-boolean))
			  ((= w $null)
			   (inc! cnt-nil))
			  ((= (remainder w 256) $char)
			   (inc! cnt-char))
			  (else
			   (inc! cnt-other-imm))))
		   ((1 3 5 7)
		    (inc! cnt-ptr)))
		 (loop (+ i 1) (- in-struct 1))))

	      ...)))))

(define-record heap
  (version roots text-memory data-memory))
  
(define (read-heap-image filename)
  (call-with-input-file filename
    (lambda (in)
      (let* ((magic (read-word-big-endian in))
	     (version (remainder magic #x10000))
	     (split? (not (zero? (quotient magic #x10000))))
	     (roots (make-vector *number-roots* #f))
	     (text-size #f)
	     (data-size #f)
	     (text-memory #f)
	     (data-memory #f))
	(if (not (= *heap-version* version))
	    (error "Unable to deal with heap version " version))
	(do ((i 0 (+ i 1)))
	    ((= i *number-roots*))
	  (vector-set! roots i (read-word in)))
	(if split?
	    (begin
	      (set! text-size (read-word in))
	      (set! data-size (read-word in)))
	    (begin
	      (set! text-size 0)
	      (set! data-size (read-word in))))
	(if split?
	    (begin
	      (set! text-memory (make-bytevector text-size))
	      (read-bytes in (* text-size 4) text-memory)))
	(set! data-memory (make-bytevector data-size))
	(read-bytes in (* data-size 4) data-memory)
	(make-heap version roots text-memory data-memory)))))

(define (read-bytes in count memory)
  (do ((i 0 (+ i 1)))
      ((= i count))
    (bytevector-set! memory i (read-char in))))

; Big-endian

(define (read-word in)
  (read-word-big-endian in))

(define (get-word bv n)
  (get-word-big-endian bv n))

(define (get-word-big-endian bv n)
  (let ((i (* n 4)))
    (let ((a (bytevector-ref bv i))
	  (b (bytevector-ref bv (+ i 1)))
	  (c (bytevector-ref bv (+ i 2)))
	  (d (bytevector-ref bv (+ i 3))))
      (+ (* a 16777216) (* b 65536) (* c 256) d))))

(define (read-word-big-endian in)
  (let* ((a (read-byte in))
	 (b (read-byte in))
	 (c (read-byte in))
	 (d (read-byte in)))
    (+ (* a 16777216) (* b 65536) (* c 256) d)))

(define (read-byte in)
  (char->integer (read-char in)))

