; Ffi/ffi-access.sch
; Larceny FFI -- Access/update abstractions
;
; $Id$

; Alignment is an integer
; Representation is either a keyword or an integer (representing a number 
; of bytes).

(define (ffi/foreign-accessor alignment representation)

  (define (peek-bytes p k)
    (let ((v (make-bytevector k)))
      (do ((i 0 (+ i 1)))
	  ((= i k) v)
	(bytevector-set! v i ($peek-unsigned8 (+ p i))))))

  (define (bv->int w)
    (cond ((< (bytevector-ref w 0) 128)
	   ...)
	  ((= (bytevector-ref w 0) 128)
	   ...)
	  (else
	   (- (bv->int (complement w))))))

  (case representation
    ((signed32)
     (if (>= alignment 4)
	 $peek-signed32
	 (lambda (p)
	   (bv->int (peek-bytes p 4)))))
    ((unsigned32)
     (if (>= alignment 4)
	 $peek-unsigned32
	 (lambda (p)
	   (bv->uint (peek-bytes p 4)))))
    ...
    (else
     (lambda (p)
       (peek-bytes p representation)))))

(define (ff/foreign-updater alignment size representation)
  ...)

; eof
