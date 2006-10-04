; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- low-level FFI.
;
; Note that the caller of dlsym must have canonicalized the name for
; the particular OS (SunOS 4 prepends _ to names for C; SunOS 5 does not),
; as no translation is performed on this level.

(define (ffi/dlopen name)
  (let ((r (sys$c-ffi-dlopen (ffi/string->asciiz name))))
    (if (zero? r)
	#f
	r)))

(define (ffi/dlsym handle sym)
  (let ((r (sys$c-ffi-dlsym handle (ffi/string->asciiz sym))))
    (if (zero? r)
	#f
	r)))

(define (ffi/convert-arg-descriptor abi arg-descriptor)
  (let ((d (make-bytevector (length arg-descriptor))))
    (do ((l arg-descriptor (cdr l))
	 (i 0 (+ i 1)))
	((null? l) d)
      (bytevector-set! d i (case (car l)
			     ((signed32) 0)
			     ((unsigned32) 1)
			     ((ieee32) 2)
			     ((ieee64) 3)
			     ((pointer) 4)
			     (else ???))))))

(define (ffi/convert-ret-descriptor abi ret-descriptor)
  (case ret-descriptor
    ((signed32) 0)
    ((unsigned32) 1)
    ((ieee64) 2)
    ((ieee32) 3)
    ((void) 4)
    (else ???)))

(define (ffi/apply trampoline arg-encoding ret-encoding actuals)
  (if (or (not (trampoline? trampoline))
	  (not (bytevector? arg-encoding))
	  (not (fixnum? ret-encoding))
	  (not (list? actuals)))
      (error "ffi/apply: bad arguments.")
      (call-without-interrupts
	; We use call-without-interrupts because the FFI error info
	; constitutes state; if it weren't for that, this code would
	; be reentrant.  We can make it reentrant by passing a location
	; to put the error info in.
	(lambda ()
	  (let ((r (sys$c-ffi-apply (tr-code trampoline)
				    arg-encoding
				    ret-encoding
				    actuals)))
	    (if (eq? r (undefined))
		(call-with-values 
		 sys$c-ffi-error
		 (lambda (code info)
		   (case code
		     ((0) (values #t 'conversion-error))
		     ((1) (values #t info))
		     (else (error "ffi/apply: Did not understand "
				  code " FFI error code.")))))
		(values #f r)))))))

; FIXME
(define (sys$c-ffi-error)
  (values 0 #f))

; eof
