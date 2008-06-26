; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- FFI trampoline construction (Common code)

; Trampoline for callout
;
; Common trampoline fields; procedures that abstract these are at the
; end of the common section:
;
;   name    offset  contents
;   ------  ------  --------
;   tag          0   an object
;   code         1   a bytevector -- the assembled code
;   ilist        2   a list -- instructions, in order
;   increment    3   an integer -- the increment in the passed argument array
;   fptr         4   an integer -- the function pointer

(define (ffi/make-callout abi function-address arg-descriptor ret-descriptor)

  (define (create-trampoline tr)
    (set-tr-fptr! tr function-address)
    (do ((a arg-descriptor (cdr a)))
	((null? a))
      (case (car a)
	((unsigned32 signed32 pointer)
	 ((abi 'arg-word) tr))
	((unsigned64 signed64)
	 ((abi 'arg-word2) tr))
	((ieee64)
	 ((abi 'arg-ieee64) tr))
	((ieee32)
	 ((abi 'arg-ieee32) tr))
	(else
	 (error "make-callout: " (car a) " is not a valid argument type."))))
    (case ret-descriptor
      ((signed32 unsigned32)
       ((abi 'ret-word) tr))
      ((signed64 unsigned64)
       ((abi 'ret-word2) tr))
      ((ieee64)
       ((abi 'ret-ieee64) tr))
      ((ieee32)
       ((abi 'ret-ieee32) tr))
      ((void)
       ((abi 'ret-void) tr))
      (else
       (error "make-callout: " ret-descriptor " is not a valid return type.")))
    ((abi 'done) tr)
    (tr-pasteup tr)
    ((abi 'done-pasteup) tr))

  (let ((tr ((abi 'alloc))))
    (create-trampoline tr)
    tr))

; Use this to relink after heap restoring.
;
; This recomputes the code vector in the trampoline.  The foreign function
; stub may not keep that code vector independently of the trampoline.

(define (ffi/set-callout-address! abi tramp new-addr)
  (set-tr-fptr! tramp new-addr)
  ((abi 'change-fptr) tramp)
  (tr-pasteup tramp)
  ((abi 'done-pasteup) tramp)
  tramp)


; Common operations.

(define *trampoline-basis-size* 5)
(define *trampoline-tag* (vector 'trampoline))

(define (trampoline? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) *trampoline-tag*)))

(define (tr-init-common tr)
  (vector-set! tr 0 *trampoline-tag*)
  (set-tr-code! tr #f)
  (set-tr-increment! tr 8)          ; FIXME: should be a parameter!
  (set-tr-ilist! tr '())
  (set-tr-fptr! tr 'illegal-fptr))

(define (tr-code tr) (vector-ref tr 1))
(define (tr-ilist tr) (vector-ref tr 2))
(define (tr-increment tr) (vector-ref tr 3))
(define (tr-fptr tr) (vector-ref tr 4))

(define (set-tr-code! tr code) (vector-set! tr 1 code))
(define (set-tr-ilist! tr ilist) (vector-set! tr 2 ilist))
(define (set-tr-increment! tr val) (vector-set! tr 3 val))
(define (set-tr-fptr! tr val) (vector-set! tr 4 val))

(define (tr-at-end tr i)
  (let ((ilist (tr-ilist tr)))
    (if (null? ilist)
	(set-tr-ilist! tr (list i))
	(append! (tr-ilist tr) (list i)))))

(define (tr-at-beginning tr i)
  (set-tr-ilist! tr (cons i (tr-ilist tr))))

(define (tr-pasteup tr)
  (let ((c (make-nonrelocatable-bytevector
	    (apply + (map bytevector-length (tr-ilist tr))))))
    (set-tr-code! tr c)
    (let iloop ((l (tr-ilist tr)) (i 0))
      (if (not (null? l))
	  (let jloop ((j 0) (i i) (v (car l)))
	    (if (< j (bytevector-length v))
		(begin (bytevector-set! c i (bytevector-ref v j))
		       (jloop (+ j 1) (+ i 1) v))
		(iloop (cdr l) i)))))))


; Callbacks 
;
; arg-descriptor is a list:
;  signed32
;  unsigned32
;  ieee32
;  ieee64
;  pointer
;
; FIXME: structures not supported.

; The ABI is a callback-abi

(define (ffi/make-callback abi proc arg-descriptor ret-descriptor)

  (define (ptrof x)
    (* (quotient x 8) 8))

  (let* ((argc          (length arg-descriptor))
	 (proc-encoding (ptrof
			 (ffi/handle->address (ffi/gcprotect proc))))
	 (arg-encoding  (ptrof
			 (ffi/handle->address
			  (ffi/gcprotect
			   (ffi/convert-arg-descriptor abi arg-descriptor)))))
	 (ret-encoding  (ffi/convert-ret-descriptor abi ret-descriptor))
	 (tr            ((abi 'alloc))))
    (set-tr-fptr! tr ((abi 'callback-addr)))
    ((abi 'proc-addr) tr proc-encoding)
    ((abi 'arg-types) tr arg-encoding)
    ((abi 'ret-type) tr ret-descriptor ret-encoding)
    (do ((args arg-descriptor (cdr args)))
	((null? args))
      (case (car args)
	((signed32 unsigned32 pointer)
	 ((abi 'arg-word) tr))
	((signed64 unsigned64)
	 ((abi 'arg-word2) tr))
	((ieee32)
	 ((abi 'arg-single) tr))
	((ieee64)
	 ((abi 'arg-double) tr))
	(else ???)))
    ((abi 'done) tr (length arg-descriptor))
    (tr-pasteup tr)
    ((abi 'done-pasteup) tr)
    tr))

; eof
