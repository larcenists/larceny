; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- memory management details, for the non-conservative 
;   collectors.
;
; PROTOTYPE CODE!!

; Some syscall numbers, which must be defined here (and not in Lib/unix.sch)
; because we're outside the Scheme kernel.

(define syscall:make-nonrelocatable 35)
(define syscall:object->address 36)
(define syscall:getaddr 37)

; Nonrelocatable object allocation.
;
; Currently the nonrelocatable objects are allocated in the static area,
; so they won't even be garbage collected.

; Allocate a nonrelocatable bytevector of the required length (number of 
; bytes).  The length should not include space for the header.

(define (make-nonrelocatable-bytevector length)
  (syscall syscall:make-nonrelocatable length 5))

; Allocate a nonrelocatable pair.

(define (cons-nonrelocatable a b)
  (let ((p (syscall syscall:make-nonrelocatable 2 1)))
    (set-car! p a)
    (set-cdr! p b)
    p))

; Allocate a nonrelocatable vector of the given number of elements.

(define (make-nonrelocatable-vector length . rest)
  (let ((p (syscall syscall:make-nonrelocatable length 3)))
    (if (not (null? rest))
	(vector-fill! p (car rest)))
    p))


; Return the address of a boxed object.

(define (ffi/handle->address obj)
  (syscall syscall:object->address obj))


; Given a symbol that denotes a run-time system FFI procedure, return
; its address.  
; 
; Procedures available are
;  convert-and-call      larceny_C_ffi_convert_and_call

(define (ffi/getaddr name)
  (case name
    ((convert-and-call) (syscall syscall:getaddr 0))
    (else ???)))

; 'GCprotect' mechanism.

; Protect an object against garbage collection by adding it to a list of
; objects so protected, and return an 'object ID' for the object that
; serves as a handle to the object.

(define (ffi/gcprotect obj)
  (if (or (vector-like? obj)
	  (procedure? obj)
	  (bytevector-like? obj)
	  (pair? obj))
      (let* ((id (ffi/new-object-id))
	     (handle (cons-nonrelocatable obj (logior id 1))))
	(ffi/remember-handle handle id)
	handle)
      (error "Can't gcprotect a non-pointer: " obj)))

; Increment the reference count on a protected object (represented as a
; handle to the object).

(define (ffi/gcprotect-increment handle)
  (set-cdr! handle (+ (cdr handle) 1)))

; Decrement the reference count on a protected object (represented as a
; handle to the object).

(define (ffi/gcunprotect handle)
  (let ((c  (logand (cdr handle) 65535))
	(id (rsha (cdr handle) 16)))
    (if (= c 1)
	(begin 
	  (ffi/forget-handle handle id)
	  (set-car! handle #f)
	  (set-cdr! handle #f))
	(set-cdr! handle (logior id (- c 1))))))


; Here we can use a hash table with the id being the hash code, this
; remains to be implemented.  FIXME.

; FIXME: should be string->uninterned-symbol

(define *ffi/anchor* (string->symbol "GCPROTECT"))

(define (ffi/remember-handle handle id)
  (let ((p (or (getprop *ffi/anchor* 'handles) '())))
    (putprop *ffi/anchor* 'handles (cons handle p))))

(define (ffi/forget-handle handle id)
  ; fixme
  #f)


; Note: object IDs are not necessarily unique.

(define ffi/new-object-id
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      (remainder id 65536))))

; eof
