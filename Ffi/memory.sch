; Ffi/memory.sch
; Larceny Foreign function interface -- memory management details,
;  for the non-conservative collectors.
;
; $Id$

(define syscall:make-nonrelocatable 35)
(define syscall:object->address 36)
(define syscall:getaddr 37)

(define syscall (system-function 'syscall))

(define (make-nonrelocatable-bytevector length)
  (syscall syscall:make-nonrelocatable length 5))

(define (cons-nonrelocatable a b)
  (let ((p (syscall syscall:make-nonrelocatable 2 1)))
    (set-car! p a)
    (set-cdr! p b)
    p))

(define (make-nonrelocatable-vector length . rest)
  (let ((p (syscall syscall:make-nonrelocatable length 3)))
    (if (not (null? rest))
	(vector-fill! p (car rest)))
    p))

(define (ffi/handle->address obj)
  (syscall syscall:object->address obj))

(define (ffi/getaddr name)
  (case name
    ((convert-and-call) (syscall syscall:getaddr 0))
    (else ???)))

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

(define (ffi/gcprotect-increment handle)
  (set-cdr! handle (+ (cdr handle) 1)))

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
