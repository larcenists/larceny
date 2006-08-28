; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- memory management details, for the Boehm-Demers-Weiser 
;   conservative collector.

(define make-nonrelocatable-bytevector make-bytevector)
(define cons-nonrelocatable cons)
(define make-nonrelocatable-vector make-vector)

(define (ffi/gcprotect obj)
  (cons obj 0))

(define (ffi/gcprotect-increment handle)
  #t)

(define (ffi/gcunprotect handle)
  #t)

; eof
