; Ffi/bdw-memory.sch
; Larceny Foreign function interface -- memory management details,
;  for the Boehm-Demers-Weiser conservative collector.
;
; $Id: bdw-memory.sch,v 1.1.1.1 1998/11/19 21:52:26 lth Exp $

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
