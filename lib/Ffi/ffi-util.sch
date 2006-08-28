; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- FFI utility functions

(define (ffi/string->asciiz str)
  (let ((b (make-bytevector (+ (string-length str) 1))))
    (bytevector-set! b (string-length str) 0)
    (do ((i (- (string-length str) 1) (- i 1)))
	((< i 0) b)
      (bytevector-set! b i (char->integer (string-ref str i))))))

(define (ffi/asciiz-length bv)
  (let ((limit (bytevector-length bv)))
    (let loop ((i 0))
      (cond ((= i limit) limit)
	    ((= (bytevector-ref bv i) 0) i)
	    (else (loop (+ i 1)))))))

(define (ffi/asciiz->string bv)
  (let* ((len (ffi/asciiz-length bv))
	 (s   (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len) s)
      (string-set! s i (integer->char (bytevector-ref bv i))))))
    
; eof
