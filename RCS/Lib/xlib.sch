; Larceny run-time library.
; System-specific library routines and global variables.
;
; $Id$

; Typetags for bytevector-like structures.

(define sys$tag.bytevector-typetag 0)
(define	sys$tag.string-typetag 1)
(define sys$tag.flonum-typetag 2)
(define sys$tag.compnum-typetag 3)
(define sys$tag.bignum-typetag 4)

; Typetags for vector-like structures.

(define sys$tag.vector-typetag 0)
(define sys$tag.rectnum-typetag 1)
(define sys$tag.ratnum-typetag 2)
(define sys$tag.symbol-typetag 3)
(define sys$tag.port-typetag 4)

; Make-string is in Scheme for simplicity.

(define (make-string n . init)
  (let ((s (make-bytevector n)))
    (if (not (null? init))
	(bytevector-fill! s (char->integer (car init))))
    (typetag-set! n sys$tag.string-typetag)
    s))

; Ditto for string-set!

(define (string-set! s i x)
  (bytevector-like-set! s i (char->integer x)))
