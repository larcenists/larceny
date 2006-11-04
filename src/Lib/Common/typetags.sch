; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Type tags.
;
; The type tags really should be gotten from the auto-generated stuff.

(define sys$tag.bytevector-typetag 0)
(define	sys$tag.string-typetag 1)
(define sys$tag.flonum-typetag 2)
(define sys$tag.compnum-typetag 3)
(define sys$tag.bignum-typetag 4)
(define sys$tag.ustring-typetag 5) ; FIXME: not added anywhere else

; Typetags for vector-like structures.

(define sys$tag.vector-typetag 0)
(define sys$tag.rectnum-typetag 1)
(define sys$tag.ratnum-typetag 2)
(define sys$tag.symbol-typetag 3)
(define sys$tag.port-typetag 4)
(define sys$tag.structure-typetag 5)

; eof
