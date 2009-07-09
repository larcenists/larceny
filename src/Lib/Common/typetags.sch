; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Type tags.

(define sys$tag.pair-tag           $tag.pair-tag)
(define sys$tag.vector-tag         $tag.vector-tag)
(define sys$tag.bytevector-tag     $tag.bytevector-tag)
(define sys$tag.procedure-tag      $tag.procedure-tag)

; Typetags for bytevector-like structures.

(define sys$tag.bytevector-typetag (quotient $tag.bytevector-typetag 4))
(define	sys$tag.string-typetag     (quotient $tag.string-typetag 4))
(define sys$tag.flonum-typetag     (quotient $tag.flonum-typetag 4))
(define sys$tag.compnum-typetag    (quotient $tag.compnum-typetag 4))
(define sys$tag.bignum-typetag     (quotient $tag.bignum-typetag 4))
(define sys$tag.ustring-typetag    (quotient $tag.ustring-typetag 4))

; Typetags for vector-like structures.

(define sys$tag.vector-typetag     (quotient $tag.vector-typetag 4))
(define sys$tag.rectnum-typetag    (quotient $tag.rectnum-typetag 4))
(define sys$tag.ratnum-typetag     (quotient $tag.ratnum-typetag 4))
(define sys$tag.symbol-typetag     (quotient $tag.symbol-typetag 4))
(define sys$tag.port-typetag       (quotient $tag.port-typetag 4))
(define sys$tag.structure-typetag  (quotient $tag.structure-typetag 4))

; eof
