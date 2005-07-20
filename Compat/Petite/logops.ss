; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Chez Scheme compatibility code -- logical operations.
;
; The following work because Chez Scheme fixnums are the same as Larceny 
; fixnums.

(define logior fxlogor)
(define logand fxlogand)
(define logxor fxlogxor)
(define lognot fxlognot)
(define lsh fxsll)
(define rshl fxsrl)
(define rsha fxsra)

; eof

