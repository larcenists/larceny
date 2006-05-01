; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Chez Scheme compatibility code -- logical operations.
;
; The following work because Chez Scheme fixnums are the same as Larceny 
; fixnums.

(define fxlogior fxlogor)
(define fxlogand fxlogand)
(define fxlogxor fxlogxor)
(define fxlognot fxlognot)
(define fxlsh fxsll)
(define fxrshl fxsrl)
(define fxrsha fxsra)

; eof

