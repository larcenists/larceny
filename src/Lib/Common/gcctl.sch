; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- garbage collector policy control
;
; This is a hack.

($$trace "gcctl")

; Heap number is 1..n.
; Operation codes can be read off the "case" below.  These things should
; go in a config file.
;
; Not all collectors implement all of these.  Check the sources!

(define (gcctl heap op . args)
  (case op
    ((j-fixed)      (sys$gcctl heap 0 (car args)))    ; NP only
    ((j-percent)    (sys$gcctl heap 1 (car args)))    ; NP only
    ((incr-fixed)   (sys$gcctl heap 2 (car args)))
    ((incr-percent) (sys$gcctl heap 3 (car args)))
    ((decr-fixed)   (sys$gcctl heap 4 (car args)))
    ((decr-percent) (sys$gcctl heap 5 (car args)))
    ((himark)       (sys$gcctl heap 6 (car args)))    ; expansion limit
    ((lomark)       (sys$gcctl heap 7 (car args)))    ; contraction limit
    ((oflomark)     (sys$gcctl heap 8 (car args)))    ; promotion
    ((grow)         #f)
    (else           (error "gcctl: no such operation: " op)
		    #t)))

; eof
