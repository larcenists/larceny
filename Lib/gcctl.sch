; Lib/gcctl.sch
; Larceny library -- garbage collector policy control
;
; $Id: gcctl.sch,v 1.1 1997/05/31 01:50:28 lth Exp lth $
;
; This is a hack, until the collector gets rewritten in Scheme.

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
    ((himark)       #f)
    ((lomark)       #f)
    ((oflomark)     #f)
    ((grow)         #f)
    (else (error "gcctl: no such operation: " op))))

; eof
