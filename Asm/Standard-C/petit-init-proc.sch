; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Petit Larceny bootstrap procedure.
;
; The startup procedure is same as for SPARC larceny except for the
; patch instruction.

(define init-proc
  `((,$.proc)
    (,$args= 1)
    (,$reg 1)
    (,$setreg 2)
    (,$const (thunks))
    (,$op1 petit-patch-boot-code)       ; Petit larceny
    (,$setreg 1)
    (,$.label 1001)
    (,$reg 1)
    (,$op1 null?)                       ; (null? l)
    (,$branchf 1003)
    (,$const (symbols))                 ; dummy list of symbols
    (,$setreg 1)
    (,$global go)
    (,$invoke 2)                        ; (go <list of symbols> argv)
    (,$.label 1003)
    (,$save 2)
    (,$store 0 0)
    (,$store 1 1)
    (,$store 2 2)
    (,$setrtn 1004)
    (,$reg 1)
    (,$op1 car)
    (,$invoke 0)                        ; ((car l))
    (,$.align 4)
    (,$.label 1004)
    (,$.cont)
    (,$restore 2)
    (,$pop 2)
    (,$reg 1)
    (,$op1 cdr)
    (,$setreg 1)
    (,$branch 1001)))                   ; (loop (cdr l))

; eof
