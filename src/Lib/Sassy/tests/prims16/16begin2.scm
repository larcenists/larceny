(bits 16)

(text
 (begin (nop)
	(seq (nop)
	     (begin (nop)
		    z!)
	     (nop))
	(nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  90                nop
; 00000003  7501              jnz 0x6
; 00000005  90                nop
; 00000006  90                nop
