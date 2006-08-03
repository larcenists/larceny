(bits 16)

(text
 (begin (nop)
	(alt (nop)
	     (begin (nop)
		    z!)
	     (nop))
	(nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  EB04              jmp short 0x8
; 00000004  90                nop
; 00000005  7401              jz 0x8
; 00000007  90                nop
; 00000008  90                nop
