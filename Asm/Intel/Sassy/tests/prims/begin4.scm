(text
 (begin (nop)
	(alt (nop)
	     (begin (nop)
		    (inv z!))
	     (nop))
	(nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  EB04              jmp short 0x8
; 00000004  90                nop
; 00000005  7501              jnz 0x8
; 00000007  90                nop
; 00000008  90                nop

