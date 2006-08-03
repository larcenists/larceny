(text
 (iter (seq (nop)
	    (nop)
	    (jnz $lose)
	    (nop)
	    (nop))))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  0F8504000000      jnz near 0xc
; 00000008  90                nop
; 00000009  90                nop
; 0000000A  EBF4              jmp short 0x0
	    