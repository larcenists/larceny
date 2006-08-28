(bits 16)

(text
 (iter (seq (nop)
	    (nop)
	    (jnz $lose)
	    (nop)
	    (nop))))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  0F850400          jnz near 0xa
; 00000006  90                nop
; 00000007  90                nop
; 00000008  EBF6              jmp short 0x0
