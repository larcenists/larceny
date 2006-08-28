(bits 16)

(text
 (iter (seq (nop)
	    (nop)
	    (jnz short $lose)
	    (nop)
	    (nop)
	    (je short $win))))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  7506              jnz 0xa
; 00000004  90                nop
; 00000005  90                nop
; 00000006  74F8              jz 0x0
; 00000008  EBF6              jmp short 0x0
	    