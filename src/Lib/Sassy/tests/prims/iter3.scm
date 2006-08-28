(text
 (iter (alt (nop)
	    (seq z! a!)
	    (nop)
	    ge!)))

; 00000000  90                nop
; 00000001  EBFD              jmp short 0x0
; 00000003  7502              jnz 0x7
; 00000005  77F9              ja 0x0
; 00000007  90                nop
; 00000008  EBF6              jmp short 0x0
; 0000000A  7DF4              jnl 0x0
