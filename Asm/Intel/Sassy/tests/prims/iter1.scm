(text
 (iter (seq (nop)
	    (alt z! a!)
	    (nop))))

; 00000000  90                nop
; 00000001  7402              jz 0x5
; 00000003  7603              jna 0x8
; 00000005  90                nop
; 00000006  EBF8              jmp short 0x0

