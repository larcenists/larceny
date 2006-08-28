(bits 16)

(text
 (iter (alt (nop)
	    (iter (seq (nop)
		       (nop)
		       z!
		       (nop)))
	    (iter (inv (alt z!
			    l!))))))
; 00000000  90                nop
; 00000001  EBFD              jmp short 0x0
; 00000003  90                nop
; 00000004  90                nop
; 00000005  7503              jnz 0xa
; 00000007  90                nop
; 00000008  EBF9              jmp short 0x3
; 0000000A  7402              jz 0xe
; 0000000C  7DFC              jnl 0xa
