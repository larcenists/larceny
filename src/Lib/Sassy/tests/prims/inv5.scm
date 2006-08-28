(text
 (alt (if (alt (inv (nop))
	       (inv z!)
	       (inv (inv (nop)))
	       (inv ge!))
	  (inv (nop))
	  (nop))
      (nop)))

; should be the same as inv3.scm, (de Morgan)

; 00000000  90                nop
; 00000001  7505              jnz 0x8
; 00000003  90                nop
; 00000004  EB02              jmp short 0x8
; 00000006  7D03              jnl 0xb
; 00000008  90                nop
; 00000009  EB03              jmp short 0xe
; 0000000B  90                nop
; 0000000C  EB01              jmp short 0xf
; 0000000E  90                nop
