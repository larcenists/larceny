(bits 16)

(text
 (seq (nop)
      (nop)
      (seq (nop)
	   (nop)
	   z!
	   (nop)
	   (inv z!)
	   (nop))
      (seq (nop))))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  90                nop
; 00000003  90                nop
; 00000004  7505              jnz 0xb
; 00000006  90                nop
; 00000007  7402              jz 0xb
; 00000009  90                nop
; 0000000A  90                nop
