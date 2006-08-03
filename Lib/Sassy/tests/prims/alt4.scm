(text
 (seq (nop)
      (alt (nop)
	   ge!
	   (nop)
	   ge!)
      (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  EB07              jmp short 0xb
; 00000004  7D05              jnl 0xb
; 00000006  90                nop
; 00000007  EB02              jmp short 0xb
; 00000009  7C01              jl 0xc
; 0000000B  90                nop
