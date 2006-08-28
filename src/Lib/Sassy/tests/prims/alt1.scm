(text
 (seq (nop)
      (alt (nop)
	   (nop)
	   (nop)
	   (inv (nop)))
      (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  EB09              jmp short 0xd
; 00000004  90                nop
; 00000005  EB06              jmp short 0xd
; 00000007  90                nop
; 00000008  EB03              jmp short 0xd
; 0000000A  90                nop
; 0000000B  EB01              jmp short 0xe
; 0000000D  90                nop
