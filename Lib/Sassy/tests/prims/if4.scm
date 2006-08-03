(text
 (seq (nop)
      (if (seq (nop) z!)
	  (inv (nop))
	  (if g!
	      (nop)
	      (if p!
		  (nop)
		  (nop))))
      (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  7503              jnz 0x7
; 00000004  90                nop
; 00000005  EB0C              jmp short 0x13
; 00000007  7E03              jng 0xc
; 00000009  90                nop
; 0000000A  EB06              jmp short 0x12
; 0000000C  7B03              jpo 0x11
; 0000000E  90                nop
; 0000000F  EB01              jmp short 0x12
; 00000011  90                nop
; 00000012  90                nop

