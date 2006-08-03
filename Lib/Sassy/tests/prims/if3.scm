(text
 (seq (nop)
      (if (seq (nop) z!)
	  (inv (nop))
	  (if g!
	      (nop)
	      (if p!
		  (nop)
		  (seq a! (nop)))))
      (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  7503              jnz 0x7
; 00000004  90                nop
; 00000005  EB0E              jmp short 0x15
; 00000007  7E03              jng 0xc
; 00000009  90                nop
; 0000000A  EB08              jmp short 0x14
; 0000000C  7B03              jpo 0x11
; 0000000E  90                nop
; 0000000F  EB03              jmp short 0x14
; 00000011  7602              jna 0x15
; 00000013  90                nop
; 00000014  90                nop

