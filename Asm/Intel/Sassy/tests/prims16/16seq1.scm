(bits 16)

(text
 (seq (nop)
      (nop)
      (seq (nop)
	   (inv (nop))
	   (nop))
      (seq (nop))))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  90                nop
; 00000003  90                nop
; 00000004  EB02              jmp short 0x8
; 00000006  90                nop
; 00000007  90                nop
