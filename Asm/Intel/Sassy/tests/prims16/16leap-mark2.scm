(bits 16)

(text
 (leap (iter
	(seq (nop)
	     (mark
	      (leap (seq (nop)
			 (mark
			  (leap (seq (nop)
				     (mark (seq (nop) z!))))))))))))

;confusing but correct.

; 00000000  EB07              jmp short 0x9
; 00000002  90                nop
; 00000003  EB04              jmp short 0x9
; 00000005  90                nop
; 00000006  EB01              jmp short 0x9
; 00000008  90                nop
; 00000009  90                nop
; 0000000A  74F6              jz 0x2
