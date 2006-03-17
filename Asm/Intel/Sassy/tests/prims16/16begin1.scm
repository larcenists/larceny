(bits 16)

(text
 (begin (nop)
	(seq (nop)
	     (begin (nop)
		    (inv (nop)))
	     (nop))
	(nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  90                nop
; 00000003  90                nop
; 00000004  EB01              jmp short 0x7
; 00000006  90                nop
; 00000007  90                nop
