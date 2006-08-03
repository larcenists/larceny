(bits 16)

(text
 (label foo (seq (nop)
		 (label bar z!)
		 (nop)))
 (jmp bar))

; 00000000  90                nop
; 00000001  7501              jnz 0x4
; 00000003  90                nop
; 00000004  E9FAFF            jmp 0x1

