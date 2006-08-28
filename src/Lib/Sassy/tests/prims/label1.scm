(text
 (label foo (seq (nop)
		 (label bar z!)
		 (nop)))
 (jmp bar))

; foo
; 00000000  90                nop
; bar
; 00000001  7501              jnz 0x4
; 00000003  90                nop
; 00000004  E9F8FFFFFF        jmp 0x1 ; (jmp bar)

