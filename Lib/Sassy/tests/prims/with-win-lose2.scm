(text
 (with-win-lose foo bar
		(while (= eax 3)
		       (seq (nop)
			    (nop)
			    l!)))
 (seq (nop) (nop) (nop))
 (label foo (ret))
 (label bar (ret)))

; 00000000  EB08              jmp short 0xa
; 00000002  90                nop
; 00000003  90                nop
; 00000004  0F8D0E000000      jnl near 0x18
; 0000000A  83F803            cmp eax,byte +0x3
; 0000000D  74F3              jz 0x2
; 0000000F  E903000000        jmp 0x17
; 00000014  90                nop
; 00000015  90                nop
; 00000016  90                nop
; 00000017  C3                ret
; 00000018  C3                ret


