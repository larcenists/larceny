(text
 (with-win-lose foo bar
		(= eax 3))
 (seq (nop) (nop) (nop))
 (label foo (ret))
 (label bar (ret)))

; 00000000  83F803            cmp eax,byte +0x3
; 00000003  0F8408000000      jz near 0x11
; 00000009  E904000000        jmp 0x12
; 0000000E  90                nop
; 0000000F  90                nop
; 00000010  90                nop
; 00000011  C3                ret
; 00000012  C3                ret

