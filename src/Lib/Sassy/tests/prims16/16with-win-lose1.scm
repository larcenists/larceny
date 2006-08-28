(bits 16)

(text
 (with-win-lose foo bar
		(= ax 3))
 (seq (nop) (nop) (nop))
 (label foo (ret))
 (label bar (ret)))

; 00000000  3D0300            cmp ax,0x3
; 00000003  0F840600          jz near 0xd
; 00000007  E90400            jmp 0xe
; 0000000A  90                nop
; 0000000B  90                nop
; 0000000C  90                nop
; 0000000D  C3                ret
; 0000000E  C3                ret
