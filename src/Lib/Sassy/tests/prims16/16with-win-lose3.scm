(bits 16)

(text
 (label foo (ret))
 (with-win-lose bar foo
		(if (= ax 3)
		    (nop)
		    (alt (seq (nop) a!)
			 (seq (nop) p!))))
 (nop)
 (label bar (ret)))

; 00000000  C3                ret
; 00000001  3D0300            cmp ax,0x3
; 00000004  7504              jnz 0xa
; 00000006  90                nop
; 00000007  E90E00            jmp 0x18
; 0000000A  90                nop
; 0000000B  0F870900          ja near 0x18
; 0000000F  90                nop
; 00000010  0F8A0400          jpe near 0x18
; 00000014  E9E9FF            jmp 0x0
; 00000017  90                nop
; 00000018  C3                ret
