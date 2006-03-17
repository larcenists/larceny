(text
 (label foo (ret))
 (with-win-lose bar foo
		(if (= eax 3)
		    (nop)
		    (alt (seq (nop) a!)
			 (seq (nop) p!))))
 (nop)
 (label bar (ret)))

; foo:
; 00000000  C3                ret
; 00000001  83F803            cmp eax,byte +0x3
; 00000004  7506              jnz 0xc
; 00000006  90                nop
; 00000007  E914000000        jmp 0x20
; 0000000C  90                nop
; 0000000D  0F870D000000      ja near 0x20
; 00000013  90                nop
; 00000014  0F8A06000000      jpe near 0x20
; 0000001A  E9E1FFFFFF        jmp 0x0
; 0000001F  90                nop
; bar:
; 00000020  C3                ret
