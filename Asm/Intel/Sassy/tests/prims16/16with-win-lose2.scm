(bits 16)

(text
 (with-win-lose foo bar
		(while (= ax 3)
		       (seq (nop)
			    (nop)
			    l!)))
 (seq (nop) (nop) (nop))
 (label foo (ret))
 (label bar (ret)))

; 00000000  EB06              jmp short 0x8
; 00000002  90                nop
; 00000003  90                nop
; 00000004  0F8D0C00          jnl near 0x14
; 00000008  3D0300            cmp ax,0x3
; 0000000B  74F5              jz 0x2
; 0000000D  E90300            jmp 0x13
; 00000010  90                nop
; 00000011  90                nop
; 00000012  90                nop
; 00000013  C3                ret
; 00000014  C3                ret
