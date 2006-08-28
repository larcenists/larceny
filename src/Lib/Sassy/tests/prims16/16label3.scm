(bits 16)

(text
 (with-win-lose foo bar
		(zero? ax))
 (nop)
 (nop)
 (nop)
 (seq (label bar
	     (inv (iter (seq (dec ax)
			     (inc cx)
			     (>= ax 0)))))
      (label foo (push cx))))

; 00000000  85C0              test ax,ax
; 00000002  0F840D00          jz near 0x13
; 00000006  E90300            jmp 0xc
; 00000009  90                nop
; 0000000A  90                nop
; 0000000B  90                nop
; 0000000C  48                dec ax
; 0000000D  41                inc cx
; 0000000E  3D0000            cmp ax,0x0
; 00000011  7DF9              jnl 0xc
; 00000013  51                push cx
