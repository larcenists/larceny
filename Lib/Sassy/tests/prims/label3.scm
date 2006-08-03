(text
 (with-win-lose foo bar
		(zero? eax))
 (nop)
 (nop)
 (nop)
 (seq (label bar
	     (inv (iter (seq (dec eax)
			     (inc ecx)
			     (>= eax 0)))))
      (label foo (push ecx))))

; 00000000  85C0              test eax,eax
; 00000002  0F840F000000      jz near 0x17
; 00000008  E903000000        jmp 0x10
; 0000000D  90                nop
; 0000000E  90                nop
; 0000000F  90                nop
; bar:
; 00000010  48                dec eax
; 00000011  41                inc ecx
; 00000012  83F800            cmp eax,byte +0x0
; 00000015  7DF9              jnl 0x10
; foo:
; 00000017  51                push ecx
