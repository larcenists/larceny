(bits 16)

(text
 (while (inv (zero? ax))
	(seq (pop cx)
	     (dec ax))))

; 00000000  EB02              jmp short 0x4
; 00000002  59                pop cx
; 00000003  48                dec ax
; 00000004  85C0              test ax,ax
; 00000006  75FA              jnz 0x2
