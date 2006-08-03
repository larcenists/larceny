(text
 (while (inv (zero? eax))
	(seq (pop ecx)
	     (dec eax))))

; 00000000  EB02              jmp short 0x4
; 00000002  59                pop ecx
; 00000003  48                dec eax
; 00000004  85C0              test eax,eax
; 00000006  75FA              jnz 0x2
