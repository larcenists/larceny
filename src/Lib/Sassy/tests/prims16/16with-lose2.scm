(bits 16)

(text
 (label foo (ret))
 (seq (nop)
      (with-lose (reloc my-reloc foo 100)
		 (iter (alt (seq (nop) z!)
			    (seq (nop) a!)
			    (seq (nop) po!))))
      (nop)))

; 00000000  C3                ret
; 00000001  90                nop
; 00000002  90                nop
; 00000003  74FD              jz 0x2
; 00000005  90                nop
; 00000006  77FA              ja 0x2
; 00000008  90                nop
; 00000009  7BF7              jpo 0x2
; 0000000B  E96400            jmp 0x72
; 0000000E  90                nop
