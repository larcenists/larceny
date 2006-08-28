(text
 (label foo (ret))
 (seq (nop)
      (with-lose (reloc my-reloc foo 100)
		 (iter (alt (seq (nop) z!)
			    (seq (nop) a!)
			    (seq (nop) po!))))
      (nop)))
; foo:
; 00000000  C3                ret

; 00000001  90                nop
; 00000002  90                nop
; 00000003  74FD              jz 0x2
; 00000005  90                nop
; 00000006  77FA              ja 0x2
; 00000008  90                nop
; 00000009  7BF7              jpo 0x2
; 0000000B  E964000000        jmp 0x74 <- this is right, (reloc had an addend of 100)
; 00000010  90                nop
