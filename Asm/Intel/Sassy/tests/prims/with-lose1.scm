(text
 (label foo (ret))
 (seq (nop)
      (with-lose foo
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
; 0000000B  E9F0FFFFFF        jmp 0x0
; 00000010  90                nop
