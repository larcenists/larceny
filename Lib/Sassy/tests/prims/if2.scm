(text
 (seq (nop)
      (if (seq (nop)
	       (inv (alt z! a!)))
	  (inv (nop))
	  (nop))
      (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  7405              jz 0x9
; 00000004  7703              ja 0x9
; 00000006  90                nop
; 00000007  EB02              jmp short 0xb
; 00000009  90                nop
; 0000000A  90                nop

;Causing the consequent (first) arm of the if to lose cause the whole
;if to lose, and thus the outer seq to lose.


