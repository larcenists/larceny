(text
 (seq (if (= ecx 0)
	  (alt z! z!)
	  (nop))
      (nop)))

; 00000000  83F900            cmp cx,byte +0x0
; 00000003  7506              jnz 0xb
; 00000005  7405              jz 0xc
; 00000007  7403              jz 0xc
; 00000009  EB02              jmp short 0xd
; 0000000B  90                nop
; 0000000C  90                nop


