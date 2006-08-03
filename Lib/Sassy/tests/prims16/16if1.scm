(bits 16)

(text
 (seq (nop)
      (if (alt z! a!)
	  (nop)
	  (inv (nop)))
      (nop)))

; 00000000  90                nop
; 00000001  7402              jz 0x5
; 00000003  7603              jna 0x8
; 00000005  90                nop
; 00000006  EB03              jmp short 0xb
; 00000008  90                nop
; 00000009  EB01              jmp short 0xc
; 0000000B  90                nop

