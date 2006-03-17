(bits 16)

(text
 (begin (call $eip)
	(call $win)
	(call $lose))
 (seq (jmp $eip)
      (jmp $win)
      (jmp $lose)
      (nop))
 (seq (jmp short $eip)
      (jmp short $win)
      (jmp short $lose)
      (nop))
 (alt (jmp $win)
      (jmp $lose)
      (nop)
      (nop)))

; 00000000  E80000            call 0x3
; 00000003  E80000            call 0x6
; 00000006  E80000            call 0x9
; 00000009  E90000            jmp 0xc
; 0000000C  E90000            jmp 0xf
; 0000000F  E90100            jmp 0x13
; 00000012  90                nop
; 00000013  EB00              jmp short 0x15
; 00000015  EB00              jmp short 0x17
; 00000017  EB01              jmp short 0x1a
; 00000019  90                nop
; 0000001A  E90B00            jmp 0x28
; 0000001D  EB09              jmp short 0x28
; 0000001F  E90200            jmp 0x24
; 00000022  EB04              jmp short 0x28
; 00000024  90                nop
; 00000025  EB01              jmp short 0x28
; 00000027  90                nop
