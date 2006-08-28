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

; 00000000  E800000000        call 0x5
; 00000005  E800000000        call 0xa
; 0000000A  E800000000        call 0xf
; 0000000F  E900000000        jmp 0x14
; 00000014  E900000000        jmp 0x19
; 00000019  E901000000        jmp 0x1f
; 0000001E  90                nop
; 0000001F  EB00              jmp short 0x21
; 00000021  EB00              jmp short 0x23
; 00000023  EB01              jmp short 0x26
; 00000025  90                nop
; 00000026  E90D000000        jmp 0x38
; 0000002B  EB0B              jmp short 0x38
; 0000002D  E902000000        jmp 0x34
; 00000032  EB04              jmp short 0x38
; 00000034  90                nop
; 00000035  EB01              jmp short 0x38
; 00000037  90                nop
