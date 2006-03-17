(text
 (begin (push $eip)
	(push $win)
	(push $lose))
 (seq (push $eip)
      (push $win)
      (push $lose)
      (nop)))

; 00000000  6805000000        push dword 0x5
; 00000005  680A000000        push dword 0xa
; 0000000A  680F000000        push dword 0xf
; 0000000F  6814000000        push dword 0x14
; 00000014  6819000000        push dword 0x19
; 00000019  681F000000        push dword 0x1f
; 0000001E  90                nop
