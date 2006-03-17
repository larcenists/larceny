(bits 16)

(text
 (label foo (ret))
 (label bar (ret))
 (with-win bar
   (if (= eax 3)
       (push eax)
       (push ebx))))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  6683F803          cmp eax,byte +0x3
; 00000006  7505              jnz 0xd
; 00000008  6650              push eax
; 0000000A  E9F4FF            jmp 0x1
; 0000000D  6653              push ebx
; 0000000F  E9EFFF            jmp 0x1
