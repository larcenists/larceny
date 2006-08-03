(text
 (label foo (ret))
 (label bar (ret))
 (with-win bar
   (if (= eax 3)
       (push eax)
       (with-win foo
         (push ebx)))))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  83F803            cmp eax,byte +0x3
; 00000005  7506              jnz 0xd
; 00000007  50                push eax
; 00000008  E9F4FFFFFF        jmp 0x1
; 0000000D  53                push ebx
; 0000000E  E9EDFFFFFF        jmp 0x0

