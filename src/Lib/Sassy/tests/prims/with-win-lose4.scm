(text
 (label foo (push ebx))
 (with-win-lose (ret) (call foo)
  (seq
   (push eax)
   (= ecx 4))))

; 00000000  53                push ebx
; 00000001  50                push eax
; 00000002  83F904            cmp ecx,byte +0x4
; 00000005  7501              jnz 0x8
; 00000007  C3                ret
; 00000008  E8F3FFFFFF        call 0x0
