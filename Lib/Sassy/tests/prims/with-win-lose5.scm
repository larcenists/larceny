(text
 (label foo (push ebx))
 (with-win-lose
  (jmp 1000)
  (call foo)
  (seq
   (push eax)
   (= ecx 4))))

; 00000000  53                push ebx
; 00000001  50                push eax
; 00000002  83F904            cmp ecx,byte +0x4
; 00000005  7505              jnz 0xc
; 00000007  E9DC030000        jmp 0x3e8
; 0000000C  E8EFFFFFFF        call 0x0

