(bits 16)

(text
 (label foo (push bx))
 (with-win-lose
  (jmp 1000)
  (call foo)
  (seq
   (push ax)
   (= cx 4))))

; 00000000  53                push bx
; 00000001  50                push ax
; 00000002  83F904            cmp cx,byte +0x4
; 00000005  7503              jnz 0xa
; 00000007  E9DE03            jmp 0x3e8
; 0000000A  E8F3FF            call 0x0
