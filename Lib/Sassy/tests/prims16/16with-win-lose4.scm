(bits 16)

(text
 (label foo (push bx))
 (with-win-lose (ret) (call foo)
  (seq
   (push ax)
   (= cx 4))))

; 00000000  53                push bx
; 00000001  50                push ax
; 00000002  83F904            cmp cx,byte +0x4
; 00000005  7501              jnz 0x8
; 00000007  C3                ret
; 00000008  E8F5FF            call 0x0
