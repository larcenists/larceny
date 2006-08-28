(bits 16)

(text
 (label foo (ret))
 (label bar (ret))
 (label qudr (ret))
 (seq
  (nop)
  (with-lose foo z!)
  (with-lose bar ge!)
  (with-lose qudr a!)
  (nop)))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  C3                ret
; 00000003  90                nop
; 00000004  0F85F8FF          jnz near 0x0
; 00000008  0F8CF5FF          jl near 0x1
; 0000000C  0F86F2FF          jna near 0x2
; 00000010  90                nop




 