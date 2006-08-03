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

; foo:
; 00000000  C3                ret
; bar:
; 00000001  C3                ret
; qudr:
; 00000002  C3                ret
; 00000003  90                nop
; 00000004  0F85F6FFFFFF      jnz near 0x0
; 0000000A  0F8CF1FFFFFF      jl near 0x1
; 00000010  0F86ECFFFFFF      jna near 0x2
; 00000016  90                nop




 