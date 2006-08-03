(bits 16)

(text
  (nop)
  (label bar (jmp quux))
  (locals (bar quux)
    (nop)
    (label bar
      (jmp quux)
      (nop))
    (label quux
      (jmp bar)
      (nop)))
  (label quux (jmp bar)))



; 00000000  90                nop
; 00000001  E90900            jmp 0xd
; 00000004  90                nop
; 00000005  E90100            jmp 0x9
; 00000008  90                nop
; 00000009  E9F9FF            jmp 0x5
; 0000000C  90                nop
; 0000000D  E9F1FF            jmp 0x1
