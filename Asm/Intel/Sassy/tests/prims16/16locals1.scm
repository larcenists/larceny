(bits 16)

(text
  (locals (bar quux)
    (nop)
    (label bar
      (push quux)
      (nop))
    (label quux
      (push bar)
      (nop))))

; 00000000  90                nop
; 00000001  680500            push word 0x5
; 00000004  90                nop
; 00000005  680100            push word 0x1
; 00000008  90                nop

