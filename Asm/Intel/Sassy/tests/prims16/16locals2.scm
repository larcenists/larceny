(bits 16)

(text
  (nop)
  (label bar
    (push quux))
  (locals (bar quux)
    (nop)
    (label bar
      (push quux)
      (nop))
    (label quux
      (push bar)
      (nop)))
  (label quux (push bar)))

; 00000000  90                nop
; 00000001  680D00            push word 0xd
; 00000004  90                nop
; 00000005  680900            push word 0x9
; 00000008  90                nop
; 00000009  680500            push word 0x5
; 0000000C  90                nop
; 0000000D  680100            push word 0x1

