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
; bar:
; 00000001  6813000000        push dword 0x13 ; push quux
; 00000006  90                nop
; local bar:
; 00000007  680D000000        push dword 0xd  ; push local quux
; 0000000C  90                nop
; local quux:
; 0000000D  6807000000        push dword 0x7  ; push local bar
; 00000012  90                nop
; quux:
; 00000013  6801000000        push dword 0x1  ; push bar


