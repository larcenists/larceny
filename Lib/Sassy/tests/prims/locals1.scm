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
; 00000001  6807000000        push dword 0x7
; 00000006  90                nop
; 00000007  6801000000        push dword 0x1
; 0000000C  90                nop


