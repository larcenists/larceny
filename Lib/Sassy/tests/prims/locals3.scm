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
; bar:
; 00000001  E90D000000        jmp 0x13 ; jmp quux
; 00000006  90                nop
; local bar:
; 00000007  E901000000        jmp 0xd  ; jmp local quux
; 0000000C  90                nop
; local quux:
; 0000000D  E9F5FFFFFF        jmp 0x7  ; jmp local bar
; 00000012  90                nop
; quux:
; 00000013  E9E9FFFFFF        jmp 0x1  ; jmp bar
