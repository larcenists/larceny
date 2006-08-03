; testing out skipping of undefined declarations

(text
 (label bar
   (nop))
 (locals (bar quux)
   (nop)
   (label quux)
   (jmp bar)
   (nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  E900000000        jmp 0x7
; 00000007  90                nop
