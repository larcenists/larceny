(bits 16)

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
; 00000002  E90000            jmp 0x5
; 00000005  90                nop
