(text
 (iter (begin
	 (nop)
	 (label foo (nop))
	 (nop)))
 (jmp foo))

; 00000000  90                nop
; foo:
; 00000001  90                nop
; 00000002  90                nop
; 00000003  EBFB              jmp short 0x0
; 00000005  E9F7FFFFFF        jmp 0x1 ; jmp foo


