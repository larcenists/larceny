(bits 16)

(text
 (label foo (ret))

 (seq (= cx "DO")
      (locals (foo)
	(label foo
	  (with-win foo
	    (seq (pop ax)
		 (cmp ax 0)
		 nz!)))))
 
 (nop)
 (jmp foo))

; 00000000  C3                ret
; 00000001  81F9444F          cmp cx,0x4f44
; 00000005  7508              jnz 0xf
; 00000007  58                pop ax
; 00000008  3D0000            cmp ax,0x0
; 0000000B  0F85F8FF          jnz near 0x7
; 0000000F  90                nop
; 00000010  E9EDFF            jmp 0x0

