(text
 (label foo (ret))

 (seq (= ecx "DOIT")
      (locals (foo)
	(label foo
	  (with-win foo
	    (seq (pop eax)
		 (cmp eax 0)
		 nz!)))))
 
 (nop)
 (jmp foo))

; foo:
; 00000000  C3                ret
; 00000001  81F9444F4954      cmp ecx,0x54494f44
; 00000007  750A              jnz 0x13

; local foo:
; 00000009  58                pop eax
; 0000000A  83F800            cmp eax,byte +0x0
; 0000000D  0F85F6FFFFFF      jnz near 0x9
; 00000013  90                nop
; 00000014  E9E7FFFFFF        jmp 0x0
