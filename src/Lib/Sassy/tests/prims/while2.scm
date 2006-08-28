(text
 (if (until (= eax 3)
	    (seq (pop ebx)
		 (pop eax)
		 (= ebx 2)))
     (push edx)
     (push ecx)))


; 00000000  EB07              jmp short 0x9
; 00000002  5B                pop ebx
; 00000003  58                pop eax
; 00000004  83FB02            cmp ebx,byte +0x2
; 00000007  7508              jnz 0x11
; 00000009  83F803            cmp eax,byte +0x3
; 0000000C  75F4              jnz 0x2
; 0000000E  52                push edx
; 0000000F  EB01              jmp short 0x12
; 00000011  51                push ecx

