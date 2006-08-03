(text
 (label foo (ret))
 (label bar (ret))
 (begin
   (esc ((push $win))
	(alt
	 (with-win foo
		   (seq (= eax 3)
			(= ebx 2)
			(push eax)
			(push ebx)))
	 (with-win bar
		   (seq (push ebx)
			(push eax)))))
   (nop)
   (nop)))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  681F000000        push dword 0x1f
; 00000007  83F803            cmp eax,byte +0x3
; 0000000A  750C              jnz 0x18
; 0000000C  83FB02            cmp ebx,byte +0x2
; 0000000F  7507              jnz 0x18
; 00000011  50                push eax
; 00000012  53                push ebx
; 00000013  E9E8FFFFFF        jmp 0x0
; 00000018  53                push ebx
; 00000019  50                push eax
; 0000001A  E9E2FFFFFF        jmp 0x1
; 0000001F  90                nop
; 00000020  90                nop
