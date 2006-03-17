(bits 16)

(text
 (label foo (ret))
 (label bar (ret))
 (begin
   (esc ((push $win))
	(alt
	 (with-win foo
		   (seq (= ax 3)
			(= bx 2)
			(push eax)
			(push ebx)))
	 (with-win bar
		   (seq (push ebx)
			(push eax)))))
   (nop)
   (nop)))


; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  681D00            push word 0x1d
; 00000005  3D0300            cmp ax,0x3
; 00000008  750C              jnz 0x16
; 0000000A  83FB02            cmp bx,byte +0x2
; 0000000D  7507              jnz 0x16
; 0000000F  6650              push eax
; 00000011  6653              push ebx
; 00000013  E9EAFF            jmp 0x0
; 00000016  6653              push ebx
; 00000018  6650              push eax
; 0000001A  E9E4FF            jmp 0x1
; 0000001D  90                nop
; 0000001E  90                nop

