(bits 16)

(text
 (label foo (ret))
 (label bar (ret))
 (seq (nop)
      (esc ((push $win)
	    (mov dx $lose))
	   (alt
	    (with-win foo
		      (seq (= ax 3)
			   (= bx 2)
			   (push ax)
			   (push bx)))
	    (with-win bar
		      (seq (mov sp dx)
			   (push bx)
			   (push ax)))))
      (nop)))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  90                nop
; 00000003  681F00            push word 0x1f
; 00000006  BA2000            mov dx,0x20
; 00000009  3D0300            cmp ax,0x3
; 0000000C  750A              jnz 0x18
; 0000000E  83FB02            cmp bx,byte +0x2
; 00000011  7505              jnz 0x18
; 00000013  50                push ax
; 00000014  53                push bx
; 00000015  E9E8FF            jmp 0x0
; 00000018  89D4              mov sp,dx
; 0000001A  53                push bx
; 0000001B  50                push ax
; 0000001C  E9E2FF            jmp 0x1
; 0000001F  90                nop
