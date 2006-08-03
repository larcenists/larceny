(bits 16)

(text
 (if (while (= al 3)
	    (seq (pop bx)
		 (pop ax)
		 (= bh 2)))
     (push dx)
     (push cx)))

; 00000000  EB07              jmp short 0x9
; 00000002  5B                pop bx
; 00000003  58                pop ax
; 00000004  80FF02            cmp bh,0x2
; 00000007  7507              jnz 0x10
; 00000009  3C03              cmp al,0x3
; 0000000B  74F5              jz 0x2
; 0000000D  52                push dx
; 0000000E  EB01              jmp short 0x11
; 00000010  51                push cx
