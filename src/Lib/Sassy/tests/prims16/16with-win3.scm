(bits 16)

(text
 (label foo (ret))
 (label bar (ret))
 (if (while (!= ax 3)
	    (seq (pop ax)
		 (inc ax)
		 (< ax 10)))
     (with-win bar
	       (alt z! a!))
     (with-win foo
	       (push bx))))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  EB07              jmp short 0xb
; 00000004  58                pop ax
; 00000005  40                inc ax
; 00000006  3D0A00            cmp ax,0xa
; 00000009  7D0E              jnl 0x19
; 0000000B  3D0300            cmp ax,0x3
; 0000000E  75F4              jnz 0x4
; 00000010  0F84EDFF          jz near 0x1
; 00000014  7607              jna 0x1d
; 00000016  E9E8FF            jmp 0x1
; 00000019  53                push bx
; 0000001A  E9E3FF            jmp 0x0
