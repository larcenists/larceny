(bits 16)

(text
  (mov ax 10)
; this should loop 7 times
  (label foo
    (if (= ax 3)
	(with-win (ret))
	(with-win foo
	  (sub ax 1)))))

; 00000000  B80A00            mov ax,0xa
; 00000003  3D0300            cmp ax,0x3
; 00000006  7501              jnz 0x9
; 00000008  C3                ret
; 00000009  2D0100            sub ax,0x1
; 0000000C  E9F4FF            jmp 0x3


