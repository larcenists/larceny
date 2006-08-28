(bits 16)

(macro my-while
  (lambda (test . body)
    `(locals (loop)
       (label loop
	 (with-win (jmp short loop)
	   (seq ,test
		(begin ,@body)))))))

(text
  (label loop

    (nop)
    
    (my-while (< ax 10)
	      (pop bx)
	      (add ax bx)))
  
  (my-while (> cx 1000)
	    (pop bx)
	    (sub cx bx))

  (jmp short loop))

; 00000000  90                nop
; 00000001  3D0A00            cmp ax,0xa
; 00000004  7D05              jnl 0xb
; 00000006  5B                pop bx
; 00000007  01D8              add ax,bx
; 00000009  EBF6              jmp short 0x1
; 0000000B  81F9E803          cmp cx,0x3e8
; 0000000F  7E05              jng 0x16
; 00000011  5B                pop bx
; 00000012  29D9              sub cx,bx
; 00000014  EBF5              jmp short 0xb
; 00000016  EBE8              jmp short 0x0

