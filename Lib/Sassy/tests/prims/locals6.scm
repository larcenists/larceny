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
    
    (my-while (< eax 10)
	      (pop ebx)
	      (add eax ebx)))
  
  (my-while (> ecx 1000)
	    (pop ebx)
	    (sub ecx ebx))

  (jmp short loop))

; loop:
; 00000000  90                nop

; local loop (#1):
; 00000001  83F80A            cmp eax,byte +0xa
; 00000004  7D05              jnl 0xb
; 00000006  5B                pop ebx
; 00000007  01D8              add eax,ebx
; 00000009  EBF6              jmp short 0x1

; local loop (#2):
; 0000000B  81F9E8030000      cmp ecx,0x3e8
; 00000011  7E05              jng 0x18
; 00000013  5B                pop ebx
; 00000014  29D9              sub ecx,ebx
; 00000016  EBF3              jmp short 0xb

; 00000018  EBE6              jmp short 0x0
