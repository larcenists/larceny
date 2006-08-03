(bits 16)

(text
 (while (< dh 20)
	(begin
	  (iter (seq (pop bx)
		     (!= bx 4)
		     (add ax bx)))
	  (add dx ax))))

; iter is sometimes better for inner loops, since both
; while and until always generate a jmp at their start
; to the test (the test is placed after their body).
; When while or until start the body of an outer while/until
; that means a jmp or jcc to a jmp will be generated.
; Using iter for the inner loop fixes this.

; 00000000  EB0C              jmp short 0xe
; 00000002  5B                pop bx
; 00000003  83FB04            cmp bx,byte +0x4
; 00000006  7404              jz 0xc
; 00000008  01D8              add ax,bx
; 0000000A  EBF6              jmp short 0x2
; 0000000C  01C2              add dx,ax
; 0000000E  80FE14            cmp dh,0x14
; 00000011  7CEF              jl 0x2
