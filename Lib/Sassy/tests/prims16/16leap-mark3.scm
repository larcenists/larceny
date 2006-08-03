(bits 16)

(text
 (label foo
  (leap
   (iter (seq (push dx)
	      (if (= ax 3)
		  (alt (seq (pop cx) z!)
		       (seq (push bx) z! (mark (pop bx)))
		       (seq (mov dx 3)))
		  (seq (= dx 4) (jmp foo)))
	      (push dx))))))

;hmm...

; 00000000  EB0C              jmp short 0xe
; 00000002  52                push dx
; 00000003  3D0300            cmp ax,0x3
; 00000006  750E              jnz 0x16
; 00000008  59                pop cx
; 00000009  7413              jz 0x1e
; 0000000B  53                push bx
; 0000000C  7503              jnz 0x11
; 0000000E  5B                pop bx
; 0000000F  EB0D              jmp short 0x1e
; 00000011  BA0300            mov dx,0x3
; 00000014  EB08              jmp short 0x1e
; 00000016  83FA04            cmp dx,byte +0x4
; 00000019  7506              jnz 0x21
; 0000001B  E9E2FF            jmp 0x0
; 0000001E  52                push dx
; 0000001F  EBE1              jmp short 0x2
