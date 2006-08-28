(text
 (label foo
  (leap
   (iter (seq (push edx)
	      (if (= eax 3)
		  (alt (seq (pop ecx) z!)
		       (seq (push ebx) z! (mark (pop ebx)))
		       (seq (mov edx 3)))
		  (seq (= edx 4) (jmp foo)))
	      (push edx))))))

;hmm...


; 00000000  EB0C              jmp short 0xe
; 00000002  52                push edx
; 00000003  83F803            cmp eax,byte +0x3
; 00000006  7510              jnz 0x18
; 00000008  59                pop ecx
; 00000009  7417              jz 0x22
; 0000000B  53                push ebx
; 0000000C  7503              jnz 0x11
; 0000000E  5B                pop ebx
; 0000000F  EB11              jmp short 0x22
; 00000011  BA03000000        mov edx,0x3
; 00000016  EB0A              jmp short 0x22
; 00000018  83FA04            cmp edx,byte +0x4
; 0000001B  7508              jnz 0x25
; 0000001D  E9DEFFFFFF        jmp 0x0
; 00000022  52                push edx
; 00000023  EBDD              jmp short 0x2
