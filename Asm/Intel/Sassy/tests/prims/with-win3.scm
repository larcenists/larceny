(text
 (label foo (ret))
 (label bar (ret))
 (if (while (!= eax 3)
	    (seq (pop eax)
		 (inc eax)
		 (< eax 10)))
     (with-win bar
	       (alt z! a!))
     (with-win foo
	       (push ebx))))

; foo:
; 00000000  C3                ret

; bar:
; 00000001  C3                ret

; 00000002  EB07              jmp short 0xb
; 00000004  58                pop eax
; 00000005  40                inc eax
; 00000006  83F80A            cmp eax,byte +0xa
; 00000009  7D12              jnl 0x1d
; 0000000B  83F803            cmp eax,byte +0x3
; 0000000E  75F4              jnz 0x4
; 00000010  0F84EBFFFFFF      jz near 0x1
; 00000016  760B              jna 0x23
; 00000018  E9E4FFFFFF        jmp 0x1
; 0000001D  53                push ebx
; 0000001E  E9DDFFFFFF        jmp 0x0

