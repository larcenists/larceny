(text
 (label foo (ret))
 (label bar (ret))
 (seq (nop)
      (esc ((jmp $win)
	    (jmp $lose))
	   (alt
	    (with-win foo
		      (seq (= eax 3)
			   (= ebx 2)
			   (push eax)
			   (push ebx)))
	    (with-win bar
		      (seq (mov esp edx)
			   (push ebx)
			   (push eax)))))
      (nop)))

; 00000000  C3                ret
; 00000001  C3                ret
; 00000002  90                nop
; 00000003  E91F000000        jmp 0x27
; 00000008  E91B000000        jmp 0x28
; 0000000D  83F803            cmp eax,byte +0x3
; 00000010  750C              jnz 0x1e
; 00000012  83FB02            cmp ebx,byte +0x2
; 00000015  7507              jnz 0x1e
; 00000017  50                push eax
; 00000018  53                push ebx
; 00000019  E9E2FFFFFF        jmp 0x0
; 0000001E  89D4              mov esp,edx
; 00000020  53                push ebx
; 00000021  50                push eax
; 00000022  E9DAFFFFFF        jmp 0x1
; 00000027  90                nop
