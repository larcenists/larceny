(text
  (mov eax 10)
; this should loop 7 times
  (label foo
    (if (= eax 3)
	(with-win (ret))
	(with-win foo
	  (sub eax 1)))))

; 00000000  B80A000000        mov eax,0xa
; 00000005  83F803            cmp eax,byte +0x3
; 00000008  7501              jnz 0xb
; 0000000A  C3                ret
; 0000000B  83E801            sub eax,byte +0x1
; 0000000E  E9F2FFFFFF        jmp 0x5
