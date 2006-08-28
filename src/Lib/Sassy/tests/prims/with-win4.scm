(text
 (label foo (push ebx))
 (seq (with-win (ret)
		(push eax))))

; 00000000  53                push ebx
; 00000001  50                push eax
; 00000002  C3                ret

