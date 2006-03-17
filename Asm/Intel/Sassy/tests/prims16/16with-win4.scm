(bits 16)

(text
 (label foo (push bx))
 (seq (with-win (ret)
		(push ax))))

; 00000000  53                push bx
; 00000001  50                push ax
; 00000002  C3                ret
