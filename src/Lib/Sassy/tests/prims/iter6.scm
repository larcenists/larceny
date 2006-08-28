(text
 (iter
  (begin (iter (seq (pop ebx)
		    (= ebx 2)))
	 (iter (seq (pop ebx)
		    (= ebx 3))))))

; 00000000  5B                pop ebx
; 00000001  83FB02            cmp ebx,byte +0x2
; 00000004  74FA              jz 0x0
; 00000006  5B                pop ebx
; 00000007  83FB03            cmp ebx,byte +0x3
; 0000000A  74FA              jz 0x6

; Why it makes sense that the outer loop doesn't loop:
; Iter can never win, it can only loop forever or lose, in which case
; the loop is exited. The begin wins or loses depending on its last
; tail. In this case, the last tail can never win, since it's an iter,
; but it can lose. But if it loses, the whole begin loses, in which
; case the outer iter loses and is exited.

; In this case, you can put '(seq) as the last item of the
; begin. Since '(seq) always wins, the begin always wins and the outer
; loop never exits, so the last jmp will be generated:

; (text
;  (iter
;   (begin (iter (seq (pop ebx)
; 		    (= ebx 2)))
; 	 (iter (seq (pop ebx)
; 		    (= ebx 3)))
; 	 (seq))))

; 00000000  5B                pop ebx
; 00000001  83FB02            cmp ebx,byte +0x2
; 00000004  74FA              jz 0x0
; 00000006  5B                pop ebx
; 00000007  83FB03            cmp ebx,byte +0x3
; 0000000A  74FA              jz 0x6
; 0000000C  EBF2              jmp short 0x0

