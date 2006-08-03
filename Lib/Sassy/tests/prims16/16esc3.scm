(bits 16)

(text
 (begin (push $eip)
	(push $win)
	(push $lose))
 (seq (push $eip)
      (push $win)
      (push $lose)
      (nop)))

; 00000000  680300            push word 0x3
; 00000003  680600            push word 0x6
; 00000006  680900            push word 0x9
; 00000009  680C00            push word 0xc
; 0000000C  680F00            push word 0xf
; 0000000F  681300            push word 0x13
; 00000012  90                nop

