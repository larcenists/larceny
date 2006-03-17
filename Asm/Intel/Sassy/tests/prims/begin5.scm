(text
 (begin (nop)
	(inv (alt z!
		  l!
		  a!))
	(nop)))

;The arguments to a begin always take as their win and lose the
;following arg in the sequence, except for the last, which takes
;begin's win and lose.  Since the assertions only generate jcc's, they
;would all be jcc's to the last (nop). But it happens that the last
;(nop) is the instruction following the a! assertion. But a jmp to the
;next instruction is really doing nothing at all, so it may be
;eliminated. But then the l! assertion is in the same circumstance,
;and so on. Hence:

; 00000000  90                nop
; 00000001  90                nop
