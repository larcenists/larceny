(bits 16)

(text
 (seq (nop)
      (inv (alt z!
		l!
		a!))
      (nop)))

; 00000000  90                nop
; 00000001  7405              jz 0x8
; 00000003  7C03              jl 0x8
; 00000005  7701              ja 0x8
; 00000007  90                nop

;In order for the outer seq to win its arguments must win. In order
;for the inv to win its argument must lose.  It's argument is alt, and
;for an alt to lose all its arguments must lose.  Therefore if any of
;its arguments win, the inv will lose as will the outer seq. Hence the
;jcc's past everything.
