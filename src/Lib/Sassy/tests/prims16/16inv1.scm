(bits 16)

(text
 (seq (nop)
      (inv (seq z!
		l!
		a!))
      (nop)))

; 00000000  90                nop
; 00000001  7504              jnz 0x7
; 00000003  7D02              jnl 0x7
; 00000005  7701              ja 0x8
; 00000007  90                nop

;In order for the outer seq to win all its arguments must win.
;In order for the inv to win its argument must lose.
;The argument to inv is seq, so far a seq to lose any one of its args must lose.
;The first two args are assertions, they lose if their opposite is true, and if their opposite is true the inv wins, hence the jcc's to 0x7.
;The last arg, also an assertion, wins if it is true, but in that case the inv will lose, causing the outer seq to lose, hance the jcc to 0x8, past everything.


