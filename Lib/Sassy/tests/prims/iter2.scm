(text
  (iter
    (seq (nop)
	 (seq z! a!)
	 (nop)
	 ge!)))

; 00000000  90                nop
; 00000001  7505              jnz 0x8
; 00000003  7603              jna 0x8
; 00000005  90                nop
; 00000006  7DF8              jnl 0x0

