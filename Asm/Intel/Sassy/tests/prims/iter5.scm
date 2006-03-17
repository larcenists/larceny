(text
 (begin
   (iter
    (alt (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (zero? eax)))
   (iter
    (alt (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (zero? eax)))))

;testing multi-pass iter for jump sizes

; 00000000  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 0000000B  EBF3              jmp short 0x0
; 0000000D  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000018  EBE6              jmp short 0x0
; 0000001A  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000025  EBD9              jmp short 0x0
; 00000027  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000032  EBCC              jmp short 0x0
; 00000034  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 0000003F  EBBF              jmp short 0x0
; 00000041  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 0000004C  EBB2              jmp short 0x0
; 0000004E  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000059  EBA5              jmp short 0x0
; 0000005B  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000066  EB98              jmp short 0x0
; 00000068  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000073  EB8B              jmp short 0x0
; 00000075  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000080  E97BFFFFFF        jmp 0x0
; 00000085  85C0              test eax,eax
; 00000087  0F8473FFFFFF      jz near 0x0
; 0000008D  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000098  EBF3              jmp short 0x8d
; 0000009A  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000A5  EBE6              jmp short 0x8d
; 000000A7  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000B2  EBD9              jmp short 0x8d
; 000000B4  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000BF  EBCC              jmp short 0x8d
; 000000C1  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000CC  EBBF              jmp short 0x8d
; 000000CE  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000D9  EBB2              jmp short 0x8d
; 000000DB  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000E6  EBA5              jmp short 0x8d
; 000000E8  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 000000F3  EB98              jmp short 0x8d
; 000000F5  818491E8030000E8  add dword [ecx+edx*4+0x3e8],0x3e8
;          -030000
; 00000100  EB8B              jmp short 0x8d
; 00000102  85C0              test eax,eax
; 00000104  7487              jz 0x8d
