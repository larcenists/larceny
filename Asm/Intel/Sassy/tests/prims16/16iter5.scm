(bits 16)

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
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (add (& ecx (* edx 4) 1000) 1000)
	 (zero? eax)))))

;testing multi-pass iter for jump sizes

