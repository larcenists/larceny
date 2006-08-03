(export exit mybuff)

(heap (label mybuff (bytes 1)))

(import exit-code)

(text
 (label exit (begin
	 (mov eax 1)
	 (mov ebx (& exit-code))
	 (int #x80))))
