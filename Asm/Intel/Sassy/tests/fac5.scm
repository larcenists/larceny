(export _start)

(text
 (label _start
  (mov eax 1)
  (mov ecx 5)
  (while (> ecx 0)
	 (begin (mul ecx)
		(sub ecx 1))))
 (mov ebx eax)
 (mov eax 1)
 (int #x80))
