(entry _start)

(text
 (label _start
	(begin
	  (mov eax 1)
	  (mov ebx 0)
	  (int #x80))))
