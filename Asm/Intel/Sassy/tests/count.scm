(entry _start)

(import exit mybuff)

(export exit-code)

(data (label exit-code (bytes 0)))

(macro stdout 1)

(macro write (lambda (fd buffer amount)
	       `(begin (mov ebx ,fd)
		       (mov ecx ,buffer)
		       (mov edx ,amount)
		       (mov eax ,4)
		       (int #x80))))

(text
 (label _start (mov ecx 0)
	 (mov eax "0")
	 (while (<= eax #\9)
		(begin (push eax)
		       (mov (& mybuff) al)
		       (write stdout mybuff 1)
		       (pop eax)
		       (add eax 1)))
	 (jmp exit)))

