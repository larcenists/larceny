(import foo)

(entry _start)

(macro stdout 1)

(text (label write ; fd buffer amount
	(mov ebx (& esp 4))
	(mov ecx (& esp 8))
	(mov edx (& esp 12))
	(mov eax 4)
	(int #x80)
	(ret)))

(text (label exit
	(mov eax 1)
	(mov ebx 0)
	(int #x80)))

(text (label _start
	(push 5)	;fd
	(push foo)	;buffer
	(push stdout)	;amount
	(call write)
	(add esp 12)
	(push 5)	;fd
	(push (& foo 16));buffer - in this case its the pointer to the local foo
	(push stdout)
	(call write)
	(call exit)))




