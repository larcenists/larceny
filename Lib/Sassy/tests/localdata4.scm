(export _global_offset_table_)

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
	get-got
	(push ebx)
	(push 5)
	(push (& ebx (got foo)))
	(push stdout)
	(call write)
	(add esp 12)
	(pop ebx)
	(push 5)	;fd
	(mov eax (& ebx (got foo)))
	(add eax 8)
	(push (& eax))
	(push stdout)
	(call write)
	(call exit)))
