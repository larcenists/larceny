(export _global_offset_table_)
(import a-string)
(macro stdout 1)
(macro write (lambda (fd buffer amount)
	       `(begin (mov ecx ,buffer)
		       (mov ebx ,fd)
		       (mov edx ,amount)
		       (mov eax ,4)
		       (int #x80))))
(macro exit (lambda (exit-code)
	      `(begin (mov eax 1)
		      (mov ebx ,exit-code)
		      (int #x80))))
(entry _start)
(text (label _start get-got
	      (write stdout (& ebx (got a-string)) 9)
	      (exit 0)))


