(export _global_offset_table_ say-hello the-string1)
(import a-string)

(data (label the-string1 (bytes "Hello "))
      (label the-string2 (bytes "World." #\newline))
      (label boxed-one (dwords (sym the-string1))))

(macro stdout 1)

(macro write (lambda (fd buffer amount)
	       `(begin (mov ecx ,buffer)
		       (mov ebx ,fd)
		       (mov edx ,amount)
		       (mov eax ,4)
		       (int #x80))))
(text
 (label exit (mov eax 1)
       (mov ebx 0)
       (int #x80))
 (label say-hello get-got
	    (push ebx)
	    (lea eax (& ebx (got-offset boxed-one)))
	    (write stdout (& eax) 6)
	    (mov ebx (& esp))
	    (lea eax (& ebx (got-offset the-string2)))
	    (write stdout eax 7)
	    (mov ebx (& esp))
	    (write stdout (& ebx (got a-string)) 9)
	    (jmp exit)))

