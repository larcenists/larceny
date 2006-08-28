(sassy-make-elf
 "tests/quick-elf-new.o"
 (sassy
  '((text
     (foo (begin
	    (push ebp)
	    (mov  ebp esp)
	    (get-got)
	    (lea eax (gotoff quux 4))
	    (mov ecx (got qadr))
	    (call (plt wizo))
	    (jmp wizo))))
    (import qadr wizo)
    (export got-name)
    (data (quux (dwords 100 200)))
    (data (pointer (dwords (sym quux)))))))
		 