BITS 32
section .text
foo:
mov [es:eax], dword 3
add edx, [ds:ebx]
xor ecx, [cs:4+eax]
push dword [dword es:4]
sub eax, [dword fs:foo]
and dword [dword gs:foo+edx], byte 3
