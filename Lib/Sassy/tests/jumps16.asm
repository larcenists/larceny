BITS 16
section .text
foo:
loop foo
loope foo, cx
loopz foo, ecx
loopne foo
loopnz foo, cx
jcxz foo
jecxz foo
call dword 0
call word 0
call dword 1000:1000
call word 1000:1000
call dword 1000:1000
call word 1000:1000
call far dword [ecx]
call far word [ecx]
call ecx
call dword [ecx]
call cx
call word [ecx]
jmp dword 0
jmp near dword 0
jmp word 0
jmp near word 0
jmp dword 1000:1000
jmp word 1000:1000
jmp dword 1000:foo
jmp word 1000:foo
jmp far dword [ecx]
jmp far word [ecx]
jmp ecx
jmp dword [ecx]
jmp cx
jmp word [ecx]
