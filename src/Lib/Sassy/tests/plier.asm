BITS 32
section .text
foo:
div dword [ebx]
idiv word [ebx]
mul byte [ebx]
div al
idiv ax
mul edx
neg edi
not edi
neg dword [ecx]
not word [eax]
