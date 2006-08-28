BITS 32
section .text
foo:
bound ax, [edi]
bound eax, [edi]
bswap edx
cmpxchg8b [edx+ecx]
enter 1000, 100
xchg ax, bx
xchg bx, ax
xchg eax, ebx
xchg ebx, eax
xchg al, ah
xchg [ebx], ah
xchg cx, bx
xchg [ecx], bx
xchg ecx, ebx
xchg [ecx], ebx
xchg al, [edi]
xchg ax, [edi]
xchg eax, [edi]
