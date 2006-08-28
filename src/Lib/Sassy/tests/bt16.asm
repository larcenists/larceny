BITS 16
section .text
foo:
bt si, si
btc [dword 300+edx], si
btr [8*eax], edi
bts edi, edi
bt si, 9
btc word [dword 300+edx], byte 9
btr edi, 9
bts dword [8*eax], byte 9
