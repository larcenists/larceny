BITS 16
section .text
foo:
dec dword [eax]
inc word [eax]
dec byte [eax]
inc ch
dec esp
inc dx
