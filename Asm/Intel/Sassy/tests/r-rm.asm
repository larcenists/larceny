BITS 32
section .text
foo:
bsf bx, ax
bsr bx, [eax]
