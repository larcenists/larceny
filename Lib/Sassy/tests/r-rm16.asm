BITS 16
section .text
foo:
bsf bx, ax
bsr bx, [eax]
