BITS 32
section .text
foo:
arpl cx, bx
arpl [ecx], bx
rsm
lldt sp
sldt [esp]
ltr sp
str [esp]
verr sp
verw [esp]
lar ebx, eax
lsl ebx, [eax]
