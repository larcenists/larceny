BITS 32
section .text
foo:
fadd dword [eax]
fsub qword [ebx]
fsubr st0, st4
fmul st7, st0
fdiv dword [eax]
fdivr qword [ebx]
fdivrp st2, st0
fdivp st2, st0
fmulp st2, st0
fsubp st3, st0
fsubrp st2, st0
faddp st4, st0
fimul dword [eax]
fiadd word [ebx]
fidiv word [ebx]
fidivr dword [eax]
fisub word [ebx]
fisubr dword [eax]
