BITS 16
section .text
foo:
invlpg [dword 300+8*esi+esp]
lgdt [dword 300+8*esi+esp]
sgdt [dword 300+8*esi+esp]
lidt [dword 300+8*esi+esp]
sidt [dword 300+8*esi+esp]
