BITS 16
section .text
foo:
rcl ebp, 1
rcr dword [dword 1000+eax], 1
