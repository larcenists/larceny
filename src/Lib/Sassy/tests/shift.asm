BITS 32
section .text
foo:
rcl ebp, 1
rcr dword [dword 1000+eax], 1
rol ebp, cl
ror dword [dword 1000+eax], cl
sal ebp, 9
sar dword [dword 1000+eax], byte 9
shl dh, 1
shr byte [ecx+ebx], 1
rcl dh, cl
rcr byte [ecx+ebx], cl
rol dh, 9
ror byte [ecx+ebx], 9
sal bx, 1
sar word [dword 1000], 1
shl bx, cl
shr word [dword 1000], cl
rcl bx, 9
rcr word [dword 1000], 9
