BITS 32
section .text
foo:
adc al, 100
add ax, 1000
and eax, 50000
cmp bl, 100
or cx, word 1000
sbb edx, dword 50000
sub cx, byte 100
xor edx, byte 100
adc dword [100+eax+edx*4], dword 50000
add dword [edx], byte 100
and word [eax+edx], word 1000
cmp word [eax+edx], byte 100
or byte [100+eax+edx*4], byte 100
sbb bl, bl
sub [ebx], bl
xor cx, cx
adc [eax+edx], cx
add edx, edx
and [100+eax+edx*4], edx
cmp bl, [ebx]
or cx, [eax+edx]
sbb edx, [100+eax+edx*4]
