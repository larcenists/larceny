BITS 16
section .text
foo:
in al, 9
in ax, 9
in eax, 9
in al, dx
in ax, dx
in eax, dx
out 9, al
out 9, ax
out 9, eax
out dx, al
out dx, ax
out dx, eax
int 128
pop cx
pop ecx
pop dword [ecx]
pop word [ecx]
pop ds
pop es
pop ss
pop fs
pop gs
push cx
push edx
push dword 100
push word 100
push byte 100
push word [esi]
push dword [esi]
push cs
push ds
push es
push ss
push fs
push gs
imul ax, bx, word 100
imul ax, [ebx], word 100
imul ax, bx, byte 100
imul ax, [ebx], byte 100
imul eax, ebx, dword 100
imul eax, [ebx], dword 100
imul eax, ebx, byte 100
imul eax, [ebx], byte 100
imul eax, dword 100
imul eax, byte 100
imul ax, word 100
imul ax, byte 100
imul ax, bx
imul ax, [ebx]
imul eax, ebx
imul eax, [ebx]
imul al
imul byte [eax]
imul ax
imul word [eax]
imul eax
imul dword [eax]
test al, 9
test ax, 9
test eax, 9
test bl, cl
test [ebx], cl
test bx, cx
test [ebx], cx
test ebx, ecx
test [ebx], ecx
test bl, 9
test dword [ebx], 9
test bx, 9
test word [ebx], 9
test ebx, 9
test byte [ebx], 9
mov bl, cl
mov [edx], cl
mov bx, dx
mov [esi], dx
mov ebx, edi
mov [eax], edi
mov bl, cl
mov bl, [edx]
mov bx, dx
mov bx, [esi]
mov ebx, edi
mov ebx, [eax]
mov bl, 9
mov bx, 9
mov ebx, 9
mov [edx], byte 9
mov [esi], word 9
mov [eax], dword 9
mov al, [dword 9]
mov ax, [dword 9]
mov eax, [dword 9]
mov [dword 9], al
mov [dword 9], ax
mov [dword 9], eax
mov bx, ds
mov [esi], ss
mov ebx, fs
mov [eax], es
mov gs, dx
mov ds, [esi]
mov fs, edi
mov ss, [eax]
mov ecx, cr2
mov ecx, dr1
mov cr0, edx
mov dr0, edx
