BITS 32
section .text
foo:
loop foo
loope foo, cx
loopz foo, ecx
loopne foo
loopnz foo, cx
jcxz foo
jecxz foo
call dword 0
call word 0
call dword 1000:1000
call word 1000:1000
call far dword [ecx]
call far word [ecx]
call ecx
call dword [ecx]
call cx
call word [ecx]
jmp dword 0
jmp near dword 0
jmp word 0
jmp near word 0
jmp dword 1000:1000
jmp word 1000:1000
jmp far dword [ecx]
jmp far word [ecx]
jmp ecx
jmp dword [ecx]
jmp cx
jmp word [ecx]
add eax, [ecx]
add eax, [ebp]
add eax, [esp]
add eax, [100]
add eax, [1600]
add eax, [ecx*1]
add eax, [ecx*2]
add eax, [ecx*4]
add eax, [ecx*8]
add eax, [ebp*1]
add eax, [ebp*2]
add eax, [ebp*4]
add eax, [ebp*8]
add eax, [ecx+100]
add eax, [ecx+1600]
add eax, [ebp+100]
add eax, [ebp+1600]
add eax, [esp+100]
add eax, [esp+1600]
add eax, [100+ecx]
add eax, [1600+ecx]
add eax, [100+ebp]
add eax, [1000+ebp+600]
add eax, [100+esp]
add eax, [1600+esp]
add eax, [ecx+edx*1]
add eax, [ebp*1+ecx]
add eax, [ecx+edx*2]
add eax, [ebp*2+ecx]
add eax, [ecx+edx*4]
add eax, [ebp*4+ecx]
add eax, [ecx+edx*8]
add eax, [ecx+ebp*8]
add eax, [edx*1+ebp]
add eax, [ebp+ebp*1]
add eax, [ebp+edx*2]
add eax, [ebp+ebp*2]
add eax, [ebp+edx*4]
add eax, [ebp*4+ebp]
add eax, [ebp+edx*8]
add eax, [ebp+ebp*8]
add eax, [esp+edx*1]
add eax, [esp+ebp*1]
add eax, [esp+edx*2]
add eax, [ebp*2+esp]
add eax, [esp+edx*4]
add eax, [esp+ebp*4]
add eax, [esp+edx*8]
add eax, [esp+ebp*8]
add eax, [ecx*1+100]
add eax, [ecx*2+100]
add eax, [100+ecx*4]
add eax, [ecx*8+100]
add eax, [100+ebp*1]
add eax, [ebp*2+100]
add eax, [100+ebp*4]
add eax, [ebp*8+100]
add eax, [1600+ecx*1]
add eax, [ecx*2+1600]
add eax, [ecx*4+1600]
add eax, [1600+ecx*8]
add eax, [ebp*1+1600]
add eax, [1600+ebp*2]
add eax, [ebp*4+1600]
add eax, [ebp*8+1600]
add eax, [ecx+100+edx*1]
add eax, [ecx+edx*2+100]
add eax, [edx*4+ecx+100]
add eax, [ecx+edx*8+100]
add eax, [ecx+ebp*1+100]
add eax, [ecx+ebp*2+100]
add eax, [ecx+ebp*4+100]
add eax, [100+ecx+ebp*8]
add eax, [ebp+edx*1+100]
add eax, [ebp+edx*2+100]
add eax, [ebp+edx*4+100]
add eax, [ebp+edx*8+100]
add eax, [ebp+ebp*1+100]
add eax, [100+ebp*2+ebp]
add eax, [ebp+ebp*4+100]
add eax, [ebp+ebp*8+100]
add eax, [esp+edx*1+100]
add eax, [esp+edx*2+100]
add eax, [esp+edx*4+100]
add eax, [esp+edx*8+100]
add eax, [esp+ebp*1+100]
add eax, [esp+ebp*2+100]
add eax, [esp+ebp*4+100]
add eax, [esp+ebp*8+100]
add eax, [ecx+edx*1+1600]
add eax, [ecx+edx*2+1600]
add eax, [ecx+edx*4+1600]
add eax, [ecx+edx*8+1600]
add eax, [ecx+ebp*1+1600]
add eax, [ecx+ebp*2+1600]
add eax, [ecx+ebp*4+1600]
add eax, [ecx+ebp*8+1600]
add eax, [ebp+edx*1+1600]
add eax, [ebp+edx*2+1600]
add eax, [ebp+edx*4+1600]
add eax, [ebp+edx*8+1600]
add eax, [ebp+ebp*1+1600]
add eax, [ebp+ebp*2+1600]
add eax, [ebp+ebp*4+1600]
add eax, [ebp+ebp*8+1600]
add eax, [esp+edx*1+1600]
add eax, [esp+edx*2+1600]
add eax, [esp+edx*4+1600]
add eax, [esp+edx*8+1600]
add eax, [esp+ebp*1+1600]
add eax, [esp+ebp*2+1600]
add eax, [esp+ebp*4+1600]
add eax, [esp+ebp*8+1600]
aaa
aas
cbw
cdq
clc
cld
cli
clts
cmc
cmpsb
cmpsw
cmpsd
cpuid
cwde
cwd
daa
das
hlt
insb
insw
insd
int3
into
invd
iret
iretw
iretd
lahf
leave
lodsb
lodsw
lodsd
movsb
movsw
movsd
nop
outsb
outsw
outsd
popa
popaw
popad
popf
popfw
popfd
pusha
pushaw
pushad
pushf
pushfw
pushfd
rdmsr
rdpmc
rdtsc
rsm
sahf
scasb
scasw
scasd
stc
std
sti
stosb
stosw
stosd
ud2
wbinvd
wrmsr
xlat
xlatb
sysenter
sysexit
adc al, 100
add ax, 1000
and eax, 50000
cmp bl, 100
sbb cx, word 1000
xor edx, dword 50000
add cx, byte 100
cmp edx, byte 100
adc dword [eax+edx*4+100], dword 50000
or dword [edx], byte 100
sub word [eax+edx], word 1000
and word [eax+edx], byte 100
or byte [eax+edx*4+100], byte 100
sbb bl, bl
sub [ebx], bl
xor cx, cx
adc [eax+edx], cx
add edx, edx
and [eax+edx*4+100], edx
cmp bl, [ebx]
or cx, [eax+edx]
sbb edx, [eax+edx*4+100]
bt si, si
btc [100+200+edx], si
btr [8*eax], edi
bts edi, edi
bt si, 9
btc word [100+200+edx], byte 9
btr edi, 9
bts dword [8*eax], byte 9
rcl ebp, 1
rcr dword [eax+1000], 1
seto ah
setno [eax+esi*1]
setb ah
setc [eax+esi*1]
setnae ah
setnb [eax+esi*1]
setnc ah
setae [eax+esi*1]
sete ah
setz [eax+esi*1]
setne ah
setnz [eax+esi*1]
setbe ah
setna [eax+esi*1]
seta ah
setnbe [eax+esi*1]
sets ah
setns [eax+esi*1]
setp ah
setpe [eax+esi*1]
setnp ah
setpo [eax+esi*1]
setl ah
setnge [eax+esi*1]
setge ah
setnl [eax+esi*1]
setle ah
setng [eax+esi*1]
setnle ah
setg [eax+esi*1]
cmovo cx, bx
cmovno bx, [edx+esi*4]
cmovb edx, eax
cmovc ebp, [esp+eax*4]
cmovnae cx, bx
cmovnb bx, [edx+esi*4]
cmovnc edx, eax
cmovae ebp, [esp+eax*4]
cmove cx, bx
cmovz bx, [edx+esi*4]
cmovne edx, eax
cmovnz ebp, [esp+eax*4]
cmovbe cx, bx
cmovna bx, [edx+esi*4]
cmova edx, eax
cmovnbe ebp, [esp+eax*4]
cmovs cx, bx
cmovns bx, [edx+esi*4]
cmovp edx, eax
cmovpe ebp, [esp+eax*4]
cmovnp cx, bx
cmovpo bx, [edx+esi*4]
cmovl edx, eax
cmovnge ebp, [esp+eax*4]
cmovge cx, bx
cmovnl bx, [edx+esi*4]
cmovle edx, eax
cmovng ebp, [esp+eax*4]
cmovnle cx, bx
cmovg bx, [edx+esi*4]
dec dword [eax]
inc word [eax]
dec byte [eax]
inc ch
dec esp
inc dx
div dword [ebx]
idiv word [ebx]
mul byte [ebx]
neg edi
not edi
div edi
lds edi, [esi]
les di, [esi]
lea edi, [esi]
lfs di, [esi]
lgs edi, [esi]
lss di, [esi]
movsx ebp, ax
movzx ebp, word [edi]
movsx ebp, al
movzx ebp, byte [edi]
movsx bp, ah
movzx bp, [edi]
bsf bx, ax
bsr bx, [eax]
lar ebx, eax
lsl ebx, [eax]
invlpg [200+8*esi+esp+100]
lgdt [200+8*esi+esp+100]
sgdt [200+8*esi+esp+100]
lidt [200+8*esi+esp+100]
sidt [200+8*esi+esp+100]
lldt sp
sldt [esp]
lmsw sp
smsw [esp]
ltr sp
str [esp]
verr sp
verw [esp]
aad
aam 9
ret
retn 1000
retf
shld bx, ax, 9
shrd [ebx], ax, 9
shld ebx, eax, 9
shrd [ebx], eax, 9
shld bx, ax, cl
shrd [ebx], ax, cl
shld ebx, eax, cl
shrd [ebx], eax, cl
cmpxchg al, bh
xadd [eax], bh
cmpxchg ax, bx
xadd [eax], bx
cmpxchg eax, ebx
xadd [eax], ebx
arpl cx, bx
arpl [ecx], bx
bound ax, [edi]
bound eax, [edi]
bswap edx
cmpxchg8b [edx+ecx]
enter 1000, 100
xchg ax, bx
xchg bx, ax
xchg eax, ebx
xchg ebx, eax
xchg al, ah
xchg [ebx], ah
xchg cx, bx
xchg [ecx], bx
xchg ecx, ebx
xchg [ecx], ebx
xchg al, [edi]
xchg ax, [edi]
xchg eax, [edi]
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
mov al, [9]
mov ax, [9]
mov eax, [9]
mov [9], al
mov [9], ax
mov [9], eax
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
jo near dword 0
jno near word 0
jb 0
jc near dword 0
jnae near word 0
jnb 0
jnc near dword 0
jae near word 0
je 0
jz near dword 0
jne near word 0
jnz 0
jbe near dword 0
jna near word 0
ja 0
jnbe near dword 0
js near word 0
jns 0
jp near dword 0
jpe near word 0
jnp 0
jpo near dword 0
jl near word 0
jnge 0
jge near dword 0
jnl near word 0
jle 0
jng near dword 0
jnle near word 0
jg 0
rep insd
rep outsw
rep lodsb
rep stosd
rep movsb
repe cmpsb
repz cmpsd
repne scasd
repnz scasb
lock add byte [eax], 1
lock dec dword [edx]
lock xor [ecx], ecx
fld1
fldl2t
fldl2e
fldpi
fldlg2
fldln2
fldz
fsin
fcos
fsincos
fptan
fpatan
f2xm1
fyl2x
fyl2xp1
fincstp
fdecstp
finit
fninit
fclex
fnclex
fwait
wait
fnop
fcompp
fucompp
ftst
fxam
fprem
fprem1
fabs
fchs
frndint
fscale
fsqrt
fxtract
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
fcmovb st0, st2
fcmove st0, st3
fcmovbe st0, st4
fcmovu st0, st5
fcmovnb st0, st6
fcmovne st0, st7
fcmovnbe st0, st1
fcmovnu st0, st2
fxch
fucom st3
fld tword [eax]
fstp qword [ebx]
fld dword [ecx]
fstp st4
fst dword [edx]
fst qword [ebx]
fst st3
fild word [ebx]
fistp dword [ebx]
fild qword [ebx]
fist word [ecx]
ficom dword [ecx]
ficomp word [ecx]
fcomp dword [edi]
fcom qword [edi]
fcomp st0
fcomi st0, st7
fcomip st0, st6
fucomi st0, st5
fucomip st0, st4
fbld tword [eax]
fbstp tword [eax]
fstcw word [ebx]
fldcw word [ebx]
fnstcw word [ebx]
fstenv [eax]
fnstenv [ebx]
ffree st2
fldenv [edx]
fsave [edx]
fnsave [edx]
frstor [edx]
fxsave [edx]
fxrstor [edx]
fstsw ax
fstsw word [ebx]
fnstsw ax
fnstsw word [ebx]
emms
movd mm1, ebx
movd mm1, [edx]
movd ebx, mm1
movd [edx], mm1
movd xmm0, ebx
movd xmm0, [edx]
movd ebx, xmm0
movd [edx], xmm0
movq mm1, mm0
movq mm1, [edx]
movq mm0, mm1
movq [edx], mm1
movq xmm0, xmm1
movq xmm0, [edx]
movq xmm1, xmm0
movq [edx], xmm0
pand mm0, mm1
pandn mm1, [edx]
por xmm2, xmm3
pxor xmm4, [ecx]
packsswb mm0, mm1
packssdw mm1, [edx]
packuswb xmm2, xmm3
punpckhbw xmm4, [ecx]
punpckhwd mm0, mm1
punpckhdq mm1, [edx]
paddb xmm2, xmm3
paddw xmm4, [ecx]
paddd mm0, mm1
paddsb mm1, [edx]
paddsw xmm2, xmm3
paddusb xmm4, [ecx]
paddusw mm0, mm1
psubb mm1, [edx]
psubw xmm2, xmm3
psubd xmm4, [ecx]
psubsb mm0, mm1
psubsw mm1, [edx]
psubusb xmm2, xmm3
psubusw xmm4, [ecx]
pmullw mm0, mm1
pmulhw mm1, [edx]
pmaddwd xmm2, xmm3
pcmpeqb xmm4, [ecx]
pcmpeqw mm0, mm1
pcmpeqd mm1, [edx]
pcmpgtb xmm2, xmm3
pcmpgtw xmm4, [ecx]
pcmpgtd mm0, mm1
punpcklbw mm2, mm3
punpcklwd mm4, [ebx]
punpckldq xmm5, xmm6
punpcklbw xmm7, [edx]
psrlw mm7, mm6
psrld mm5, [edx]
psrlq xmm7, xmm6
psllw xmm5, [edx]
pslld mm4, 100
psllq xmm4, 100
psraw mm7, mm6
psrad mm5, [edx]
movaps xmm5, xmm4
movups xmm3, [edx]
movaps [edx], xmm2
movups xmm5, xmm4
addps xmm3, xmm0
subps xmm0, [edx]
mulps xmm3, xmm0
divps xmm0, [edx]
rcpps xmm3, xmm0
sqrtps xmm0, [edx]
rsqrtps xmm3, xmm0
maxps xmm0, [edx]
minps xmm3, xmm0
andps xmm0, [edx]
andnps xmm3, xmm0
orps xmm0, [edx]
xorps xmm3, xmm0
unpckhps xmm0, [edx]
unpcklps xmm3, xmm0
addss xmm0, xmm1
subss xmm0, [eax]
mulss xmm0, xmm1
divss xmm0, [eax]
rcpss xmm0, xmm1
sqrtss xmm0, [eax]
rsqrtss xmm0, xmm1
maxss xmm0, [eax]
minss xmm0, xmm1
comiss xmm0, [eax]
ucomiss xmm0, xmm1
pavgb mm0, mm1
pavgw mm2, [edx]
pmaxub xmm3, xmm4
pmaxsw xmm5, [edx]
pminub mm0, mm1
pminsw mm2, [edx]
pmulhuw xmm3, xmm4
psadbw xmm5, [edx]
movhps xmm0, [edx]
movlps [edx], xmm6
movhlps xmm0, xmm1
movlhps xmm1, xmm0
shufps xmm0, xmm6, 10
cmpps xmm5, [ecx], 20
prefetcht0 [eax]
prefetcht1 [eax]
prefetcht2 [eax]
prefetchnta [eax]
sfence
movntps [ecx], xmm3
maskmovq mm3, mm4
movntq [ebx], mm7
pmovmskb eax, mm0
pmovmskb ecx, xmm5
pshufw mm0, mm1, 40
pshufw mm0, [ebx], 30
ldmxcsr [ebx]
stmxcsr [edx]
pinsrw mm1, eax, 4
pinsrw mm0, [edx], 5
pinsrw xmm4, ebx, 6
pinsrw xmm3, [ecx], 7
pextrw edx, mm3, 28
pextrw esi, mm0, 14
cvtsi2ss xmm3, edi
cvtsi2ss xmm3, [edi]
cvtpi2ps xmm4, mm2
cvtpi2ps xmm4, [edi+100]
movss xmm1, [edx]
movss xmm1, xmm6
movss [edx], xmm3
movmskps ebx, xmm3
cmpss xmm3, xmm4, 10
cmpss xmm3, [edx], 20
cvttss2si ebx, xmm3
cvtss2si eax, [ecx]
cvtps2pi mm3, xmm1
cvttps2pi mm4, [edx]
movapd xmm0, xmm1
movupd xmm2, [edx]
movdqa [eax], xmm3
movdqu xmm0, xmm1
movhpd xmm0, [ecx]
movmskpd ebx, xmm4
movlpd [ecx], xmm0
movsd xmm5, xmm6
movsd xmm7, [esi]
movsd [esi], xmm7
addpd xmm0, xmm1
subpd xmm2, [ecx]
mulpd xmm0, xmm1
divpd xmm2, [ecx]
sqrtpd xmm0, xmm1
maxpd xmm2, [ecx]
minpd xmm0, xmm1
andpd xmm2, [ecx]
andnpd xmm0, xmm1
orpd xmm2, [ecx]
xorpd xmm0, xmm1
unpckhpd xmm2, [ecx]
unpcklpd xmm0, xmm1
cvtpd2dq xmm2, [ecx]
cvttpd2dq xmm0, xmm1
cvtdq2ps xmm2, [ecx]
cvtps2dq xmm0, xmm1
cvttps2dq xmm2, [ecx]
cvtpd2ps xmm0, xmm1
punpckhqdq xmm2, [ecx]
punpcklqdq xmm0, xmm1
addsd xmm2, xmm3
subsd xmm4, [edi]
mulsd xmm2, xmm3
divsd xmm4, [edi]
maxsd xmm2, xmm3
minsd xmm4, [edi]
sqrtsd xmm2, xmm3
comisd xmm4, [edi]
ucomisd xmm2, xmm3
cvtdq2pd xmm4, [edi]
cvtps2pd xmm2, xmm3
cvtsd2ss xmm4, [edi]
cmppd xmm0, xmm1, 10
shufpd xmm2, [ebx], 20
pshuflw xmm0, xmm1, 10
pshufhw xmm2, [ebx], 20
pshufd xmm0, xmm1, 10
cmpsd xmm4, xmm5, 20
cmpsd xmm6, [eax], 30
cvttpd2pi mm0, xmm1
cvtpd2pi mm0, [ebp]
pause
lfence
mfence
clflush [ebx]
pmuludq mm0, mm1
paddq mm0, [edx]
psubq xmm3, xmm1
pmuludq xmm3, [edx]
maskmovdqu xmm1, xmm2
movnti [edx], eax
movq2dq xmm3, mm4
movdq2q mm5, xmm7
movntpd [eax], xmm3
movntdq [ebx], xmm4
pslldq xmm3, 20
psrldq xmm3, 20
cvtpi2pd xmm3, mm4
cvtpi2pd xmm3, [ecx]
cvtss2sd xmm3, xmm6
cvtss2sd xmm3, [edx]
cvtsd2si ecx, xmm3
cvttsd2si edx, [edx]
cvtsi2sd xmm3, eax
cvtsi2sd xmm3, [eax]
monitor
mwait
lddqu xmm0, [edx]
movddup xmm0, xmm1
addsubps xmm0, xmm1
addsubpd xmm2, [edx]
haddps xmm0, xmm1
hsubps xmm2, [edx]
haddpd xmm0, xmm1
hsubpd xmm2, [edx]
movshdup xmm0, xmm1
movsldup xmm2, [edx]
movddup xmm0, [edx]
