BITS 32
section .text
foo:
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
cvtpi2ps xmm4, [100+edi]
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
