(
(monitor)
(mwait)
(lddqu xmm0 (& edx))
; (fisttp (dword (& edx))) nasm, ndisasm, and objdump all disagree about these
; (fisttp (word (& edx))) but I've double checked, and they should be right
; (fisttp (qword (& edx))) (unless the INTEL documentation is wrong)
(movddup xmm0 xmm1)
(addsubps xmm0 xmm1)
(addsubpd xmm2 (& edx))
(haddps xmm0 xmm1)
(hsubps xmm2 (& edx))
(haddpd xmm0 xmm1)
(hsubpd xmm2 (& edx))
(movshdup xmm0 xmm1)
(movsldup xmm2 (& edx))
(movddup xmm0 (& edx))
)
