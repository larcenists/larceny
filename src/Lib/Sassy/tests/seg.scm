(
(mov (es (& eax)) (dword 3))
(add edx (ds (& ebx)))
(xor ecx (cs (& eax 4)))
(push (dword (es (& 4))))
(sub eax (fs (& foo)))
(and (dword (gs (& foo edx))) (byte 3))
)
