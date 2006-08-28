(
(cmpxchg al bh)
(xadd    (& eax) bh)
(cmpxchg ax bx)
(xadd    (& eax) bx)
(cmpxchg eax ebx)
(xadd    (& eax) ebx)
)
