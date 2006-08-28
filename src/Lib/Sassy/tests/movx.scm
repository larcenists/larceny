(
(movsx ebp ax)
(movzx ebp (word (& edi)))
(movsx ebp al)
(movzx ebp (byte (& edi)))
(movsx bp ah)
(movzx bp (& edi))
)
