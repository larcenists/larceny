BITS 32
section .text
foo:
lmsw sp
smsw [esp]
