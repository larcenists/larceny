BITS 16
section .text
foo:
lmsw sp
smsw [esp]
