# Claude's decisions

Questions that came up while working without the user at hand, and the
decision taken for the short term.  Each entry: the question, the
decision, and why.  Revisit any of them; when one is settled, say so
here (or move the outcome into the code, docs, or DONE.md).

## Native x86-64 Larceny (branch x86-64-native)

### 2026-10-10: Where to do the work?

- Question: native and Petit builds must not share a tree (CLAUDE.md),
  and the Petit build in ~/accomplice works.
- Decision: a git worktree, ~/accomplice-native, on a new branch
  `x86-64-native` made from `x86-64-linux`.
- Why: keeps the working Petit build (a fallback host, and a reference
  for test results) and keeps the native work's commits apart until it
  works.

### 2026-10-10: Register assignment

- Question: which x86-64 registers hold what?
- Decision: rax=TMP1 and rdx=TMP2 (assembler scratch, also IDIV's
  fixed registers), rcx=TMP0 ($r.tmp; cl is the shift count), rbx=
  GLOBALS (callee-saved in C), rbp=STKP, rsp stays the C stack,
  rsi=RESULT, rdi=SECOND, r8..r15=REG0..REG7, so LASTREG=7.  F0..F3
  are xmm0..xmm3 (saved by millicode), xmm15 a floating scratch.
- Why: 15 usable registers less 7 fixed roles leaves 8; putting the
  scratch registers on rax/rdx/rcx means IDIV, CQO, and shifts by cl
  need no shuffling of Scheme registers.  rbp is used (no frame
  pointer chain through Scheme code, as on AArch64 where x29 is left
  alone): one more MacScheme register seemed worth more than
  debugger backtraces through Scheme frames.  Could revisit: dropping
  TMP2 would free a register, since x86 immediates are 32 bits and few
  sequences need two scratch registers.

### 2026-10-10: Calls and returns

- Question: how do Scheme code, millicode, and return addresses work
  without BL/RET?
- Decision: millicode is called with CALL through the globals table
  (as BLR on AArch64); the millicode pops the return address, so rsp
  must be a valid C stack while Scheme runs (it is: Scheme never
  changes it otherwise).  Scheme calls store a return address made by
  LEA [rip+k] into the frame and JMP; returns are JMP [STKP+off].
  Return points are padded with 1-byte NOPs to 4-byte alignment.
- Why: frames live on the Scheme stack (STKP), so CALL/RET for Scheme
  calls would grow rsp without bound.  Using rsp as STKP (as
  IAssassin did) would let returns use the return-address predictor;
  left for later, since it needs signal handling on an alternate stack.

### 2026-10-10: Branch sizes

- Decision (the user agreed): forward branches always rel32, backward
  branches rel8 when they reach.  See TODO.md for later options.

### 2026-10-10: Testing the encoder

- Question: GNU as picks among several encodings of many x86
  instructions, so comparing bytes (as the AArch64 test does) would
  force the encoder to copy as's choices.
- Decision: test/Asm/x86-64 compares objdump disassemblies of the
  expected text (assembled by as) and of the encoder's bytes, case by
  case, each case in a section of its own.  Forward branches in the
  expected text use {disp32}.
