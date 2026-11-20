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

### 2026-10-10: Names in the build

- Question: what do the build, the runtime, and Scheme call the new
  system?
- Decision: target machine `x86-64` (beside `arm`; machine source
  "Fence"), runtime and heap type `x86-64-native`, makefile
  configuration `x86-64-linux-gcc`, feature set
  `features-x86-64-linux` with a new architecture feature `X86_64`,
  heap `x86-64.heap`, heap scripts `src/Build/x86-64-linux-*-heap.sch`
  (copies of the arm64 ones), architecture name "X86-64" (code 6 in
  primitive.c's arch-name; R7RS cond-expand feature `x86-64`).  The
  arm64 heap project in Lib/makefile.sch became `fence64-heap-project`,
  shared by both.  The few runtime tests of `ARM` that mean "Fence"
  became `ARM || X86_64` rather than introducing a FENCE feature.
- Why: follows the arm64 port's shape, so the two can be compared and
  merged later; a FENCE feature would touch the ARM builds too.

### 2026-10-10: Code space on Linux

- Decision: use CODE_SPACE (as arm64 does), mapped read-write-execute
  (no MAP_JIT on Linux); bootstrap heaps are split heaps.
- Why: it is the tested path of the 64-bit native system; keeping code
  in the data heap would need the old executable-heap path, untested
  on 64 bits.

### 2026-10-10: Millicode return addresses must be aligned too

- Found: the address after CALL to millicode is saved (relative to the
  code) in G_RETADDR, which the collector scans as a root, so it must
  look like a fixnum: cant.trap pads with NOPs so the call ends on a
  4-byte boundary (on AArch64 every instruction is 4 bytes, so this
  never came up).

### 2026-10-10: A Twobit bug exposed by 8 registers

- Found: in a conditional in tail position whose arms share the current
  frame (Twobit gives the arms frames of their own only when that frame
  is unused), the then arm's own conditionals could get frames of their
  own, and the else arm then use the shared frame: the inner save
  instructions nested inside it, and a tail call (pop of the inner frame
  only) left the outer frame on the stack, so a later return jumped to
  its zero return address (get-line on a UTF-8 port, in
  io/get-line-maybe's scan).  With 16 or 32 registers the frame is used
  less often, so the case had not come up.
- Decision: fixed in Twobit (cg-if-result, pass4p1.sch): no own frames
  for the arms while the current frame is shared with an arm yet to be
  compiled.  Compiling iosys.sch, 122 of 124 such frames remain.
- Note: this changes code generation for every target (only where the
  bug could arise); tested on native x86-64 (and Petit, see the commit).
  Not retested on macOS/arm64.
