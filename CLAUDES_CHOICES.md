# Claude's choices: porting safehouse's fixes (2026-10-18)

The task: go through ~/Dev/LangPlay/safehouse/FIXED.md (bugs that
safehouse, another port of Larceny, found in the runtime, compiler and
assemblers), check whether each can be reproduced here, and fix those
that can, one fix per commit.  This file records the decisions taken
while doing that without the user at hand.

## How the list was triaged

- safehouse and accomplice share history up to e57eab0f (2026-10-10).
  A fix whose commit is an ancestor of master is already here.  For
  each of those, `git show <fix> | git apply -R --check` against
  master showed that the fix is still in the tree (or that later
  commits changed the same lines).  The recent merge of upstream
  (6ece1da9) only added files, so it undid none of them.
- Already here: 2.1-2.7, 2.8 (first half, b35db420), 2.9-2.11, 3.1,
  3.2, 3.5, 3.6, 4.2-4.5, 5.  Not examined further.
- Not here: 1.1-1.7, 2.8 (second half, 9d8c338d), 3.3, 3.4, 4.1 (and
  543e91ec, listed under 4.1).
- To fetch safehouse's commits, accomplice has a remote
  `safehouse-local` pointing at ~/Dev/LangPlay/safehouse.  It can be
  removed with `git remote remove safehouse-local`.

## Section 1 (the regional collector and base.sps): skipped

- I started a stress run of base.sps under -rrof here (384 runs), and
  the user then said that the base.sps -rrof crash may be a hardware
  fault of this machine that shows only under high load, and not to
  try to reproduce or fix it.  I stopped the run (no results kept)
  and left all of section 1 alone, since all of its entries came from
  the hunt for that crash.
- For the record, some of them are plain code defects that do not
  depend on the crash, and could be taken separately if wanted.  The
  defective lines are all still in accomplice: 1.2 (extbmp.c's
  `unsigned int delta` and `>` for a half-open range), 1.4
  (extbmp.c's `return bitmap_word & bit` as an int-sized `bool`), 1.5
  (memmgr.c's SSBs all created with data 0), 1.1 (cheney.c/cheney.h
  reading `gen_of` once before forwarding).

## Entries

### 2.8, second half: stop-and-copy stack overflow with a big frame

- Reproduced: a compiled procedure whose frame holds 1500 values
  (12KB), called recursively, hung under -stopcopy (killed after 60
  s); 0.24 s with the default collector.
- Fixed by safehouse's change to sc-heap.c (b3c7e9a3).  No test was
  added to test/GC: compiling a procedure with such a frame takes
  about 30 s, too slow for the suite.

### 3.3: fl=, fl<, fl<=, fl>, fl>= and fl-- looped forever (Fence)

- Reproduced: each of the six, called as a procedure, never returned
  (fl+, fl-, fl*, fl/ were fine).
- Safehouse's user chose, among dropping them, defining them with
  fl.sch's procedures, and making them primitives, the last; I took
  the same fix (4dedad64), so that the two trees agree.
- Safehouse's fix came with a new Twobit special form, `.primop`
  (54d46f59, used in 263 definitions).  I did not port it: it is a
  tool for finding such loops, not part of the fix.  Without it the
  definitions in primops.sch stay as they were, `(fl= a b)`, and
  integrate now that the table has the primitives.
- No change to the feature markers: the calling convention and
  millicode interface are unchanged; old compiled code calls the
  procedures, which now work.
- Tested on x86-64 only.  The emitters are in the shared
  pass5p2.sch, so AArch64 gets them too, but I could not build or run
  it here.

### 4.1: references to undefined names

- Reproduced three: `(bitwise-not 'a)` returned the symbol
  bitwise-not; the R6RS record mutator's out-of-range error and
  reading `#1#` reported "undefined global variable"
  (assertion-violaton, errSRFI38).  test/Lib/except.sch now tests
  them.
- The rest of safehouse's list could not be triggered from a program
  here (`#&` is rejected by the lexer before the reader's symBox,
  textual ports keep their own positions so utf16.sch's position
  lambda is never called, s8-list->bytevector is not exported, the
  others are on paths their callers guard).  I fixed them in the same
  commit anyway (b9eca68c), since each is a one-token repair of an
  obvious typo and the commit message says which were reproduced.
- errSRFI38 is code 16 here, not 15: upstream's merge added
  errIllegalNumvec as 15.
- Not ported from that commit: the definitions of assigned globals
  (`(define read)` and the like), karatsuba's and go's cleanups, and
  main.sch's .NET code.  They served safehouse's split of the runtime
  into libraries and fix no bug here.

### 543e91ec (listed under 4.1): ehandler.sch's `(bigdump* x port)`

- Not reproduced, and not fixed.  The call is in dstruct's last
  branch, reached only when the object, index and value are all valid,
  and then the primitive does not trap.  bigdump* takes one argument,
  so the call would itself fail if it were ever reached; safehouse
  gave bigdump* an optional port.  Left alone because it cannot be
  reached and the user asked for fixes of what can be reproduced.

### 3.4: Petit continuation ids against Twobit labels

- Reproduced in a Petit worktree (~/Dev/LangPlay/accomplice-petit,
  built with Racket from master): compile-file of one procedure
  holding 1100 called lambdas failed in gcc, "duplicate case value"
  (compiled_temp_2_2103 against label 2103).  Safehouse needed
  whole-library compilation to get there, but one large procedure is
  enough.
- Fixed with safehouse's change (9989faac); tested on Petit (all
  suites).

## Other things noticed

- Your TODO.md item about "Error while loading
  src/Build/<platform>-larceny-heap.sch" in --program mode: 4.5's fix
  (f49bb530) is here, but did not cover --program; safehouse's tree
  prints the same message, so it has no fix to port.  Left as a TODO.
- TODO.md had your uncommitted changes; I did not touch it or commit
  it.  The entries for this work are in DONE.md under 2026-10-18.
- CLAUDE.md says this Linux machine is small (1.6 GB, one CPU); it now
  has 128 CPUs and 250 GB, so I ran the suites in parallel.  I did not
  change CLAUDE.md.

## How things were tested

- Native x86-64 (this tree): rebuilt with `src/Build/bootstrap.sh
  --native --larceny ./larceny --r7rs` after each compiler or library
  change; test/Lib under the default collector, -stopcopy and -rrof,
  test/Compiler, test/R7RS/Lib's testall (39 passed) and
  testr6rs-larceny (15 passed), all in copies of the test directories.
- Petit x86-64: the same suites in ~/Dev/LangPlay/accomplice-petit,
  rebuilt with all four fixes.  That worktree is detached at b9eca68c
  with the Petit fix as an uncommitted change (identical to 9989faac);
  remove it with `git worktree remove --force ../accomplice-petit`
  when it is not wanted.
