# DONE

Finished items, moved here from TODO.md.  Newest first.

## 2026-09-30

- String ports keep their characters in a string instead of layering
  on bytevector ports under a UTF-8 transcoder (a layering from 2007,
  d41c1fdb, when buffers became bytevectors): the port's byte buffer
  is flushed into, or filled from, the string.  Opening a string output
  port costs 157 ns instead of 330; normalization 8.46 s -> 5.99
  (Racket 2.90).  Tests in `test/Lib/io.sch` (03b7dc0e).
- The copying collector's scan could stop before scanning all of
  to-space after forwarding large objects (45b12101); found through
  crashes of the vecsort benchmark under the profiler.  Test:
  `test/GC/scan-after-large-objects.sch`.
- String and bytevector copies with memmove, through a new syscall
  (a84df02d): the string benchmark takes 2.70 s instead of 7.62.
- Sorting vectors with a merge sort on vectors instead of lists
  (97d27658).
- AArch64: quotient and remainder of fixnums in line (68a4c316): 110 ms
  for 1e8 quotients instead of 387.
- equal? interleaves recursion and union-find (4e6fda33): the equal
  benchmark 0.46x Racket's time (3x before).
- In-line stack underflow on AArch64 (930bfa49), fxarithmetic-shift-right
  in line (1562e6bf; vecsort 5.27 s -> 3.41), and two-stage Unicode
  tables for normalization (5f7b5ae2;
  normalization 11.24 s -> 8.46).
- The compiler benchmark "outlier" (7x) was compile time: my benchmark
  harness ran the .sch, which run-r6rs-program expands and compiles;
  from the .slfasl it takes 2.2 s (Racket 3.0).
- Reading an unterminated string or |symbol| raises a lexical error
  instead of looping forever (19c4963c); found by the read0 benchmark.
- Petit's R7RS mode loads compiled libraries again: load-from-port
  evaluates the value of the #!fasl flag, which imports the Petit
  primitives (f4dd3e1c; broken since the feature markers, cc7da70a).
- Ran the R6RS benchmarks against Racket 9.3; results in TODO.md.
- FFI callouts on AArch64 MacOS, native and Petit (c08fcb59), and
  pointer-sized C types on 64-bit systems (f199b1df).
- Microsecond clocks: `current-second` is fractional, and
  `current-jiffy` has 1000000 jiffies per second on arm64 MacOS, through
  the FFI's gettimeofday (3a4462b1).

## 2026-09-29

- `-rrof` drops code referenced only from garbage when collecting the
  code space after a mark cycle, using the marks (e1761505); fixed an
  out-of-bounds region index that crashed minor collections when
  regions had been added since the cycle began (13c01da4).  Test:
  `test/GC/code-space.sch`.
- AArch64 millicode without a context switch for the SATB pre-barrier
  and for eqv? (flonums, immediates, eq? objects): memv of flonums 2.3x
  faster (fbacc6c0).
- Tighter loops: Twobit omits checks of variables known true in the
  consequent of an `if` (6e1638a2), so a guarded range check no longer
  materializes the loop test; AArch64 shifted-register adds for indexing
  (5237e641); the vector length comes from the type check's header load,
  and mutators skip `#!unspecified` when RESULT is dead (15c2d886).  A
  vector-sum loop is 25 instructions (was 32), 12% faster.
- Runtime diagnostics print words with `PRI?WORD` and pointers with
  `%p`; message functions are format-checked and the arm64 builds use
  `-Wformat` (176537b2).
- Objects up to 16GB on 64-bit systems, was 2GB (716da5b5): 64-bit
  byte offsets within objects in the regional collector, whose locsets
  truncated them and crashed on stores beyond 2GB (2efe8c91); nonmoving
  vectors had an uninitialized last slot (6ba39414).  Test:
  `test/GC/big-objects.sch`.
- Twobit on enormous single procedures: hashtrees ordered by a
  scrambled hash code, so the environments of pass 3 commoning stay
  balanced (7000 statements: 2.4 GB to 559 MB, 6e6b1427); pass 2
  computes escaping formals lazily (3aea5ccd); pass 1 finds source
  positions with an eq table, via a new compat-layer eq table API
  (fb7eb475).  8000 statements compile in under 2 s.
- AArch64 code layout: slow paths (timer, stack overflow, SATB barrier,
  generic arithmetic's flonum and millicode paths, safety checks' error
  calls) at the end of the code vector, branch-on-overflow fixnum
  operations, inline flonum paths for immediate operands (f219fd5c);
  switches FENCE_LONG_BRANCHES and FENCE_INLINE_DEFERRED for testing.
- The stop-and-copy heap no longer loops forever when a large frame
  cannot be restored from the heap (b35db420).
- Lighter millicode on AArch64: the generational write barrier in
  assembly without a context switch (pointer stores 2.5x faster,
  1759ca5e); inline flonum and mixed fixnum/flonum paths for generic
  arithmetic and comparison (2.5x-35x faster, fd7a9f24); paired stores for
  the context switch of the remaining calls (5c5f18ac).
- `-rrof` collects the code space even when marking is nearly
  continuous: a code collection requested during marking stays pending
  and runs between mark cycles; before, it was dropped, and code grew
  without bound (d4617689).
- The value of `set!` and of mutation primitives is `#!unspecified` in
  native and Petit code; before, it could be a global's value cell or a
  code vector (22ba9dad).
- `-nocontract` works again: the heap-size policy compared bytes with the
  peak heap size in words, so it kept only 1/8 of the peak (4f4cb540).
- Feature markers on compiled code: `#lap(...)`/`#lop(...)` with
  `assumes` and `guarantees` sets, in both readers (8922e52f); written by
  the compiler into `.lap`, `.lop`, and `.fasl` files (718fac3f); checked
  by make (a mismatch is out of date), at startup, and when loading
  `.fasl` files, against what the heap's code assumes and what the
  collector requires; `gc-technology` reports `regional` (cc7da70a).
- `build-config-files` rewrites generated files only when they change,
  so builds no longer recompile everything (f9cae291); `bootstrap.sh`
  can be hosted by the tree's own Larceny (514d2611).
- Compiled files (`.lap`, `.lop`, `.fasl`) made by the build's make
  rules are out of date when the compiler or assembler sources are newer
  (`make:tool-files`); `larceny.bin` is installed read-only by renaming
  a fresh copy into place; `'clean` keeps the bootstrap logs (4dd308c4).
  The `-rrof` failures in this tree came from library code compiled
  before the SATB pre-barrier existed and reused by later heap builds.
- GC byte counts are 64-bit on 64-bit systems (`s_word`): semispaces,
  the low-level allocator, heap sizes and policies, statistics, heap
  images, and command-line sizes (which now accept a G suffix).  Before,
  a heap over 2 GB overflowed them: the statistics went negative and the
  stop-and-copy collector looped (c606805d).
- Code space memory is included in the owning generation's statistics
  (90a7fb9e).
- Native: long branch sequences for code vectors that may exceed the
  +/-1 MB reach of conditional branches (5c083edb).
- Native: zero stores from xzr, no redundant frame-header stores,
  LDP/STP pairing of adjacent loads and stores (e917577c).
- Native: calls and returns use BL/BLR and RET, so the return-address
  predictor works; call-heavy code 13-20% faster (cf9a21c5).
- Native: allocation in line (fixed-size and make-vector-style) when the
  nursery has room; allocation-heavy loops 1.4x-4.5x faster, now faster
  than Petit (e997e334).
- Nightly-build scripts (`test/Scripts/nightly-build`, `checkout-git`)
  know macosx-arm64, native and Petit, hosted by Larceny or Racket (175aec4d).
- `(build-larceny-heap)` / `(build-twobit-heap)` for the second build
  stage; `src/Build/bootstrap.sh` over host (Racket or Larceny) and
  target (native or Petit); Larceny hosts of another kind load build
  files from source (38263b70).
- Native `-rrof`: SATB pre-write barrier in the Fence code generator;
  under the regional collector the code space belongs to the static
  generation and is collected by a scanning pass (29a90872).
- `setup` records the build configuration in `build-configuration` and
  refuses to mix configurations; `'clean` removes the build products
  (via git), `'keep-build-products` keeps them (d14b9927).
- Racket-hosted builds write `.lop` and `.fasl` files in Larceny syntax,
  so Larceny can read them (46333896).
- REPL/debugger no longer loop forever ("Read error on port") when the
  console must be reopened but there is no controlling terminal; the
  console falls back to /dev/null (27a26528).
- Native build in the main tree, bootstrapped with the Larceny in
  `../accomplice-native2`; R7RS/R6RS libraries compiled.
- Ignore `arm64.heap` and `bootstrap.log`.
- `src/Build/bootstrap-arm64-native.sh`: one command to bootstrap native
  AArch64 Larceny from Racket (3509d216).
- Code space collects code when much code is installed (0524c5d6).
- Native Larceny for AArch64 on macOS (34a6a685), with the collectable
  code space (f15e8caa), 64-bit support in the Fence code generator
  (2557ab85), and the AArch64 back end (272bbe8b).  Passes test/Lib, the
  compiler tests, and the R7RS (8782) and R6RS (6260) suites with the
  default and stop-and-copy collectors.
- 64-bit Petit Larceny on macOS/arm64, bootstrapped from Racket, with the
  regional collector working (62135dce).
