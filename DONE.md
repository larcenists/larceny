# DONE

Finished items, moved here from TODO.md.  Newest first.

## 2026-10-25

- current-jiffy is nanoseconds of the monotonic clock on 64-bit Unix
  systems (6db40b83), a fixnum from a new syscall, not microseconds of the
  time of day: no allocation, no jumps when the clock is set, and a call
  0.18 us -> 32 ns (x86-64 Linux).

## 2026-10-24

- current-jiffy no longer loads the FFI (c2b42415; lockpick's LARCENY-NOTES.md
  item 11): its first call took 0.3 s, now about 1 us (later calls 30 us
  to 0.18 us).
- x86-64: a timer tick is subq $1 of the timer in memory and a jb (ec0308e1;
  lockpick's LARCENY-NOTES.md item 12), not a load, test, decrement and
  store through a register: 3.4 instructions fewer per loop iteration,
  11% fewer cycles in a loop summing vector elements.  Keeping the timer
  in a register, as lockpick suggested, would need a register x86-64
  does not have free.
- Accessors and mutators of R7RS define-record-type types without a
  parent are open-coded, in any library (0735da4c; lockpick's LARCENY-NOTES.md
  item 15): 10^8 accessor calls 0.44 s to 0.32 s, unsafe 0.13 s.
- Lambda lifting no longer makes a procedure take more arguments than
  fit in registers (e49e8c10; lockpick's LARCENY-NOTES.md item 10): on
  x86-64 a named let with free variables consed the arguments beyond 5
  on every iteration.  Lockpick's case went from 40 million words
  allocated over 10^7 iterations to none, and 109 ms to 83 ms.  Calls
  written with more than 5 arguments still cons.
- The x86-64 work merged on 2026-10-23 and 2026-10-24, tested on
  native AArch64 macOS (none of it needed changes there):
  - fl=, fl<, fl<=, fl>, fl>= and fl-- as primitives (4dedad64): in
    line, as values, with three arguments, -0., and errors for
    non-flonums.
  - The open-coded bytevector accessors, with whole-word loads and
    stores and the one-test alignment check (b1a2dd94, 8a784063,
    eef4e099): the unaligned byte offsets use LDUR/STUR, and low4 and
    low5 are TST masks 15 and 31.  test/Lib's bytevector-accessors.sch
    passes.
  - The open-coded fixnum shifts (eb1c9144, 60f8c39a, 7a3691ac):
    test/Lib's fixnums.sch passes.
  - scheme-dump-heap.sch (7247d26e, 8f5d3509, 2f856cbf, 96373491):
    interactive heaps dumped under the default collector, -stopcopy and
    -rrof start and run under all three (globals, eq? hashtables across
    collections, closures, bignums, call/cc, eval, errors); a heap
    dumped from an R7RS session with (larceny dump-heap) keeps its
    libraries' state.
  - At 94674ce0: test/Lib, test/Compiler (sanity switches), R7RS
    testall (8782 passed) and testr6rs-larceny under --r7rs and --r6rs
    (6260 passed each) pass under the default collector, -stopcopy and
    -rrof.
- fxarithmetic-shift-left and fxarithmetic-shift are open-coded (60f8c39a,
  7a3691ac; lockpick's LARCENY-NOTES.md item 4), with errors of the kinds R6RS
  asks for, and the count checks of the fixnum shifts no longer make
  booleans; on x86-64, shifts by a count in rcx no longer exchange
  registers.  (fxarithmetic-shift-left 1 k) went from 0.142 s to 0.023 s
  over 10^7 iterations, as fast as lockpick's vector of powers.
- The 16-, 32- and 64-bit bytevector accessors are open-coded by
  compiler macros (bytevector-accessor-rules in Compiler/common.imp.sch;
  b1a2dd94, with .fxlsh taking an immediate shift count, eb1c9144), for
  lockpick's LARCENY-NOTES.md item 1: on x86-64, summing 10^7 elements
  with bytevector-u32-native-ref went from 0.244 s to 0.059 s (vector-ref:
  0.020 s), u64 from 0.357 s to 0.099 s.  Fixed with it: an out-of-bounds
  read by bytevector-u16-ref at the greatest fixnum index, and stores of
  out-of-range values without an error.  Then (8a784063) 32- and 64-bit values
  in the native byte order are loaded and stored whole on 64-bit Fence
  targets: with the index stepped by the size, u32-native-ref 0.027 s
  and u64-native-ref 0.038 s against vector-ref's 0.022 s.  Then (eef4e099)
  the alignment check is one test and the bounds check shares the type
  check's header load: u32-native-ref 0.023 s, u64-native-ref 0.033 s,
  u32-native-set! 0.020 s (vector-ref, vector-set!: 0.021 s).  What is
  left is in TODO.md.
- An interactive heap dumped from an R7RS session (`--r7rs`) could not
  start, from dump-interactive-heap as from scheme-dump-interactive-heap:
  it kept the session's load evaluator (aeryn-evaluator, which
  evaluates in the R7RS top level), so loading startup.sch failed.  The
  REPL now loads the init files with the load evaluator the heap was
  built with (96373491).  Heaps dumped from R7RS sessions before then
  still fail.
- Heap dumping under any collector, in Scheme with no runtime changes:
  `scheme-dump-heap` and `scheme-dump-interactive-heap` in
  `lib/Experimental/scheme-dump-heap.sch` (7247d26e), native 64-bit
  only.  As designed (220bb044), sized by sro, with a walk that
  allocates nothing (gc-counter checked) and a table from address to
  index of fixnums.  What changed from the design: objects are read by
  address with peek-bytes, never held as Scheme values, so the queue
  holds fixnums and the walk stores no pointers at all; peek-bytes is
  called through `%syscall`, since `syscall` conses a rest list (the
  check caught it); code vectors of nested lambdas, which only constant
  vectors point to, are found in a pass of their own; and G_CALLOUTS
  (the cell of millicode-support) is a root, which arithmetic
  millicode needs.  Dumping takes about a second on x86-64 Linux.
  Tested: heaps dumped under the default collector and -rrof load and
  run under the default collector, -stopcopy and -rrof, and test/Lib
  gives the same output from a dumped heap as from larceny.heap.

## 2026-10-18

Bugs found by safehouse (another port of Larceny) and reproduced
here; see CLAUDES_CHOICES.md for the triage of its list.

- Stop-and-copy: a stack overflow with a frame larger than STACK_ROOM
  could loop forever (a 12KB frame hung -stopcopy) (b3c7e9a3).
- Fence: fl=, fl<, fl<=, fl>, fl>= and fl-- called themselves forever;
  they are now primitives (4dedad64).  Tested in test/Lib/number.sch.
- References to undefined names on error paths: bitwise-not of a
  non-integer returned a symbol, the R6RS record mutator's range error
  and the reader's undefined SRFI 38 label reported undefined globals,
  and several latent ones (b9eca68c).  Tested in test/Lib/except.sch.
- Petit: implicit continuation ids collided with Twobit's labels in a
  segment with over a thousand lambdas, so gcc rejected the C
  (9989faac).
- The regional collector, from safehouse's hunt for a rare -rrof
  crash (ported without reproducing them): a generation read before a
  forward that adds a region (d71f4b3d), extbmp's tree walk in 32-bit
  arithmetic (17822b42), smircy's expanded mark bitmap too small
  (dbac36ef), extbmp membership truncated to an int-sized bool
  (e65fecc7), the startup SSBs all tagged as the nursery's (f15afda8),
  and -rhashrep skipping the static area's minor remembered set
  (7dd8f116).

## 2026-10-10

- Native Larceny for x86-64 Linux (branch x86-64-native): an x86-64 back
  end for the Fence assembler (e0e489d0, daa7117d), the runtime glue and
  build configuration (672fdfb1), microsecond jiffies (2df69c27).
  Bootstraps with Racket in about 4 minutes; test/Lib passes with the
  default collector, -stopcopy, and -rrof; the compiler tests, the
  R7RS suite and the R6RS suite (--r7rs and --r6rs modes) pass with all
  three collectors; test/FFI's ABI and std-ffi tests pass.  Decisions
  made along the way are in CLAUDES_DECISIONS.md.
- Twobit: a conditional in tail position whose arms shared the current
  frame could give the then arm's own conditionals frames of their own,
  which nested inside the shared frame when the else arm used it, so a
  tail call left the outer frame on the stack (18f057b8).  Showed up with
  8 registers (native x86-64), in get-line on UTF-8 ports.  Petit passes
  test/Lib and the compiler tests with it (on x86-64-linux as 99ab870e).

- Petit Larceny builds on x86-64 Linux (Ubuntu, gcc 15.2, Racket 8.18
  as the host): a linux-x86-64 configuration (0afb5ef5); 64-bit code is
  compiled as gnu17, since in C23 bool is a keyword.  test/Lib passes
  with the default collector, -stopcopy, and -rrof; the compiler tests
  (sanity-switches), the R7RS suite (also with -rrof), and the R6RS
  suite (--r7rs and --r6rs) pass.
- FFI callouts on x86-64 Linux (3a44cf01), made from C like those on
  AArch64; it had used the 32-bit i386 ABI, so (scheme time)'s
  current-jiffy failed.  std-ffi narrows char and short results, whose
  upper bits the System V ABI leaves undefined (abfdff3f).

## 2026-10-08

- The debugger's backtrace and procedure-source-file said that code
  compiled at the REPL came from the heap build script: the heap is
  dumped during the script's load, which left it the current source
  file, along with its last datum's source positions.  A load or
  compile-file that stopped on an error also left its positions to
  later REPL code.  Both are cleared now (f49bb530).
- memstats' words allocated counts the young heap's allocation since its
  last collection (c3a5d108); it had counted it only at collections, so
  `time` reported "Words allocated: 0" for 100000 conses.  Now within
  0.25% even with a 32KB nursery (a collection also counts the stack
  frames it flushes).

## 2026-10-07

- test/Lib's "Error case #24" (bug 105) failed under small nurseries
  (`-size0 64K`, with or without `-rrof`): memstats counts allocation
  only at collections, so a collection during one of its two
  measurements made them differ by a nursery.  Each measurement now
  starts after a collection (185ff58f).
- Card marking for large objects (8c14a60a generational, 67d0187b
  regional): a remembered large object (64KB or more) has a bitmap of
  512-byte cards, marked from the slot offsets that the write barrier
  logs, and minor collections scan only the marked cards.  A young
  pointer stored into an old 50-million-word vector cost 12 ms per minor
  collection; now nothing measurable.  Test: test/GC/card-marking.sch.
- The regional collector's major collections and summarization scan
  only the cards that may point into other regions, a second bitmap
  (30d6aa9b): summarization in the test 107 ms -> 23 ms, its longest
  step 13 -> 4 ms.  `CARD_CHECK_ACROSS` (card.h) checks the bits at
  each scan.
- Regional collector: the two-region shortcut (no remembered set work
  when the promoted data fits in the target region) did not leave room
  for the chunk that expanding the region takes, so find_space_rgnl
  asserted (memmgr.c:3507) under `-rrof -size0 64K` (de889429).
- continuation-mark-set? was #f for every mark set, and printing one
  failed in the record printer (5138ae17).
- `bootstrap.sh --r7rs` ends by running an R6RS program in `--r6rs`
  mode, which reads libraries with strict R6RS syntax (0d26cb8b).
- `(require 'srfi-19)` works in R5RS mode (a107d295).
- In R5RS mode, with-exception-handler and guard catch errors that
  primitives signal, while handlers are installed (77437d03).
- srfi-133-test reports its failures; its vector-cumulate test uses the
  final argument order (4245785a).
- Investigated, not done: restoring several frames per stack underflow
  (an adaptive count, grown on each underflow and halved at each
  collection, since frames restored but not returned into before the
  next flush are the waste).  Returning through a flushed frame costs
  two mispredicted branches (the return into the underflow frame, and
  the handler's branch to the restored frame), about 5.7 ns per frame
  at depth (Racket about 1, as Chez does not move frames); batching
  would save only the second.  Locality is not a factor: the cost is
  the same for frames interleaved with live data after a collection, or
  out of cache at depth 10^6, because the collector copies the chain in
  return order.  Counted on the R6RS benchmarks (an instrumented build):
  at most about 2 million underflows a second (listsort; 0.2 to 19000
  per collection across the suite), and fence_stack_underflow is 2.3%
  of listsort's samples (about 8 ns an underflow) and less elsewhere,
  so the upper bound is 2-3% on listsort and under 1% on the others.
- call/cc, continued: a continuation called with one value makes no
  rest list (0e0bcb2c), and creg-set! reinstates it in assembly
  (7d77d125): a throw 28 ns -> 19.5; fibc 1.1x Racket's time, ctak 1.1x.
- call/cc (fibc 3.1x Racket's time -> 1.4x, ctak 2.5x -> 1.5x):
  zero?, integer?, exact?, inexact? are in line for fixnums (cd5ef179;
  zero? was 18% of fibc), and a throw no longer goes through the
  dynamic-wind wrapper's rest list and reroot! (two calls into C) when
  nothing needs rerooting, nor through (apply values args) for one value
  (ac48c2a5): a throw 48 ns -> 28.
- A runtime that fails to compile stops the build (36bf501c): build-runtime
  ignored make's status, so the old larceny.bin was installed and
  bootstrap.sh said "Done".
- The collector clears the registers that the running code does not
  use (47c65bee): a dead value left in a register was a root until
  something overwrote it (an 80MB vector in REG13 survived 68
  collections).  The native assembler records a mask of the registers
  each code vector uses or receives arguments in; `-keepregs` keeps all
  registers.  Test: test/GC/dead-registers.sch.
- Bulk copies and fills (TODO: "vector-copy ... move an element per
  iteration").  Vectors: two syscalls copy (memmove) and fill vector-like
  objects, doing the barriers' work per slot in C, the SATB logging
  included (f97a0cf2); vector-copy, vector-append, vector-copy! 3-4.5x
  faster, vector-fill! 11x.  A third fills a vector from a list for
  list->vector (fcea8bce).  Users: vector-sort's merges (c9a437e9),
  bignum digit copies (5d4bd175), text chunk vectors (a9222e76), SRFI
  43 and 133 (e783d7de).  bytevector-fill! (the procedure) uses the
  memset primitive and string-fill! doubles with memmove (322482ed).
  Tests: test/Lib/vector.sch, test/GC/vector-move.sch.
- bytevector-u8-set! and bytevector-set! accept octets only
  (e9436201): the compiler macros accepted -128 through 255, and
  native stored -1 as 255.  bytevector-s8-set! checks -128..127.
- Compiling record.sch no longer prints eight `Assertion is false`
  warnings (10864607): the bummed record accessor macros complain
  through record:wrong-type instead of a false .check!.
- (rnrs control) read in `--r6rs` mode (a0ed4720): case-lambda's
  expansion used an identifier beginning with a dot.
- The reader after `#!r6rs` rejects `@`, `@b` and `#\null` (036e7940); an
  unterminated `#^B"..."` codevector in a fasl file is a lexical error,
  not a loop.  The byte-order mark and Unicode whitespace parts of the
  TODO item had already been fixed.
- `timezone-offset` gives the daylight saving offset during DST
  (e2f32d11).
- `bootstrap.sh --r7rs` fails, with the error, when compiling the
  libraries fails (ca97dab0).
- A heap build that fails leaves `larceny.heap` as it was (12921cfa):
  the heap scripts used it as a temporary file and dumped into it, so a
  failure left 29 bytes of date.  Each successful build keeps the
  binary that dumped its heap (ad1f17ef): `larceny.bin.cur` with
  `larceny.heap`, and `larceny.bin.prev` with `larceny.heap.prev`.
- A nursery larger than 32MB (`-size0 256M`) overflowed the low-level
  allocator's initial page table, whose first allocation did not grow
  it; garbage generations then crashed the write barrier's remembered
  set processing (fe80ba24).  An assertion now checks that the table
  covers each allocation.  Test: `test/GC/large-nursery.sch`.

## 2026-10-06

- Procedures with optional arguments use case-lambda (bc72898d):
  display, write, newline, the port procedures of stdio.sch and
  portio.sch, member, assoc, make-list, the string and vector
  conversions and fills, string->utf8, utf8->string, number->string,
  string->number, atan, log, vector-sort(!).  A call with optional
  arguments conses nothing; write-u8 to a port 13 ns -> 7.  args>=
  stores an empty rest list in line (7fb0f4ca): a rest-list procedure
  called with only its required arguments 5.1 ns -> 1.7.
- arity-dispatch left a false pointer in SECOND, which crashed the
  collector when a clause allocated with a full nursery (5e7748d5).
- write-u8 without a port wrote to the current input port (f05c1f12).
- iosys.sch had non-Ascii string literals, which a Racket-hosted build
  compiles wrongly (3d0cf6c3).
- log with a non-numeric second argument referred to unbound
  variables (in bc72898d).
- case-lambda dispatches on the argument count without consing
  (2a28596a): a new MAL instruction, arity-dispatch (Fence and
  Petit), is the code of every case-lambda procedure, whose slots hold
  the clauses and their arities.  A call takes 1.6-2.3 ns instead of
  8-27 ns (40 with a rest clause, now 7.5); a fixed-arity procedure
  1.9.  The copy procedures with optional arguments use it (788d303c):
  string-copy! of 2 characters 12.0 ns -> 6.6.  Procedures are named
  after their definitions again (3af005c4).
- R7RS bytevector-copy! ignored an explicit end (7024985d).
- Normalization: decompose writes into a string, from a cache of
  flattened decompositions, and sorts only runs of combining marks
  that are out of order (d20ae85b).  NFD of short strings 1.2-2x as
  fast, of long ones 4.5x; the normalization benchmark 3.07 s -> 2.55
  (50 iterations).
- String ports read and write their string directly (2b50abc1): the
  port's buffer is the string, with its own port types (11 and 13)
  and in-line paths, instead of a UTF-8 byte buffer encoded into or
  decoded from.  Per 10 million characters: read-char 32 ms -> 20
  (Ascii), 178 -> 21 (non-Ascii); write-char 59 -> 30, 135 -> 32;
  write-string 104 -> 18; get-string-all 90 -> 12.  read0 4.23 s ->
  3.72; reader-bound benchmarks on file ports 1-2% slower (read1,
  slatex).  Tests in `test/Lib/io.sch`.
- read-line at the end of input after a #\return raised an error
  (be8ba400); `test/Lib/normalization.sch` no longer replaces the
  system's read-line with its own.

## 2026-10-04

- UTF-8 decoding in get-char (c1b835cd): a decoding error consumes
  only the maximal subpart of the invalid sequence, not the byte that
  shows it is invalid (#xce #x62 lost the b); a surrogate (#xed #xa0
  #x80) is a decoding error, not a failure of integer->char;
  lookahead-char in replace mode no longer consumes the invalid bytes;
  a decoding error in auxbuf no longer makes the next read fail the
  assertion (fx= 0 auxptr); and a character left incomplete by end of
  file is a decoding error instead of vanishing.  Tests in
  `test/Lib/io.sch`.
- bytevector-fill! (and make-bytevector with a fill) checks that the
  fill is a fixnum from -128 through 255, in line on AArch64 and in
  Petit (711322da): 256 filled with 0 and a symbol with garbage.
  Tests in `test/Lib/bytevector.sch` (and on Petit: test/Lib, R7RS
  and R6RS suites).
- The `.check! #f 71` warnings when loading `test/Lib/io.sch` came
  from put-char's fast path, whose bytevector-set! checks fail for a
  constant character of 128 or more in dead code; it stores with the
  trusted primitive now (be71e240).

## 2026-09-30

- get-line reads a line in the buffer at once when it is UTF-8, not
  only when it is Ascii (NormalizationTest.txt has UTF-8 on almost
  every line): reading that file 80 times takes 0.84 s instead of 2.08
  (Racket 1.08), and the normalization benchmark 4.95 s instead of
  6.03.  get-line's fast path now also counts the line for
  port-lines-read and port-line-start (before, Ascii lines read by
  get-line were not counted, so later line numbers, such as those in
  the reader's error messages, were too small).  Tests in
  `test/Lib/io.sch` (cac0438b).
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
