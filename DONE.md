# DONE

Finished items, moved here from TODO.md.  Newest first.

## 2026-10-07

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
