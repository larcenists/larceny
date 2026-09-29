# DONE

Finished items, moved here from TODO.md.  Newest first.

## 2026-09-29

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
