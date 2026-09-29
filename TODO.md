# TODO

Things we have thought of doing.  When an item is finished, move it to
DONE.md (with the date and the commit, if any).

## Native AArch64 Larceny

- Under `-rrof`, the code-scanning pass is skipped while concurrent
  marking is in progress (the mark stack may hold code pointers); it could
  instead update the mark state.  It also copies code referenced only
  from garbage.
- Lighter-weight millicode calls: every call saves and restores the whole
  register context (allocation is now in line in the common case, but
  arithmetic on non-fixnums, generic comparisons, etc. still call out).
- More peephole optimization for the AArch64 back end (the ARM-32 back
  end has a framework in `src/Asm/Fence/arm-optimizer.sch`), e.g. the
  stack overflow check and timer tick sequences.
- GC byte counts are C `int`s (`semispace_t.allocated`/`used`, the GC
  statistics, ...), so a heap over 2 GB overflows them and the collector
  panics (seen compiling a 20000-statement procedure, below).  Widen them.
- Twobit's memory use explodes on enormous single procedures (a
  procedure of 8000 statements needs over 2 GB); the AArch64 long-branch
  mode for code vectors over 1 MB could only be tested with its
  threshold lowered.
- FFI and the `-np` collector (neither exists for Petit either).  Part
  of the FFI: `foreign-ctools` compiles its C helpers with `-arch armv4t`
  on ARM, so `src/Build/compile-standard-libraries.sch` (used by the
  nightly builds) fails at the libraries that use it, such as
  `lib/Standard/file-system.sch`.

## Build process footguns

- `setup`'s `'clean` needs git to find the build products; it could fall
  back to a list of generated file types in a tree without git.
- Out-of-tree builds (a directory per configuration).

## Repository

- Push the 64-bit work on `master` to `origin`.
